package lambdanet.architecture

import lambdanet._
import botkop.numsca
import botkop.numsca.:>
import cats.data.Chain
import funcdiff._
import lambdanet.NeuralInference.{
  AccessFieldUsage,
  ClassFieldUsage,
  LabelUsages,
  LabelVector,
  Message,
  MessageKind,
  MessageModel
}
import lambdanet.translation.PredicateGraph.{PNode, ProjNode}

import scala.collection.GenSeq

abstract class NNArchitecture(
    val arcName: String,
    dimMessage: Int,
    pc: ParamCollection,
) extends ArchitectureHelper {

  /** Store the dropout masks so that they can be reused across a
    * single forward propagation (but should be cleared between iterations) */
  var dropoutStorage: Option[ParamCollection] = None

  case class Embedding(
      vars: Map[ProjNode, CompNode],
  )

  type UpdateMessages = Map[ProjNode, Chain[Message]]
  val emptyMessages: UpdateMessages = Map()

  def toVars(msg: Map[PNode, Chain[Message]]): UpdateMessages = {
    msg.collect {
      case (k, v) if k.fromProject => ProjNode(k) -> v
    }
  }

  val layerFactory: LayerFactory = LayerFactory(arcName, pc)
  import layerFactory._

  def initialEmbedding(projectNodes: Set[ProjNode]): Embedding

  def calculateMessages(
      messages: GenSeq[(MessageKind, Vector[MessageModel])],
      encodeNode: PNode => CompNode,
      encodeLabel: Symbol => CompNode,
      encodeName: Symbol => CompNode,
      labelUsages: LabelUsages,
      isLibLabel: Symbol => Boolean,
  ): UpdateMessages = {
    import MessageKind._
    import MessageModel._
    import cats.implicits._

    def bidirectional(
        name: String,
        n1s: Vector[PNode],
        n2s: Vector[PNode],
        inputs: Vector[CompNode]
    ): UpdateMessages = {
      toVars(
        verticalBatching(n1s.zip(inputs), singleLayer(name / 'left, _)) |+|
          verticalBatching(n2s.zip(inputs), singleLayer(name / 'right, _))
      )
    }

    def batchedAttention(
        inputKeys: Vector[PNode],
        keys: Vector[PNode],
        values: Vector[PNode],
        nodes: Vector[PNode],
        label: Symbol,
        name: SymbolPath
    ): UpdateMessages = {
      require(inputKeys.nonEmpty)
      val (exKey, exValue) = {
        val n = if (isLibLabel(label)) label else '?
        randomVar(name / 'experienceKey / n) ->
          randomVar(name / 'experienceValue / n)
      }
      val (inKeys1, nodes1) =
        inputKeys.zip(nodes).filter(_._2.fromProject).unzip
      if (inKeys1.isEmpty) return emptyMessages

      val keys1 = keys.map(encodeNode) :+ exKey
      val values1 = values.map(encodeNode) :+ exValue
      val weightedSum =
        concatN(axis = 0, fromRows = true)(inKeys1.map(encodeNode))
          .dot(concatN(axis = 0, fromRows = true)(keys1).t)
          .pipe(softmax) //todo: sharpen mechanism?
          .dot(concatN(axis = 0, fromRows = true)(values1))
      val messages =
        concatN(axis = 0, fromRows = true)(nodes1.map(encodeNode))
          .concat(weightedSum, axis = 1)
          .pipe(singleLayer(name, _))
      nodes1.zipWithIndex.map {
        case (n, i) =>
          Map(ProjNode(n) -> Chain(messages.slice(i, :>))) //todo: use rows operator
      }.combineAll
    }

    def extractMessages(
        kind: MessageKind,
        models: Vector[MessageModel]
    ): UpdateMessages = {
      kind match {
        case KindSingle(name) =>
          val paired = models
            .asInstanceOf[Vector[Single]]
            .map(s => s.n -> encodeNode(s.n))
          verticalBatching(paired, singleLayer(name, _))
            .pipe(toVars)
        case KindBinary(name) =>
          val (n1s, n2s, inputs) = models
            .asInstanceOf[Vector[Binary]]
            .map { s =>
              val merged =
                encodeNode(s.n1).concat(encodeNode(s.n2), axis = 1)
              (s.n1, s.n2, merged)
            }
            .unzip3
          bidirectional(name, n1s, n2s, inputs)
        case KindNaming(name) =>
          val paired = models
            .asInstanceOf[Vector[Naming]]
            .map(s => s.n -> encodeName(s.name))
          verticalBatching(paired, singleLayer(name, _))
            .pipe(toVars)
        case KindBinaryLabeled(name, LabelType.Position) =>
          //todo: try RNN based embedding
          val (n1s, n2s, inputs) = models
            .asInstanceOf[Vector[Labeled]]
            .map {
              case Labeled(n1, n2, label) =>
                val pos = label.asInstanceOf[Label.Position].get
                val lEmbed = encodePosition(pos)
                val input = concatN(axis = 1, fromRows = true)(
                  Vector(encodeNode(n1), encodeNode(n2), lEmbed)
                )
                (n1, n2, input)
            }
            .unzip3
          bidirectional(name, n1s, n2s, inputs)
        case KindBinaryLabeled(name, LabelType.Field) =>
          val (receivers, inputs) = models
            .asInstanceOf[Vector[Labeled]]
            .map {
              case Labeled(n1, n2, label) =>
                val f = label.asInstanceOf[Label.Field].get
                val input = concatN(axis = 1, fromRows = true)(
                  Vector(
                    encodeNode(n1),
                    encodeNode(n2),
                    encodeLabel(f),
                    encodeName(f)
                  )
                )
                ((n1, n2), input)
            }
            .unzip
          val (n1s, n2s) = receivers.unzip
          bidirectional(name, n1s, n2s, inputs)
        case KindAccess(label) =>
          val (cStack, fStack) = labelUsages.classesInvolvingLabel
            .getOrElse(label, Vector())
            .map {
              case ClassFieldUsage(c, field) => (c, field)
            }
            .unzip
          val (receiverStack, resultStack) = models
            .asInstanceOf[Vector[AccessFieldUsage]]
            .map {
              case AccessFieldUsage(receiver, result) => (receiver, result)
            }
            .unzip

          batchedAttention(
            receiverStack,
            cStack,
            fStack,
            resultStack,
            label,
            'AccessAttention / 'toResult
          ) |+| batchedAttention(
            resultStack,
            fStack,
            cStack,
            receiverStack,
            label,
            'AccessAttention / 'toReceiver
          )
        case KindField(label) =>
          val (receiverStack, resultStack) = labelUsages.accessesInvolvingLabel
            .getOrElse(label, Vector())
            .map {
              case AccessFieldUsage(receiver, result) => (receiver, result)
            }
            .unzip

          val (cStack, fStack) = models
            .asInstanceOf[Vector[ClassFieldUsage]]
            .map {
              case ClassFieldUsage(c, field) => (c, field)
            }
            .unzip

          batchedAttention(
            cStack,
            receiverStack,
            resultStack,
            fStack,
            label,
            'FieldAttention / 'toField
          ) |+| batchedAttention(
            fStack,
            resultStack,
            receiverStack,
            cStack,
            label,
            'FieldAttention / 'toClass
          )
      }
    }

    messages
      .map { case (a, b) => extractMessages(a, b) }
      .fold(Map())(_ |+| _)
  }

  def similarity(
      inputs: Vector[CompNode],
      candidates: Vector[CompNode],
      name: SymbolPath,
  ): CompNode = {
    val inputs1 =
      concatN(axis = 0, fromRows = true)(inputs)
        .pipe(linear(name / 'similarityInputs, dimMessage))
    val candidates1 =
      concatN(axis = 0, fromRows = true)(candidates)
        .pipe(linear(name / 'similarityCandidates, dimMessage))

//    val sim = cosineSimilarity(inputs1, candidates1)
//    val sharpen =
//      (sim ~> linear('sharpening / 'L1, dimMessage) ~> relu
//        ~> linear('))
//      .pipe(sum(_, axis = 1))
//      .pipe(softPlus(_) + 1.0)
    val factor = 1.0 / math.sqrt(dimMessage)
    val sim = inputs1.dot(candidates1.t) * factor

    sim ~> softmax
  }

  def predictProjectTypes(
      inputs: Vector[CompNode],
      numLibType: Int
  ): CompNode = {
    concatN(axis = 0, fromRows = true)(inputs) ~>
      linear('libDistr / 'L1, dimMessage) ~> relu ~>
      linear('libDistr / 'L2, dimMessage) ~> relu ~>
      linear('libDistr / 'L3, numLibType)
  }

  def separatedSimilarity(
      inputs: Vector[CompNode],
      libCandidates: Vector[CompNode],
      projCandidates: Vector[CompNode],
  ): CompNode = {
    val inputs1 =
      concatN(axis = 0, fromRows = true)(inputs)
//        .pipe(singleLayer('similarityInputs, _))
//    val candidates1 =
//      concatN(axis = 0, fromRows = true)(projCandidates)
//        .pipe(singleLayer('similarityCandidates, _))

    val pIsLib = inputs1 ~>
      linear('libDecider / 'L1, dimMessage) ~> relu ~>
      linear('libDecider / 'L2, dimMessage) ~> relu ~>
      linear('libDecider / 'L3, 1) ~> sigmoid

    val libTypeNum = libCandidates.length
    val libDistr = inputs1 ~>
      linear('libDistr / 'L1, dimMessage) ~> relu ~>
      linear('libDistr / 'L2, dimMessage) ~> relu ~>
      linear('libDistr / 'L3, libTypeNum) ~> softmax
//    val libDistr = similarity(inputs, libCandidates, 'libDistr)
    val projDistr = similarity(inputs, projCandidates, 'projDistr)

    (libDistr * pIsLib).concat(projDistr * (-pIsLib + 1), 1)
  }

  def encodeFunction(args: Vector[CompNode], to: CompNode): CompNode = {
    (to +: args).zipWithIndex
      .map {
        case (a, i) =>
          a.concat(encodePosition(i - 1), axis = 1)
      }
      .pipe(concatN(axis = 0, fromRows = true))
      .pipe(singleLayer('encodeFunction, _))
      .pipe(sum(_, axis = 0))
  }

  def encodeObject(elements: Vector[(LabelVector, CompNode)]): CompNode = {
    if (elements.isEmpty) {
      randomVar('emptyObject)
    } else {
      elements
        .map {
          case (v1, v2) => v1.concat(v2, axis = 1)
        }
        .pipe(concatN(axis = 0, fromRows = true))
        .pipe(singleLayer('encodeObject, _))
        .pipe(sum(_, axis = 0))
    }
  }

  def encodeLibTerm(
      experience: CompNode,
      signature: CompNode,
      name: CompNode,
  ): CompNode = {
    singleLayer(
      'encodeLibTerm,
      concatN(axis = 1, fromRows = true)(Vector(experience, signature, name)),
    )
//     todo: see if type signature helps
//    experience
  }

//  def encodeLibType(experience: CompNode, name: CompNode): CompNode = {
//    singleLayer('encodeLibType, experience.concat(name, axis = 1))
//  }

  def mergeMessages[K](
      name: SymbolPath,
      messages: GenSeq[(K, Chain[Message])],
      embedding: K => CompNode,
  ): Map[K, Message] = {
    messages
      .map {
        case (n, ms) =>
//          val init = randomVar(name / 'mergeMsgs / 'init)
//          n -> ms.foldLeft(init){ (acc, msg) =>
//            gru(name / 'mergeMsgs / 'gru)(acc, msg)
//          }

//          val n1 = embedding(n)
//          val values = concatN(axis = 0, fromRows = true)(ms.toVector)
//          val keys = linear(name / 'mergeMsgs / 'transKey, dimMessage)(values)
//
//          val attention = softmax(keys.dot(n1.t).t / dimMessage)
//          n -> attention.dot(values)

          n -> plusN(ms.toVector)
      }
      .seq
      .toMap
  }

  def update[K](
      name: SymbolPath,
      embedding: Map[K, CompNode],
      messages: Map[K, CompNode],
  ): Map[K, CompNode]

  /** stack inputs vertically (axis=0) for batching */
  def verticalBatching[K](
      inputs: Vector[(K, CompNode)],
      transformation: CompNode => CompNode,
  ): Map[K, Chain[CompNode]] = {
    import cats.implicits._
    import numsca.:>

    val (nodes, vectors) = inputs.unzip

    val output = transformation(concatN(axis = 0, fromRows = true)(vectors))
    nodes.zipWithIndex.map {
      case (n, i) =>
        Map(n -> Chain(output.slice(i, :>)))
    }.combineAll
  }

  /** stack inputs vertically (axis=0) for batching */
  def verticalBatching2[K](
      inputs: Vector[(K, (CompNode, CompNode))],
      transformation: (CompNode, CompNode) => CompNode,
  ): Map[K, Chain[CompNode]] = {
    import cats.implicits._
    import numsca.:>

    val (nodes, vectors) = inputs.unzip
    val (l, r) = vectors.unzip

    val l1 = concatN(axis = 0, fromRows = true)(l)
    val r1 = concatN(axis = 0, fromRows = true)(r)
    val output = transformation(l1, r1)
    nodes.zipWithIndex.map {
      case (n, i) =>
        Map(n -> Chain(output.slice(i, :>)))
    }.combineAll
  }

  val singleLayerModel = "2 FCs"
  def singleLayer(
      path: SymbolPath,
      input: CompNode,
      useDropout: Boolean = false,
  ): CompNode = {
    def oneLayer(name: Symbol)(input: CompNode) = {
      val p = path / name
      val r = linear(p, dimMessage)(input) ~> relu
//      dropoutStorage match {
//        case None => r
//        case Some(maskPc) =>
//          import botkop.{numsca => ns}
//          val keepProb = 0.5
//          val mask = maskPc.getConst(p) {
//            (ns.rand(ns.Shape.make(1,r.shape.sizes(1))) < keepProb).boolToFloating / keepProb
//          }
//          r * mask
//      }
      r
    }

    if (useDropout)
      input ~> oneLayer('L1) ~> dropout(0.75) ~> oneLayer('L2) ~> dropout(0.5)
    else input ~> oneLayer('L1) ~> oneLayer('L2)
  }

  def encodePosition(pos: Int): CompNode = {
    assert(pos >= -1)
    if (pos == -1) {
      randomVar('position / 'head)
    } else {
      randomVar('position / Symbol(pos.toString))
    }
  }

//  val encodePosition = {
//    def encodePosition(pos: Int): CompNode = {
//      assert(pos >= -1)
//      if (pos == -1) {
//        getVar('position / 'head) { randomVec() }
//      } else {
//        getConst('position / Symbol(pos.toString)) {
//          val phases = (0 until dimMessage / 2).map { dim =>
//            pos / math.pow(1000, 2.0 * dim / dimMessage)
//          }
//          numsca
//            .Tensor(phases.map(math.sin) ++ phases.map(math.cos): _*)
//            .reshape(1, -1)
//        }
//      }
//    }
//    val maxFunctionArity = 100
//    val map = (-1 to maxFunctionArity).map(i => i -> encodePosition(i)).toMap
//
//    pos: Int => map.getOrElse(pos, encodePosition(pos))
//  }

}
