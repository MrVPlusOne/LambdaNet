package lambdanet.architecture

import lambdanet._
import botkop.numsca
import botkop.numsca.{:>, Shape, Tensor}
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
import lambdanet.train.{DecodingResult, Joint, TwoStage}
import lambdanet.translation.PredicateGraph.{PNode, PType, ProjNode}

import scala.collection.GenSeq

abstract class NNArchitecture(
    val arcName: String,
    dimMessage: Int,
    pc: ParamCollection
) extends ArchitectureHelper {

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
      iteration: Int,
      messages: GenSeq[(MessageKind, Vector[MessageModel])],
      encodeNode: PNode => CompNode,
      encodeLabel: Symbol => CompNode,
      encodeName: Symbol => CompNode,
      labelUsages: LabelUsages,
      isLibLabel: Symbol => Boolean
  ): UpdateMessages = {
    import MessageKind._
    import MessageModel._
    import cats.implicits._

    val iterSymbol = Symbol(s"iter-$iteration")

    def bidirectional(
        name0: SymbolPath,
        n1s: Vector[PNode],
        n2s: Vector[PNode],
        inputs: Vector[CompNode]
    ): UpdateMessages = {
      val name = name0 / iterSymbol
      toVars(
        verticalBatching(n1s.zip(inputs), messageLayer(name / 'left)) |+|
          verticalBatching(n2s.zip(inputs), messageLayer(name / 'right))
      )
    }

    def batchedAttention(
        inputKeys: Vector[PNode],
        keys: Vector[PNode],
        values: Vector[PNode],
        nodes: Vector[PNode],
        label: Symbol,
        name0: SymbolPath
    ): UpdateMessages = {
      require(inputKeys.nonEmpty)

      val name = name0 / iterSymbol
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
        stackRows(inKeys1.map(encodeNode))
          .dot(stackRows(keys1).t)
          .pipe(softmax)
          .dot(stackRows(values1))
      val messages =
        stackRows(nodes1.map(encodeNode))
          .concat(weightedSum, axis = 1)
          .pipe(messageLayer(name / 'messages))
      nodes1
        .zip(messages.rows)
        .map {
          case (n, r) => Map(ProjNode(n) -> Chain(r))
        }
        .combineAll
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
          verticalBatching(paired, messageLayer(name / iterSymbol))
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
          verticalBatching(paired, messageLayer(name / iterSymbol))
            .pipe(toVars)
        case KindBinaryLabeled(name, LabelType.Position) =>
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

  def attendPredictionSpace(
      embed: Embedding,
      candidates: Vector[CompNode],
      name: SymbolPath
  ): Embedding = {

    val (keys0, values0) = stackRows(candidates)
      .pipe { batch =>
        linearLayer(name / 'keys, batch) -> linearLayer(name / 'values, batch)
      }
    val (defaultK, defaultV) = randomVar(name / 'defaultK) -> randomVar(
      name / 'defaultV
    )
    val keys = keys0.concat(defaultK, axis = 0)
    val values = values0.concat(defaultV, axis = 0)
    val (nodes, nVecs) = embed.vars.toVector.unzip
    val nodeMatrix = stackRows(nVecs)
    val weightedSum = nodeMatrix
      .dot(keys.t)
      .pipe(softmax)
      .dot(values)
    val newMatrix = nodeMatrix + weightedSum
    Embedding(nodes.zip(newMatrix.rows).toMap)
  }

  def attendPredictionSpaceByName(
      nodes: Vector[ProjNode],
      nVecs: Vector[CompNode],
      candidates: Vector[(PType, CompNode)],
      similarityScoresOpt: Option[Tensor],
      name: SymbolPath
  ): Embedding = {
    val defaultV = randomVar(name / 'defaultV)

    val weightedSum = similarityScoresOpt match {
      case Some(similarityScores) =>
        val sharpness = getVar(name / 'sharpness)(Tensor(1.0).reshape(1, 1))
        val attention = similarityScores
          .pipe(x => const(x) * sharpness)
          .concat(numsca.ones(nodes.length, 1), axis = 1) // guardian
          .pipe(softmax)

        val values0 = stackRows(candidates.map(_._2))
        //      .pipe(linearLayer(name / 'values, _))

        val values = values0.concat(defaultV, axis = 0)
        attention.dot(values)
      case None =>
        defaultV
    }

    val nodeMatrix = stackRows(nVecs)
    val newMatrix = nodeMatrix + weightedSum
    Embedding(nodes.zip(newMatrix.rows).toMap)
  }

  // new similarity
  def similarity(
      inputs: Vector[CompNode],
      candidates: Vector[CompNode],
      useDropout: Boolean,
      name: SymbolPath
  ): Joint = {
    val rows = for {
      input <- inputs
      cand <- candidates
    } yield {
      input -> cand
    }

    val logits0 = concatTupledRows(rows) ~>
      linear(name / 'sim0, dimMessage) ~> relu ~>
//      (if (useDropout) dropout(0.5) else identity) ~>
      linear(name / 'sim1, dimMessage / 2) ~> relu ~>
      linear(name / 'sim2, dimMessage / 4) ~> relu ~>
      linear(name / 'sim3, 1)

    val logits = logits0.reshape(Shape.make(inputs.length, candidates.length))
    Joint(logits)
  }

//  // simple similarity
//  def similarity(
//      inputs: Vector[CompNode],
//      candidates: Vector[CompNode],
//      useDropout: Boolean,
//      name: SymbolPath
//  ): Joint = {
//    val inputs1 =
//      stackRows(inputs)
//        .pipe(linear(name / 'similarityInputs, dimMessage))
//    val candidates1 =
//      stackRows(candidates)
//        .pipe(linear(name / 'similarityCandidates, dimMessage))
//
//    Joint(inputs1.dot(candidates1.t))
//  }

  def encodeLibType(n: PNode, encodeName: Symbol => CompNode): CompNode = {
    def encodeNameOpt(nameOpt: Option[Symbol]): CompNode = {
      nameOpt.map(encodeName).getOrElse(randomVar('libTypeNameMissing))
    }

    assert(n.fromLib)
    val ex = randomVar('libType / n.symbol)
    val name = encodeNameOpt(n.nameOpt)
    linearLayer('encodeLibType / 'mix, ex.concat(name, axis = 1))
  }

  @deprecated
  def predictLibraryTypes(
      inputs: Vector[CompNode],
      numLibType: Int,
      dropoutP: Option[Double]
  ): CompNode = {
    def drop(n: CompNode) = dropoutP match {
      case Some(p) => dropout(p)(n)
      case _       => n
    }
    stackRows(inputs) ~>
      linear('libDistr / 'L1, dimMessage) ~> relu ~> drop ~>
      linear('libDistr / 'L2, dimMessage) ~> relu ~> drop ~>
      linear('libDistr / 'L3, numLibType)
  }

  def twoStageSimilarity(
      inputs: Vector[CompNode],
      libCandidates: Vector[CompNode],
      projCandidates: Vector[CompNode],
      isLibOracle: Option[Vector[Boolean]],
      useDropout: Boolean
  ): DecodingResult = {
    val inputs1 =
      stackRows(inputs)

    val pIsLib = isLibOracle match {
      case None =>
        inputs1 ~>
          linear('libDecider / 'L1, dimMessage) ~> relu ~>
          linear('libDecider / 'L2, dimMessage / 2) ~> relu ~>
          (if (useDropout) dropout(0.5) else identity) ~>
          linear('libDecider / 'L3, 1)
      case Some(truth) =>
        truth
          .map(x => if (x) 1000.0 else -1000.0)
          .pipe(x => const(Tensor(x.toArray).reshape(-1, 1)))
    }

    val libLogits = similarity(
      inputs,
      libCandidates,
      useDropout,
      'libDistr
    ).logits
    val projLogits =
      if (projCandidates.nonEmpty)
        similarity(inputs, projCandidates, useDropout, 'projDistr).logits
          .pipe(Some.apply)
      else None

    TwoStage(pIsLib, libLogits, projLogits)
  }

  def encodeFunction(args: Vector[CompNode], to: CompNode): CompNode = {
    (to +: args).zipWithIndex
      .map {
        case (a, i) => a -> encodePosition(i - 1)
      }
      .pipe(concatTupledRows)
      .pipe(nonLinearLayer('encodeFunction))
      .pipe(sum(_, axis = 0))
  }

  def encodeObject(elements: Vector[(LabelVector, CompNode)]): CompNode = {
    if (elements.isEmpty) {
      randomVar('emptyObject)
    } else {
      elements
        .pipe(concatTupledRows)
        .pipe(nonLinearLayer('encodeObject))
        .pipe(sum(_, axis = 0))
    }
  }

  def encodeLibTerm(
      experience: CompNode,
      signature: CompNode,
      name: CompNode
  ): CompNode = {
    nonLinearLayer('encodeLibTerm)(
      concatN(axis = 1, fromRows = true)(
        Vector(experience, signature, name)
      )
    )
  }

  def mergeMessages[K](
      name: SymbolPath,
      messages: GenSeq[(K, Chain[Message])],
      embedding: K => CompNode
  ): Map[K, Message] = {
    messages
      .map {
        case (n, ms) =>
//          val init = randomVar(name / 'mergeMsgs / 'init)
//          n -> ms.foldLeft(init){ (acc, msg) =>
//            gru(name / 'mergeMsgs / 'gru)(acc, msg)
//          }

//          val n1 = embedding(n)
//          val values = stackRows(ms.toVector)
//          val keys = linear(name / 'mergeMsgs / 'transKey, dimMessage)(values)
//
//          val attention = softmax(keys.dot(n1.t).t / dimMessage)
//          n -> attention.dot(values)

          n -> meanN(ms.toVector)
      }
      .seq
      .toMap
  }

  def update[K](
      name: SymbolPath,
      embedding: Map[K, CompNode],
      messages: Map[K, CompNode]
  ): Map[K, CompNode]

  /** stack inputs vertically (axis=0) for batching */
  def verticalBatching[K](
      inputs: Vector[(K, CompNode)],
      transformation: CompNode => CompNode
  ): Map[K, Chain[CompNode]] = {
    import cats.implicits._
    import numsca.:>

    val (nodes, vectors) = inputs.unzip

    val output = transformation(stackRows(vectors))
    nodes.zipWithIndex.map {
      case (n, i) =>
        Map(n -> Chain(output.slice(i, :>)))
    }.combineAll
  }

  /** stack inputs vertically (axis=0) for batching */
  def verticalBatching2[K](
      inputs: Vector[(K, (CompNode, CompNode))],
      transformation: (CompNode, CompNode) => CompNode
  ): Map[K, Chain[CompNode]] = {
    import cats.implicits._
    import numsca.:>

    val (nodes, vectors) = inputs.unzip
    val (l, r) = vectors.unzip

    val l1 = stackRows(l)
    val r1 = stackRows(r)
    val output = transformation(l1, r1)
    nodes.zipWithIndex.map {
      case (n, i) =>
        Map(n -> Chain(output.slice(i, :>)))
    }.combineAll
  }

  def linearLayer(
      path: SymbolPath,
      input: CompNode
  ): CompNode = {
    linear(path / 'L0, dimMessage)(input)
  }

  val messageLayerModel = "2 FC"

  def messageLayer(path: SymbolPath)(input: CompNode): CompNode = {
    fcNetwork(path, numLayer = 2)(input)
  }

  def nonLinearLayer(path: SymbolPath)(input: CompNode): CompNode = {
    fcNetwork(path, numLayer = 2)(input)
  }

  def fcNetwork(path: SymbolPath, numLayer: Int)(input: CompNode): CompNode = {
    require(numLayer >= 1)
    def oneLayer(i: Int)(input: CompNode) = {
      val p = path / Symbol(s"L$i")
      linear(p, dimMessage)(relu(input))
    }
    val x0 = input ~> linear(path / Symbol("L0"), dimMessage)
    (1 until numLayer).foldLeft(x0)((x, i) => oneLayer(i)(x))
  }

  def predictionLayer(
      path: SymbolPath,
      input: CompNode,
      useDropout: Boolean = false
  ): CompNode = {
    def oneLayer(name: Symbol)(input: CompNode) = {
      val p = path / name
      linear(p, dimMessage)(input) ~> relu
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
