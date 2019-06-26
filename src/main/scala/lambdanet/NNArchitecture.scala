package lambdanet

import botkop.numsca
import botkop.numsca.{:>, Tensor}
import cats.data.Chain
import funcdiff._
import lambdanet.NewInference.{LabelVector, Message, MessageKind, MessageModel}
import lambdanet.translation.PredicateGraph.{PNode, PType, ProjNode}

case class NNArchitecture(dimMessage: Int, layerFactory: LayerFactory) {
  import layerFactory._

  private val normalizeFactor = 0.1 / math.sqrt(dimMessage)
  def randomVec(): Tensor = {
    numsca.randn(1, dimMessage) * normalizeFactor
  }

  def similarity(
      inputMatrix: CompNode,
      candidateMatrix: CompNode,
  ) = {
    val inputs1 = singleLayer('similarityInputs, inputMatrix)
    val candidates1 = singleLayer('similarityCandidates, candidateMatrix)
    inputs1.dot(candidates1.t) / dimMessage
  }

  def calculateMessages(
      kind: MessageKind,
      models: Vector[MessageModel],
      encodeNode: PNode => CompNode,
      encodeLabel: Symbol => CompNode,
      encodeFixedType: PType => CompNode,
  ): Map[ProjNode, Chain[Message]] = {
    import MessageKind._
    import MessageModel._
    import cats.implicits._

    val result0 = kind match {
      case KindSingle(name) =>
        val paired = models
          .asInstanceOf[Vector[Single]]
          .map(s => s.n -> encodeNode(s.n))
        verticalBatching(paired, singleLayer(name, _))
      case KindBinary(name) =>
        val (n1s, n2s, inputs) = models
          .asInstanceOf[Vector[Binary]]
          .map { s =>
            val merged = encodeNode(s.n1).concat(encodeNode(s.n2), axis = 1)
            (s.n1, s.n2, merged)
          }
          .unzip3
        verticalBatching(n1s.zip(inputs), singleLayer(name / 'left, _)) |+|
          verticalBatching(n2s.zip(inputs), singleLayer(name / 'right, _))
      case KindLabeled(name, labelType) =>
        val (n1s, n2s, inputs) = models
          .asInstanceOf[Vector[Labeled]]
          .map {
            case Labeled(n1, n2, label) =>
              val lEmbed = labelType match {
                case LabelType.Field =>
                  val l = label.asInstanceOf[Label.Field].get
                  encodeLabel(l)
                case LabelType.Position =>
                  val pos = label.asInstanceOf[Label.Position].get
                  encodePosition(pos)
              }
              val input = concatN(axis = 1)(
                Vector(encodeNode(n1), encodeNode(n2), lEmbed),
              )
              (n1, n2, input)
          }
          .unzip3
        verticalBatching(n1s.zip(inputs), singleLayer(name / 'left, _)) |+|
          verticalBatching(n2s.zip(inputs), singleLayer(name / 'right, _))
      case KindWithType(name) =>
        // limitation: information flows in only one direction
        val inputs = models
          .asInstanceOf[Vector[WithType]]
          .map {
            case WithType(n, ty) =>
              n -> encodeFixedType(ty)
          }
        verticalBatching(inputs, singleLayer(name, _))
    }

    // filter out all messages for lib nodes
    result0.collect {
      case (k, v) if k.fromProject => ProjNode(k) -> v
    }
  }

  def encodeFunction(args: Vector[CompNode], to: CompNode): CompNode = {
    (to +: args).zipWithIndex
      .map {
        case (a, i) =>
          a.concat(encodePosition(i), axis = 1)
      }
      .pipe(concatN(axis = 0))
      .pipe(singleLayer('encodeFunction, _))
      .pipe(sum(_, axis = 0))
  }

  def encodeObject(elements: Vector[(LabelVector, CompNode)]): CompNode = {
    if (elements.isEmpty) {
      getVar('emptyObject) { randomVec() }
    } else {
      elements
        .map {
          case (v1, v2) => v1.concat(v2, axis = 1)
        }
        .pipe(concatN(axis = 0))
        .pipe(singleLayer('encodeObject, _))
        .pipe(sum(_, axis = 0))
    }
  }

  def encodeLibTerm(experience: CompNode, signature: CompNode): CompNode = {
    singleLayer('encodeLibTerm, experience.concat(signature, axis = 1))
  }

  def mergeMessages(
      messages: Map[ProjNode, Chain[Message]],
      embedding: ProjNode => CompNode,
  ): Map[ProjNode, Message] = {
    messages.map {
      case (n, ms) =>
        val n1 = embedding(n)
        val values = concatN(axis = 0)(ms.toVector)
        val keys = singleLayer('mergeMsgs / 'transKey, values)

        //todo: check other kinds of attentions
        val attention = softmax(keys.dot(n1.t).t / dimMessage)
        n -> attention.dot(values)
    }
  }

  def update(
      embedding: Map[ProjNode, CompNode],
      messages: Map[ProjNode, CompNode],
  ): Map[ProjNode, CompNode] = {
    import numsca._

    val inputs = embedding.toVector.map {
      case (k, v) =>
        k -> v.concat(messages.getOrElse(k, emptyMessage), axis = 1)
    }
    verticalBatching(inputs, stacked => {
      val old = stacked.slice(:>, 0 :> dimMessage)
      val msg = stacked.slice(:>, dimMessage :> 2 * dimMessage)
      gru('updateEmbedding)(old, msg)
    }).mapValuesNow { chain =>
      val Vector(x) = chain.toVector
      x
    }
  }

  private val emptyMessage = getVar('emptyMessage) { randomVec() }

  /** stack inputs vertically (axis=0) for batching */
  private def verticalBatching[K](
      inputs: Vector[(K, CompNode)],
      transformation: CompNode => CompNode,
  ): Map[K, Chain[CompNode]] = {
    import numsca.:>
    import cats.implicits._

    val (nodes, vectors) = inputs.unzip

    val output = transformation(concatN(axis = 0)(vectors))
    nodes.zipWithIndex.map {
      case (n, i) =>
        Map(n -> Chain(output.slice(i, :>)))
    }.combineAll
  }

  private def singleLayer(path: SymbolPath, input: CompNode): CompNode = {
    linear(path / 'linear, dimMessage)(input) ~> relu
  }

  private val encodePosition = {
    def encodePosition(pos: Int): CompNode = {
      assert(pos >= -1)
      if (pos == -1) {
        getVar('position / 'head) { randomVec() }
      } else {
        getConst('position / Symbol(pos.toString)) {
          val phases = (0 until dimMessage / 2).map { dim =>
            pos / math.pow(1000, 2.0 * dim / dimMessage)
          }
          numsca
            .Tensor(phases.map(math.sin) ++ phases.map(math.cos): _*)
            .reshape(1, -1)
        }
      }
    }
    val maxFunctionArity = 50
    (-1 to maxFunctionArity).map(i => i -> encodePosition(i)).toMap
  }

}
