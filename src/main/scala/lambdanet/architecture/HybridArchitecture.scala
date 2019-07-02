package lambdanet.architecture

import cats.data.Chain
import funcdiff._
import lambdanet.NewInference.{Message, MessageKind, MessageModel}
import lambdanet.translation.PredicateGraph.{PNode, PType, ProjNode}

case class HybridArchitecture(dimMessage: Int, pc: ParamCollection)
    extends NNArchitecture(s"hybrid-$dimMessage", dimMessage, pc) {

  def initialEmbedding(projectNodes: Set[ProjNode]): Embedding = {
    val vec = randomVar('nodeInitVec)
    projectNodes.map(_ -> vec).toMap
  }

  def similarity(
      inputMatrix: CompNode,
      candidateMatrix: CompNode,
  ): CompNode = {
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
      case KindNaming(name) =>
        val paired = models
          .asInstanceOf[Vector[Naming]]
          .map(s => s.n -> encodeLabel(s.name))
        verticalBatching(paired, singleLayer(name, _))
      case KindBinaryLabeled(name, labelType) =>
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

}
