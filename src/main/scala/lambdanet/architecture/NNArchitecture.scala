package lambdanet.architecture

import lambdanet._
import botkop.numsca
import botkop.numsca.Tensor
import cats.data.Chain
import funcdiff._
import lambdanet.NewInference.{LabelVector, Message, MessageKind, MessageModel}
import lambdanet.translation.PredicateGraph.{PNode, PType, ProjNode}

import scala.collection.GenSeq

abstract class NNArchitecture(
    name: String,
    dimMessage: Int,
    pc: ParamCollection,
) {

  type Embedding = Map[ProjNode, CompNode]
  val layerFactory: LayerFactory = LayerFactory(name, pc)

  import layerFactory._

  def initialEmbedding(projectNodes: Set[ProjNode]): Embedding

  def similarity(
      inputMatrix: CompNode,
      candidateMatrix: CompNode,
  ): CompNode

  def calculateMessages(
      kind: MessageKind,
      models: Vector[MessageModel],
      encodeNode: PNode => CompNode,
      encodeLabel: Symbol => CompNode,
      encodeFixedType: PType => CompNode,
  ): Map[ProjNode, Chain[Message]]

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
      randomVar('emptyObject)
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

  def encodeLibTerm(
      experience: CompNode,
      signature: CompNode,
      name: CompNode,
  ): CompNode = {
    singleLayer(
      'encodeLibTerm,
      experience.concat(signature, axis = 1).concat(name, axis = 1),
    )
  }

  def encodeLibType(experience: CompNode, name: CompNode): CompNode = {
    singleLayer('encodeLibType, experience.concat(name, axis = 1))
  }

  def mergeMessages(
      messages: GenSeq[(ProjNode, Chain[Message])],
      embedding: ProjNode => CompNode,
  ): Map[ProjNode, Message] = {
    messages
      .map {
        case (n, ms) =>
          val n1 = embedding(n)
          val values = concatN(axis = 0)(ms.toVector)
          val keys = singleLayer('mergeMsgs / 'transKey, values)

          //todo: check other kinds of attentions
          val attention = softmax(keys.dot(n1.t).t / dimMessage)
          n -> attention.dot(values)
      }
      .seq
      .toMap
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
  def verticalBatching[K](
      inputs: Vector[(K, CompNode)],
      transformation: CompNode => CompNode,
  ): Map[K, Chain[CompNode]] = {
    import cats.implicits._
    import numsca.:>

    val (nodes, vectors) = inputs.unzip

    val output = transformation(concatN(axis = 0)(vectors))
    nodes.zipWithIndex.map {
      case (n, i) =>
        Map(n -> Chain(output.slice(i, :>)))
    }.combineAll
  }

  def singleLayer(path: SymbolPath, input: CompNode): CompNode = {
    linear(path / 'linear, dimMessage)(input) ~> relu
  }

  val encodePosition = {
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
    val maxFunctionArity = 100
    val map = (-1 to maxFunctionArity).map(i => i -> encodePosition(i)).toMap

    pos: Int => map.getOrElse(pos, encodePosition(pos))
  }

  private val normalizeFactor = 0.1 / math.sqrt(dimMessage)
  def randomVec(): Tensor = {
    numsca.randn(1, dimMessage) * normalizeFactor
  }

  def randomVar(name: SymbolPath): CompNode = {
    getVar(name)(randomVec())
  }

  def zeroVec() = numsca.zeros(1, dimMessage)

}
