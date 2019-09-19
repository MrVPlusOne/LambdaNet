package lambdanet.architecture

import botkop.numsca
import cats.data.Chain
import funcdiff._
import lambdanet.NeuralInference.Message
import lambdanet.translation.PredicateGraph.ProjNode

import scala.collection.GenSeq

case class SimpleArchitecture(dimEmbedding: Int, pc: ParamCollection)
    extends NNArchitecture(s"Simple-$dimEmbedding", dimEmbedding, pc) {

  def initialEmbedding(projectNodes: Set[ProjNode]): Embedding = {
    val nodeVec = randomVar('nodeInitVec)
    val vars = projectNodes.map(_ -> nodeVec).toMap
    Embedding(vars)
  }

  import layerFactory._

  private val emptyMessage = getVar('emptyMessage) { randomVec() }

  def mergeMessages[K](
      name: SymbolPath,
      messages: GenSeq[(K, Chain[Message])],
      embedding: K => CompNode
  ): Map[K, Message] = {
    messages
      .map {
        case (n, ms) =>
          n -> meanN(ms.toVector)
      }
      .seq
      .toMap
  }

  def update[K](
      name: SymbolPath,
      embedding: Map[K, CompNode],
      messages: Map[K, CompNode]
  ): Map[K, CompNode] = {
    val inputs = embedding.toVector.map {
      case (k, v) =>
        k -> (v, messages.getOrElse(k, emptyMessage))
    }
    verticalBatching2[K](
      inputs,
      (old, msg) => old + msg // gru(name / 'updateEmbedding)(old, msg)
    ).map {
      case (k, chain) =>
        val Vector(x) = chain.toVector
        k -> x
    }
  }
}

case class GATArchitecture(dimEmbedding: Int, pc: ParamCollection)
    extends NNArchitecture(s"GAT-$dimEmbedding", dimEmbedding, pc) {

  def initialEmbedding(projectNodes: Set[ProjNode]): Embedding = {
    val nodeVec = randomVar('nodeInitVec)
    val vars = projectNodes.map(_ -> nodeVec).toMap
    Embedding(vars)
  }

  import layerFactory._

  private val emptyMessage = getVar('emptyMessage) { randomVec() }

  def mergeMessages[K](
      name: SymbolPath,
      messages: GenSeq[(K, Chain[Message])],
      embedding: K => CompNode
  ): Map[K, Message] = {
    messages
      .map {
        case (n, ms) =>
          def trans(name1: Symbol, values: CompNode) =
            linear(name / 'mergeMsgs / name1, dimEmbedding, useBias = false)(
              values
            )

          val n1 = embedding(n)
          val key1 = trans('key1, n1)  // [1, D]
          val stacked = stackRows(n1 +: ms.toVector) // [N, D]
          val keys2 = trans('keys2, stacked)  // [N, D]
          val values2 = trans('values2, stacked)  // [N, D]

          val attention = softmax(leakyRelu(key1.dot(keys2.t), 0.2)) // [1, N]
          n -> attention.dot(values2) // [1, D]
      }
      .seq
      .toMap
  }

  def update[K](
      name: SymbolPath,
      embedding: Map[K, CompNode],
      messages: Map[K, CompNode]
  ): Map[K, CompNode] = {
    val inputs = embedding.toVector.map {
      case (k, v) =>
        k -> (v, messages.getOrElse(k, emptyMessage))
    }
    verticalBatching2[K](
      inputs,
      (old, msg) => old + msg // gru(name / 'updateEmbedding)(old, msg)
    ).map {
      case (k, chain) =>
        val Vector(x) = chain.toVector
        k -> x
    }
  }

}
