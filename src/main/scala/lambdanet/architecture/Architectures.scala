package lambdanet.architecture

import cats.data.Chain
import funcdiff._
import lambdanet.NeuralInference.Message
import lambdanet.translation.PredicateGraph.ProjNode

import scala.collection.GenSeq
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

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

case class GATArchitecture(
    numHeads: Int,
    dimEmbedding: Int,
    pc: ParamCollection
) extends NNArchitecture(s"GAT-$dimEmbedding", dimEmbedding, pc) {

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

    require(dimEmbedding % numHeads == 0)
    val dimValue = dimEmbedding / numHeads
    messages
      .map {
        case (n, ms) =>
          val n1 = embedding(n)
          val stacked = stackRows(n1 +: ms.toVector) // [N, D]
          val heads = for(i <- 0 until numHeads) yield {
            val prefix = name / Symbol(s"mergeMsgs$i")
            def trans(name1: Symbol, targetDim: Int)(values: CompNode) =
              linear(prefix / name1, targetDim, useBias = false)(
                values
              )

            val key1 = trans('key1, dimValue)(n1) // [1, D]
            val keys2 = trans('keys2, dimValue)(stacked) // [N, D]
            val values2 = trans('values2, dimValue)(stacked) // [N, D]

            val attention = softmax(leakyRelu(key1.dot(keys2.t), 0.2)) // [1, N]
            attention.dot(values2) // [1, D]
          }

        n -> concatN(1,fromRows = true)(heads.toVector)
      }
      .seq
      .toMap
  }

//  def mergeMessages[K](
//      name: SymbolPath,
//      messages: GenSeq[(K, Chain[Message])],
//      embedding: K => CompNode
//  ): Map[K, Message] = {
//    def trans(name1: Symbol, values: CompNode) =
//      linear(name / 'mergeMsgs / name1, dimEmbedding, useBias = false)(
//        values
//      )
//
//    val key1Service = BatchingService(trans('key1, _))
//    val key2Service = BatchingService(trans('key2, _))
//    val values2Service = BatchingService(trans('values2, _))
//
//    import concurrent.ExecutionContext.Implicits.global
//    import cats.implicits._
//
//    val mergedFs = messages.map {
//      case (n, ms) =>
//        val n1 = embedding(n)
//        val key1F = key1Service.register(n1)
//        val Seq(keys2F, values2F) = Seq(key2Service, values2Service).map {
//          service =>
//            (n1 +: ms.toVector)
//              .map(service.register)
//              .sequence
//              .map(stackRows)
//        }
//
//        for {
//          key1 <- key1F
//          keys2 <- keys2F
//          values2 <- values2F
//        } yield {
//          val attention = softmax(leakyRelu(key1.dot(keys2.t), 0.2)) // [1, N]
//          n -> attention.dot(values2) // [1, D]
//        }
//    }.toVector
//
//    val c1 = Future(key1Service.compute())
//    val c2 = Future(key2Service.compute())
//    val c3 = Future(values2Service.compute())
//
//    val resultF = for {
//      _ <- c1
//      _ <- c2
//      _ <- c3
//      values <- mergedFs.sequence
//    } yield {
//      values.toMap
//    }
//    Await.result(resultF, Duration.Inf)
//  }

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
