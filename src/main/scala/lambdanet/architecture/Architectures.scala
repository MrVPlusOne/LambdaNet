package lambdanet.architecture

import botkop.numsca.{:>, between}
import cats.data.Chain
import funcdiff._
import lambdanet.ChainingSyntax
import lambdanet.NeuralInference.Message
import lambdanet.translation.PredicateGraph.ProjNode

import scala.collection.GenSeq

/**
  * A simple GNN architecture that aggregate messages by taking their mean and updates
  * the node embeddings across iterations using sum.
  */
case class SimpleArchitecture(dimEmbedding: Int, pc: ParamCollection)
    extends NNArchitecture(s"Simple-$dimEmbedding", dimEmbedding) {

  def initialEmbedding(projectNodes: Set[ProjNode]): Embedding = {
    val nodeVec = randomVar('nodeInitVec)
    val vars = projectNodes.map(_ -> nodeVec).toMap
    Embedding(vars)
  }

  import layerFactory._

  private val emptyMessage = getVar('emptyMessage) { randomVec() }

//  def mergeMessages[K](
//      name: SymbolPath,
//      messages: Vector[(K, Chain[Message])],
//      embedding: K => CompNode
//  )(implicit mode: GraphMode): Vector[(K, Message)] = {
//    messages
//      .map {
//        case (n, ms) =>
//          n -> meanN(ms.toVector)
//      }
//  }
  def mergeMessages[K](
      name: SymbolPath,
      messages: GenSeq[(K, Chain[Message])],
      embedding: K => CompNode
  )(implicit mode: GraphMode): Map[K, Message] =
    messages.map { case (n, ms) => n -> meanN(ms.toVector) }.seq.toMap

  def update[K](
      name: SymbolPath,
      embedding: Map[K, CompNode],
      messages: Map[K, CompNode]
  )(implicit mode: GraphMode): Map[K, CompNode] = {
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

/**
  * Implements the <a href="https://arxiv.org/abs/1710.10903">Graph Attention Networks</a>
  * GNN architecture. It aggregates messages using the multi-head attention kernel.
  */
@SerialVersionUID(7956816594728538398L)
case class GATArchitecture(
    numHeads: Int,
    dimEmbedding: Int,
    pc: ParamCollection
) extends NNArchitecture(s"GAT-$numHeads-$dimEmbedding", dimEmbedding) {

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
  )(implicit mode: GraphMode): Map[K, Message] = {
    val dimValue = if (numHeads == 1) dimEmbedding else dimEmbedding / 2
    messages
      .map {
        case (n, ms) =>
          val n1 = embedding(n)
          val stacked = stackRows(ms.prepend(n1).toVector) // [N, D]
          val heads = for (i <- 0 until numHeads) yield {
            val prefix = name / s"mergeMsgs$i"
            def trans(name1: Symbol, targetDim: Int)(values: CompNode) =
              linear(prefix / name1, targetDim, useBias = false)(values)

            val key1 = trans('key1, dimValue)(n1) // [1, D]
            val keys2 =
              if (dimValue == dimEmbedding) stacked
              else trans('keys2, dimValue)(stacked) // [N, D]
            val values2 = trans('values2, dimValue)(stacked) // [N, D]

            val attention = softmax(leakyRelu(key1.dot(keys2.t), 0.2)) // [1, N]
            attention.dot(values2) // [1, D]
          }
          val msg =
            if (numHeads == 1) heads.head
            else
              concatN(1, fromRows = true)(heads.toVector)
                .pipe(linear(name / "mergeHeads", dimEmbedding, useBias = false))

          n -> msg
      }
      .seq
      .toMap
  }

  def mergeMessagesBatched[K](
      name: SymbolPath,
      messages: Vector[(K, Chain[Message])],
      embedding: K => CompNode
  )(implicit mode: GraphMode): Vector[(K, Message)] = {
    val dimValue = if (numHeads == 1) dimEmbedding else dimEmbedding / 2
    val (nodes, msgChains) = messages.unzip // N nodes
    val allStacked = stackRows(msgChains.flatMap(_.toVector))
    // records the rows positions that separates nodes
    val stackPositions = msgChains.scanLeft(0)((i, chain) => i + chain.length.toInt)

    val valuesByHead = (0 until numHeads).map { h =>
      val prefix = name / s"mergeMsgs$h"
      val keys1 = stackRows(nodes.map(embedding))
        .pipe(linear(prefix / "keys1", dimValue, useBias = false))
        .rows
      val keys2 = allStacked ~>
        linear(prefix / "keys2", dimValue, useBias = false)
      val values2 = allStacked ~>
        linear(prefix / "values2", dimValue, useBias = false)
      val values1 = nodes.indices.map { i =>
        val key1 = keys1(i) // [1, D]
        val range2 = between(stackPositions(i), stackPositions(i + 1))
        val key2 = keys2.slice(range2, :>) // [M, D]
        val attention = softmax(leakyRelu(key1.dot(key2.t), 0.2)) // [1, M]
        val value2 = values2.slice(range2, :>) // [M, Dv]
        attention.dot(value2) // [1, Dv]
      }
      stackRows(values1) // [N, Dv]
    }
    // horizontally concat the values from different heads together
    val msgMat =
      if (numHeads == 1) valuesByHead.head
      else
        concatN(1)(valuesByHead.toVector) ~> // [N, H*Dv]
          linear(name / "mergeHeads", dimEmbedding, useBias = true) // [N, D]
    nodes.zip(msgMat.rows)
  }

//  import scala.concurrent.duration.Duration
//  import scala.concurrent.{Await, Future}
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
  )(implicit mode: GraphMode): Map[K, CompNode] = {
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
