package lambdanet.train

import botkop.numsca
import botkop.numsca.{:>, Tensor, argmaxAxis}
import funcdiff.TensorExtension.oneHot
import funcdiff._
import lambdanet._

case class TopNDistribution[T](distr: Vector[(Real, T)]) {
  def topValue: T = distr.head._2

  val typeProb: Map[T, Real] = distr.map { case (k, v) => (v, k) }.toMap

  def map[V](f: T => V): TopNDistribution[V] =
    TopNDistribution(distr.map { case (prob, t) => (prob, f(t)) })
}

object TopNDistribution {
  def apply[T](
      distr: Vector[(Real, T)],
      cacheTypeProb: Map[T, Real]
  ): TopNDistribution[T] = new TopNDistribution(distr) {
    override val typeProb: Map[T, Real] = cacheTypeProb
  }
}

trait DecodingResult {
  def topPredictions: Vector[Int]

  def topNPredictionsWithCertainty(n: Int): Vector[TopNDistribution[Int]]

  def toLoss(
      targets: Vector[Int],
      projWeight: Real,
      libNum: Int,
      aggMode: LossAggMode.Value,
  ): CompNode

  protected def crossEntropyWithLogitsLoss(
      logits: CompNode,
      targets: Vector[Int]
  ): CompNode = {
    val predSpaceSize = logits.shape(1).toInt
    crossEntropyOnSoftmax(logits, oneHot(targets, predSpaceSize))
  }
}

object DecodingResult {
  def sortedRows(
      n: Int
  )(logits: Tensor, offset: Int): IndexedSeq[TopNDistribution[Int]] =
    numsca.softmax(logits).rowArrays.map {
      _.zip(Stream.from(offset))
        .sortBy(_._1)
        .reverse
        .take(n)
        .toVector
        .pipe(TopNDistribution(_))
    }
}

import lambdanet.train.DecodingResult._

/**
  * Defines how to compute the total loss from the losses of individual
  * type variables. When mode is `Sum`, it means we are trying to maximize
  * the expected number of correct labels. When mode is `Product`, it means
  * we are trying to maximize the joint probability of predicting all type
  * variables correctly with the assumption that the type distributions on each
  * type variable is fully independent.
  */
object LossAggMode extends Enumeration {
  val Sum, Product = Value
}

/**
  * Build a single softmax from both library type and project type similarities.
  * This loss is equal to log(P), and P's definition depends on `aggMode`:
  * with P = \product_i{P_i}, where P_i is the
  * probability of predicting correctly for label i.
  */
case class Joint(logits: CompNode) extends DecodingResult {
  def topPredictions: Vector[Int] = {
    numsca
      .argmaxAxis(logits.value, axis = 1)
      .dataSlow
      .map(_.toInt)
      .toVector
  }

  def toLoss(
      targets: Vector[Int],
      projWeight: Double,
      libNum: Int,
      aggMode: LossAggMode.Value,
  ): Loss = {
    val weights = targets
      .map(_ < libNum)
      .map(if (_) 1.0 else projWeight)
      .pipe(x => Tensor(x.toArray).reshape(-1, 1))
    val logPs = crossEntropyWithLogitsLoss(
      logits,
      targets
    ) // this gives the log probability of correctness for each node
    if (aggMode == LossAggMode.Product)
      mean(logPs * weights)
    else
      log(mean(exp(logPs) * weights))
  }

  def sortedPredictions: Vector[Vector[Int]] = {
    import numsca._
    val Shape(Vector(row, _)) = logits.shape
    (0 until row.toInt).map { r =>
      logits
        .value(r, :>)
        .dataSlow
        .zipWithIndex
        .sortBy(x => -x._1)
        .map(_._2)
        .toVector
    }.toVector
  }

  def topNPredictionsWithCertainty(n: Int): Vector[TopNDistribution[Int]] = {
    sortedRows(n)(logits.value, 0).toVector
  }
}

/** First decides whether is a library type, then select from the
  * corresponding softmax distributions */
case class TwoStage(
    isLibLogits: CompNode,
    libLogits: CompNode,
    projLogits: Option[CompNode]
) extends DecodingResult {
  val libNum: Int = libLogits.shape(1).toInt

  def topPredictions: Vector[Int] = {
    val libMax = argmaxAxis(libLogits.value, 1).dataSlow
    if (projLogits.isEmpty)
      return libMax.map(_.toInt).toVector

    val projMax = argmaxAxis(projLogits.get.value, 1).dataSlow
    val isLib = isLibLogits.value.dataSlow.map(_ > 0)
    isLib.zipWithIndex.map {
      case (b, i) =>
        if (b) libMax(i).toInt else projMax(i).toInt + libNum
    }.toVector
  }

  def topNPredictionsWithCertainty(n: Int): Vector[TopNDistribution[Int]] = {
    val libRows = sortedRows(n)(libLogits.value, 0)
    if (projLogits.isEmpty)
      return libRows.toVector

    val projRows = sortedRows(n)(projLogits.get.value, libNum)
    val isLib = isLibLogits.value.dataSlow.map(_ > 0)
    isLib.zipWithIndex.map {
      case (b, i) =>
        if (b) libRows(i) else projRows(i)
    }.toVector
  }

  def toLoss(
      targets: Vector[Int],
      projWeight: Real,
      libNum: Int,
      aggMode: LossAggMode.Value,
  ): Loss = {
    def lossFromRows(rows: Vector[CompNode], targets: Vector[Int]) = {
      if (rows.isEmpty) const(Tensor(0.0).reshape(1, 1))
      else
        stackRows(rows).pipe(crossEntropyWithLogitsLoss(_, targets))
    }

    val isLib = targets.map(_ < libNum)
    val isLibTensor = isLib
      .map(if (_) 1.0 else 0.0)
      .pipe(x => Tensor(x.toArray).reshape(-1, 1))

    val isLibWeights = isLib
      .map(if (_) 1.0 else projWeight)
      .pipe(x => Tensor(x.toArray).reshape(-1, 1))

    val binaryLoss =
      crossEntropyOnSigmoid(isLibLogits, isLibTensor) * isLibWeights

    var libRows = Vector[CompNode]()
    var projRows = Vector[CompNode]()
    isLib.zipWithIndex.foreach {
      case (b, row) =>
        if (b) libRows :+= libLogits.slice(row, :>)
        else projRows :+= projLogits.get.slice(row, :>)
    }

    val libTargets = targets.filter(_ < libNum)
    val libLoss = lossFromRows(libRows, libTargets)

    if (projLogits.isEmpty) return {
      mean(libLoss) + mean(binaryLoss)
    }

    val projTargets = targets.filter(_ >= libNum).map(_ - libNum)
    val projLoss = lossFromRows(projRows, projTargets) * projWeight

    if(aggMode == LossAggMode.Product)
      mean(libLoss.concat(projLoss, axis = 0)) + mean(binaryLoss)
    else
      throw new NotImplementedError(s"Loss for mode $aggMode not implemented yet.")
  }
}
