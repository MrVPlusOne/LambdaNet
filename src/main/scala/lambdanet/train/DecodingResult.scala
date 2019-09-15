package lambdanet.train

import lambdanet._
import botkop.numsca
import funcdiff.TensorExtension.oneHot
import funcdiff._
import numsca.{:>, Tensor, argmaxAxis}

case class TopNDistribution[T](distr: Vector[(Real, T)]) {
  def topValue: T = distr.head._2

  def map[V](f: T => V): TopNDistribution[V] =
    TopNDistribution(distr.map { case (prob, t) => (prob, f(t)) })

}

trait DecodingResult {
  def topPredictions: Vector[Int]

  def topNPredictionsWithCertainty(n: Int): Vector[TopNDistribution[Int]]

  def toLoss(targets: Vector[Int], projWeight: Real, libNum: Int): CompNode

  protected def crossEntropyWithLogitsLoss(
      logits: CompNode,
      targets: Vector[Int]
  ): CompNode = {
    val predSpaceSize = logits.shape(1).toInt
    crossEntropyOnSoftmax(logits, oneHot(targets, predSpaceSize))
  }
}

object DecodingResult {
  def sortedRows(n: Int)(logits: Tensor, offset: Int): IndexedSeq[TopNDistribution[Int]] =
    numsca.softmax(logits).rowArrays.map {
      _.zip(Stream.from(offset))
        .sortBy(_._1)
        .reverse
        .take(n)
        .toVector
        .pipe(TopNDistribution.apply)
    }
}

import DecodingResult._

case class Joint(logits: CompNode) extends DecodingResult {
  def topPredictions: Vector[Int] = {
    numsca
      .argmaxAxis(logits.value, axis = 1)
      .dataSlow
      .map(_.toInt)
      .toVector
  }

  def toLoss(targets: Vector[Int], projWeight: Double, libNum: Int): Loss = {
    val weights = targets
      .map(_ < libNum)
      .map(if (_) 1.0 else projWeight)
      .pipe(x => Tensor(x.toArray).reshape(-1, 1))
    (crossEntropyWithLogitsLoss(
      logits,
      targets
    ) * weights)
      .pipe(mean)
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

  def toLoss(targets: Vector[Int], projWeight: Real, libNum: Int): Loss = {
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

    mean(libLoss.concat(projLoss, axis = 0)) + mean(binaryLoss)
  }
}
