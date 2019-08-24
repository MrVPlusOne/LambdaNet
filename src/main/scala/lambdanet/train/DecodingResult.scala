package lambdanet.train

import lambdanet._
import botkop.numsca
import funcdiff.TensorExtension.oneHot
import funcdiff._
import numsca.{Tensor, argmaxAxis, :>}

trait DecodingResult {
  def topPredictions: Vector[Int]

  def toLoss(targets: Vector[Int], projWeight: Real, libNum: Int): CompNode

  protected def crossEntropyWithLogitsLoss(
      logits: CompNode,
      targets: Vector[Int]
  ): CompNode = {
    val predSpaceSize = logits.shape(1).toInt
    crossEntropyOnSoftmax(logits, oneHot(targets, predSpaceSize))
  }
}

case class Joint(logits: CompNode) extends DecodingResult {
  def topPredictions: Vector[Int] = {
    numsca
      .argmaxAxis(logits.value, axis = 1)
      .data
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
        .data
        .zipWithIndex
        .sortBy(x => -x._1)
        .map(_._2)
        .toVector
    }.toVector
  }
}

case class TwoStage(
    isLibLogits: CompNode,
    libLogits: CompNode,
    projLogits: Option[CompNode]
) extends DecodingResult {
  val libNum: Int = libLogits.shape(1).toInt

  def topPredictions: Vector[Int] = {
    val libMax = argmaxAxis(libLogits.value, 1).data
    if (projLogits.isEmpty)
      return libMax.map(_.toInt).toVector

    val projMax = argmaxAxis(projLogits.get.value, 1).data
    val isLib = isLibLogits.value.data.map(_ > 0)
    isLib.zipWithIndex.map {
      case (b, i) =>
        if (b) libMax(i).toInt else projMax(i).toInt + libNum
    }.toVector
  }

  def toLoss(targets: Vector[Int], projWeight: Real, libNum: Int): Loss = {
    def lossFromRows(rows: Vector[CompNode], targets: Vector[Int]) = {
      if (rows.isEmpty) const(Tensor(0.0).reshape(1, 1))
      else
        concatN(0, fromRows = true)(rows)
          .pipe(crossEntropyWithLogitsLoss(_, targets))
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
