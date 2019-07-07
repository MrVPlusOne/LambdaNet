package lambdanet.train

import funcdiff.TensorExtension.oneHot
import funcdiff._
import lambdanet._

import scala.collection.GenSeq

trait LossModel {
  def name: String

  protected def impl(
      logitsVec: GenSeq[CompNode],
      targets: Vector[Int],
      predSpaceSize: Int,
  ): CompNode

  def predictionLoss(
      logitsVec: GenSeq[CompNode],
      targets: Vector[Int],
      predSpaceSize: Int,
  ): CompNode = {
    impl(logitsVec, targets, predSpaceSize)
      .tap { loss =>
        if (loss.value.squeeze() > 20) {
          val displayLogits = logitsVec.zipWithIndex
            .map { case (l, i) => s"iteration $i: $l" }
            .mkString("\n")
          printWarning(
            s"Abnormally large loss: ${loss.value}, logits: \n$displayLogits",
          )
        }
      }
  }

  def crossEntropyLoss(
      logits: CompNode,
      targets: Vector[Int],
      predSpaceSize: Int,
  ): CompNode = {
    mean(crossEntropyOnSoftmax(logits, oneHot(targets, predSpaceSize)))
  }
}

object LossModel {
  object EchoLoss extends LossModel {
    def name = "EchoLoss"

    def impl(
        logitsVec: GenSeq[CompNode],
        targets: Vector[Int],
        predSpaceSize: Int,
    ): Loss = {
      val losses = logitsVec.map { l =>
        crossEntropyLoss(l, targets, predSpaceSize)
      }
      val len = losses.length
      val weights = (1 to len).map(i => 1.0 / i).reverse
      val sum = weights.sum
      losses
        .zip(weights)
        .map { case (l, w) => l * w }
        .pipe(ls => total(ls.toVector) / sum)
    }
  }
}
