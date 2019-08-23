package lambdanet.train

import funcdiff.TensorExtension.oneHot
import funcdiff._
import lambdanet._

import scala.collection.GenSeq

/** Mixes the losses from each iteration into one total loss */
trait LossModel {
  def name: String

  protected def impl(
      losses: GenSeq[CompNode]
  ): CompNode

  def predictionLoss(
      logitsVec: GenSeq[CompNode],
  ): CompNode = {
    impl(logitsVec)
      .tap { loss =>
        if (loss.value.squeeze() > 20) {
          val displayLogits = logitsVec.zipWithIndex
            .map { case (l, i) => s"iteration $i: $l" }
            .mkString("\n")
          printWarning(
            s"Abnormally large loss: ${loss.value}, logits: \n$displayLogits"
          )
        }
      }
  }
}

object LossModel {
  object EchoLoss extends LossModel {
    def name = "EchoLoss"

    def impl(
        losses: GenSeq[CompNode]
    ): Loss = {
      val len = losses.length
      val weights = (1 to len).map(i => 1.0 / i).reverse
      val sum = weights.sum
      losses
        .zip(weights)
        .map { case (l, w) => l * w }
        .pipe(ls => plusN(ls.toVector) / sum)
    }
  }

  object NormalLoss extends LossModel {
    def name = "NormalLoss"

    def impl(
        losses: GenSeq[CompNode],
    ): Loss = {
      losses.last
    }
  }
}
