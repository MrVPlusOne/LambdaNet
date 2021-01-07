package lambdanet.correctness

import scala.util.Random

object Sampling {
  /**
    * @param probs unnormalized PMF
    */
  def choose[T](items: Seq[T], probs: Seq[Double]): T = {
    val p = Random.nextDouble()
    choose(items, probs, p)
  }

  def choose[T](items: Seq[T], probs: Seq[Double], p: Double): T = {
    assert(items.length == probs.length)
    assert(0 <= p && p <= 1)
    val totalProb = probs.sum
    val adjustedProb = totalProb * p
    val cmf = probs.scanLeft(0.0)((sum, y) => sum + y).drop(1)
    items.zip(cmf).dropWhile { case (_, y) => y < adjustedProb }.head._1
  }
}
