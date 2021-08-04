package lambdanet.correctness

import java.util.concurrent.ThreadLocalRandom

object Sampling {

  /**
    * @param items items with unnormalized PMF
    */
  def choose[T](items: Seq[(T, Double)]): T = {
    val p = ThreadLocalRandom.current().nextDouble()
    choose(items, p)
  }

  def choose[T](items: Seq[(T, Double)], p: Double): T = {
    assert(0 <= p && p <= 1)

    var totalProb = 0.0
    for ((_, prob) <- items) {
      totalProb += prob
    }
    val adjustedProb = totalProb * p

    var sum = 0.0
    for ((item, prob) <- items) {
      sum += prob
      if (sum >= adjustedProb) {
        return item
      }
    }
    throw new NoSuchElementException
  }
}
