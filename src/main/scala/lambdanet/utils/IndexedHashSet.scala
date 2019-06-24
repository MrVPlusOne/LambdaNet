package lambdanet.utils

import scala.collection.mutable
import scala.util.Random

class IndexedHashSet[X](var elements: Vector[X]) {
  private val set = mutable.HashSet(elements: _*)

  def +=(e: X): Unit = {
    if (!set.contains(e)) {
      set += e
      elements = elements :+ e
    }
  }

  def sample()(implicit random: Random): X = {
    val idx = random.nextInt(elements.size)
    elements(idx)
  }
}

object IndexedHashSet {
  def apply[X](elements: X*): IndexedHashSet[X] =
    new IndexedHashSet(elements.toVector)
}
