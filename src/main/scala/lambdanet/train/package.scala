package lambdanet

import botkop.numsca
import botkop.numsca.{Shape, Tensor}
import cats.Monoid
import funcdiff.CompNode
import lambdanet.train.LabelCat

package object train {
  type Logits = CompNode
  type Loss = CompNode
  type Correct = Int
  type LibCorrect = Correct
  type ProjCorrect = Correct
  type ConfusionMatrix = Map[(Int, Int), Int]
  type TruthPosition = Int

  def confusionMatrix(
      predictions: Vector[Int],
      groundTruths: Vector[Int],
      categories: Int
  ): ConfusionMatrix = {
    import cats.implicits._
    predictions
      .zip(groundTruths)
      .map {
        case (p, g) => Map((g, p) -> 1)
      }
      .combineAll
  }

  def nonZero(n: Int): Double = if (n == 0) 1.0 else n.toDouble

  def toAccuracy(counts: Counted[Int]): Double = {
    if (counts.count == 0) 1.0
    else counts.value.toDouble / counts.count
  }

  def toAccuracyD(counts: Counted[Double]): Double = {
    if (counts.count == 0) 1.0
    else counts.value / counts.count
  }

  case class Counted[V](count: Int, value: V)

  object Counted {
    def fromBool(b: Boolean): Counted[Int] =
      Counted(1, if (b) 1 else 0)

    def zero[V](v: V) = Counted(0, v)
  }

  implicit def countedMonoid[V](
      implicit m: Monoid[V]
  ): Monoid[Counted[V]] = new Monoid[Counted[V]] {
    def empty: Counted[V] = Counted(0, m.empty)

    def combine(x: Counted[V], y: Counted[V]): Counted[V] = {
      Counted(x.count + y.count, m.combine(x.value, y.value))
    }
  }

//  implicit object LossMonoid extends Monoid[Loss] {
//    def empty: Loss = 0.0
//
//    def combine(x: Loss, y: Loss): Loss = x + y
//  }

  implicit object TensorMonoid extends Monoid[Tensor] {
    def empty: Tensor = Tensor(0.0)

    def combine(x: Tensor, y: Tensor): Tensor = x + y
  }

  case class StopException(msg: String) extends Exception

  object LabelCat extends Enumeration {
    val Library, Project = Value

    def fromLib(isLib: Boolean): LabelCat.Value =
      if (isLib) Library else Project
  }

  object DatumCat extends Enumeration {
    val Train, Dev, Test = Value
  }
}
