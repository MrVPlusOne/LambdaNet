package lambdanet.utils

import cats.Monoid

object Statistics {
  import Fractional.Implicits._
  case class Average[T: Fractional](sum: T, count: Int) {
    def value: T =
      average(sum, count)
  }

  object Average {
    def apply[T](
      x: Option[T]
    )(implicit fractional: Fractional[T]): Average[T] =
      if (x.nonEmpty) {
        Average(x.get, 1)
      } else {
        Average(fractional.zero, 0)
      }
  }

  implicit def AverageMonoid[T](
                                 implicit fractional: Fractional[T]
                               ): Monoid[Average[T]] =
    new Monoid[Average[T]] {
      override def empty: Average[T] =
        Average(fractional.zero, 0)

      override def combine(
        x: Average[T],
        y: Average[T]
      ): Average[T] =
        Average(
          fractional.plus(x.sum, y.sum),
          x.count + y.count
        )
    }

  def average[T: Fractional](xs: Seq[T]): T =
    average(xs.sum, xs.size)

  def average[T](sum: T, count: Int)(implicit fractional: Fractional[T]): T =
    if (count == 0) {
      fractional.zero
    } else {
      fractional.div(sum, fractional.fromInt(count))
    }

  def variance[T](xs: Seq[T])(implicit fractional: Fractional[T]): T =
    variance(xs, average(xs))

  def variance[T](xs: Seq[T], mean: T)(implicit fractional: Fractional[T]): T =
    if (xs.size <= 1) {
      fractional.zero
    } else {
      val sum2 = xs.map(x => (x - mean) * (x - mean)).sum
      sum2 / fractional.fromInt(xs.size - 1)
    }

  def stdev[T: Fractional](xs: Seq[T]): Double =
    Math.sqrt(variance(xs).toDouble)

  def stdev[T: Fractional](xs: Seq[T], mean: T): Double =
    Math.sqrt(variance(xs, mean).toDouble)
}
