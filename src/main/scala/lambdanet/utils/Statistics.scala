package lambdanet.utils

import cats.Monoid

object Statistics {
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
}
