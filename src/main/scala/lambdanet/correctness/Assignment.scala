package lambdanet.correctness

import cats.Monoid
import cats.implicits.catsSyntaxSemigroup
import lambdanet.translation.PredicateGraph.{PNode, PType}

object Assignment {
  case class Difference(results: TypeDistrs, diff: Map[PNode, (PType, PType)]) {
    def pPrint(): String = {
      diff
        .map {
          case (node, (gold, pred)) =>
            s"$node -> ($gold, $pred): (${results(node).typeProb.get(gold)}, ${results(node).typeProb.get(pred)})"
        }
        .mkString("\n")
    }

    override def toString: String = pPrint()
  }

  case class DiffStats(results: TypeDistrs, diff: Map[PNode, (PType, PType)]) {
    case class Average[T](sum: T, count: Int)(
        implicit fractional: Fractional[T]
    ) {
      def value: T =
        if (count == 0) {
          fractional.zero
        } else {
          fractional.div(sum, fractional.fromInt(count))
        }
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

    case class DiffProb(
        averageGoldProb: Average[Double],
        averagePredProb: Average[Double],
        unseenGoldCount: Int
    )

    object DiffProb {
      def apply(
          maybeGoldProb: Option[Double],
          maybePredProb: Option[Double]
      ): DiffProb = {
        val averageGoldProb = Average[Double](maybeGoldProb)
        new DiffProb(
          averageGoldProb,
          Average(maybePredProb),
          1 - averageGoldProb.count
        )
      }
    }

    implicit val DiffProbMonoid: Monoid[DiffProb] = new Monoid[DiffProb] {
      def empty: DiffProb = DiffProb(Average(0, 0), Average(0, 0), 0)

      def combine(x: DiffProb, y: DiffProb): DiffProb = DiffProb(
        x.averageGoldProb |+| y.averageGoldProb,
        x.averagePredProb |+| y.averagePredProb,
        x.unseenGoldCount + y.unseenGoldCount
      )
    }

    val pairs: Map[(PType, PType), Map[PNode, (PType, PType)]] =
      diff.groupBy(_._2)
    val counts: Seq[((PType, PType), Int)] =
      pairs.mapValuesNow(_.size).toSeq.sortBy(-_._2)
    val probs: Seq[((PType, PType), DiffProb)] = pairs
      .mapValuesNow(_.map {
        case (node, (gold, pred)) =>
          // fixme: Use log probability
          DiffProb(
            results(node).typeProb.get(gold),
            results(node).typeProb.get(pred)
          )
      }.reduce(DiffProbMonoid.combine))
      .toSeq

    override def toString: String = {
      counts
        .zip(probs)
        .map {
          case ((pair, count), (_, diffProb)) =>
            s"$pair: $count, " +
              f"${diffProb.averageGoldProb.value}%.3f -> ${diffProb.averagePredProb.value}%.3f," +
              s"unseenGoldCount = ${diffProb.unseenGoldCount}"
        }
        .mkString("\n")
    }
  }

  object DiffStats {
    def apply(difference: Difference): DiffStats =
      new DiffStats(difference.results, difference.diff)
  }

  def diff(
      results: TypeDistrs,
      a: Assignment,
      b: Assignment
  ): Difference = {
    val set =
      for (node <- a.keySet if a(node) != b(node))
        yield (node, (a(node), b(node)))
    Difference(results, set.toMap)
  }
}
