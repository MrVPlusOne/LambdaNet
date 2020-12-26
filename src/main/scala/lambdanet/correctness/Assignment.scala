package lambdanet.correctness

import cats.Monoid
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
    case class Average[T: Numeric, I: Integral](sum: T, count: I)

    implicit def AverageMonoid[T, I](
        implicit numeric: Numeric[T],
        integral: Integral[I]
    ): Monoid[Average[T, I]] =
      new Monoid[Average[T, I]] {
        override def empty: Average[T, I] =
          Average(numeric.zero, integral.zero)

        override def combine(
            x: Average[T, I],
            y: Average[T, I]
        ): Average[T, I] =
          Average[T, I](
            numeric.plus(x.sum, y.sum),
            integral.plus(x.count, y.count)
          )
      }

    case class DiffProb[I: Integral](
        averageGoldProb: Average[Double, I],
        averagePredProb: Average[Double, I],
        unseenGoldCount: I
    )
//
//    object DiffProb {
//      def apply(maybeGoldProb: Option[Double], maybePredProb: Option[]): DiffProb =
//        new DiffProb(maybeGoldProb, maybePredProb)
//    }
//    case class DiffProbMonoid extends Monoid[]
    val pairs: Map[(PType, PType), Map[PNode, (PType, PType)]] =
      diff.groupBy(_._2)
    val counts = pairs.mapValuesNow(_.size).toSeq.sortBy(-_._2)
    val probs = pairs.mapValuesNow(_.map {
      case (node, (gold, pred)) =>
        (results(node).typeProb.get(gold), results(node).typeProb.get(pred))
    }.filter { case (a, b) => a.nonEmpty && b.nonEmpty })
//    override def toString: String = {}
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
