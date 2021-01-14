package lambdanet.correctness

import cats.Monoid
import cats.implicits.catsSyntaxSemigroup
import lambdanet.translation.PredicateGraph.{PNode, PType}
import lambdanet.utils.Statistics.Average

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
