package lambdanet.architecture

import funcdiff._
import lambdanet._
import lambdanet.PrepareRepos.ParsedRepos
import lambdanet.translation.PredicateGraph._

import scala.collection.GenSeq

case class SegmentedLabelEncoder(
    repos: ParsedRepos,
    coverageGoal: Double,
    architecture: NNArchitecture,
) {
  import cats.implicits._
  import repos._

  private val segmentsMap: Map[Segment, CompNode] = {

    val totalUsages = graphs.foldMap {
      case (_, pg, annots) =>
        val predsUsage = pg.predicates.toVector.collect {
          case DefineRel(_, expr) =>
            expr.allLabels.toVector.foldMap(nameUsages)
          case HasName(_, name) =>
            nameUsages(name)
        }.combineAll

        val annotsUsage = annots.toVector.foldMap {
          case (_, t) => t.allLabels.toVector.foldMap(nameUsages)
        }

        Vector(predsUsage, annotsUsage).combineAll
    }

    val (segments, achieved) =
      SM.selectBasedOnFrequency(totalUsages.toSeq, coverageGoal)
    printResult(s"number of segments selected: ${segments.length}")
    printResult(s"coverage achieved: $achieved")
    printResult(s"Fist 100 segs: ${segments.take(100)}")

    segments.map {
      case (s, _) =>
        s -> architecture.randomVar("segments" / s.symbol)
    }.toMap
  }

  def encode(labels: GenSeq[Symbol]): Symbol => CompNode =
    DebugTime.logTime("encode labels") {
      val unknownSeg = architecture.zeroVec()

      def encodeSeg(seg: Segment): CompNode = {
        segmentsMap.getOrElse(seg, architecture.randomVec())
      }

      labels
        .map { l =>
          l -> segmentName(l)
            .map(encodeSeg)
            .pipe(totalSafe(_, unknownSeg))
        }
        .toMap
        .apply
    }

  case class Segment(symbol: Symbol)
  def segmentName(symbol: Symbol): Vector[Segment] = {
    def splitCamelCase(s: String): Vector[Segment] = {
      s.replaceAll(
          String.format(
            "%s|%s|%s",
            "(?<=[A-Z])(?=[A-Z][a-z])",
            "(?<=[^A-Z])(?=[A-Z])",
            "(?<=[A-Za-z])(?=[^A-Za-z])",
          ),
          " ",
        )
        .split("\\s+")
        .map(s => Segment(Symbol(s.toLowerCase)))
        .toVector
    }

    symbol.name.split("_+").toVector.flatMap(splitCamelCase)
  }

  def nameUsages(name: Symbol): Map[Segment, Int] = {
    segmentName(name).foldMap(s => Map(s -> 1))
  }
}
