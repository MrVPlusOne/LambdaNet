package lambdanet.architecture

import botkop.numsca.Tensor
import funcdiff._

import scala.collection.GenSeq
import lambdanet._
import lambdanet.PrepareRepos.ParsedRepos
import lambdanet.train.Datum
import lambdanet.translation.PredicateGraph._

import collection.concurrent.TrieMap
import scala.util.Random

trait LabelEncoder {
  def name: String

  private val random = new Random()

  def newEncoder(dropoutProb: Double): Symbol => CompNode = {
    val map = new TrieMap[Symbol, CompNode]()
    l: Symbol => {
      val r = random.synchronized(random.nextDouble())
      if (r < dropoutProb) dropoutImpl
      else map.getOrElseUpdate(l, impl(l))
    }
  }

  protected def dropoutImpl: CompNode

  protected def impl(label: Symbol): CompNode
}

object LabelEncoder {

  case class RandomLabelEncoder(architecture: NNArchitecture)
      extends LabelEncoder {
    def name: String = "RandomLabelEncoder"

    protected def impl(label: Symbol): CompNode = {
      dropoutImpl
    }

    protected def dropoutImpl: CompNode =
      const(architecture.randomUnitVec())
  }

  case class ConstantLabelEncoder(architecture: NNArchitecture)
      extends LabelEncoder {
    def name: String = "ConstLabelEncoder"

    private val zeroVec: Tensor = architecture.zeroVec()

    protected def impl(label: Symbol): CompNode = zeroVec

    protected def dropoutImpl: CompNode = zeroVec
  }

  import scala.collection.GenSeq

  case class TrainableLabelEncoder(
      trainSet: Vector[Datum],
      coverageGoal: Double,
      architecture: NNArchitecture
  ) extends LabelEncoder {
    import cats.implicits._

    def name: String = "TrainableLabelEncoder"

    private val labelsMap: Map[Symbol, CompNode] = {
      val totalUsages = trainSet.foldMap { p =>
        val predsUsage = p.predictor.graph.predicates.toVector.collect {
          case DefineRel(_, expr) =>
            expr.allLabels.toVector.foldMap(nameUsages)
          case HasName(_, name) =>
            nameUsages(name)
        }.combineAll

        val annotsUsage = p.annotations.toVector.foldMap {
          case (_, t) => t.allLabels.toVector.foldMap(nameUsages)
        }

        Vector(predsUsage, annotsUsage).combineAll
      }

      val (labels, achieved) =
        SM.selectBasedOnFrequency(totalUsages.toSeq, coverageGoal)
      printResult(s"number of labels selected: ${labels.length}")
      printResult(s"coverage achieved: $achieved")
      printResult(s"Fist 100 labels: ${labels.take(100)}")

      labels.map {
        case (s, _) =>
          s -> architecture.randomVar('label / s)
      }.toMap
    }

    def isLibLabel(label: Symbol): Boolean = labelsMap.contains(label)

    protected def impl(label: Symbol): CompNode = {
      labelsMap.getOrElse(label, dropoutImpl)
    }

    protected def dropoutImpl: CompNode =
      architecture.randomVar('label / '?)

    private def nameUsages(name: Symbol): Map[Symbol, Int] = {
      Map(name -> 1)
    }

  }

  case class SegmentedLabelEncoder(
      trainSet: Vector[Datum],
      coverageGoal: Double,
      architecture: NNArchitecture
  ) extends LabelEncoder {
    import cats.implicits._

    def name: String = "SegmentedLabelEncoder"

    private val segmentsMap: Map[Segment, CompNode] = {
      val totalUsages = trainSet.foldMap { p =>
        val predsUsage = p.predictor.graph.predicates.toVector.collect {
          case DefineRel(_, expr) =>
            expr.allLabels.toVector.foldMap(nameUsages)
          case HasName(_, name) =>
            nameUsages(name)
        }.combineAll

        val annotsUsage = p.annotations.toVector.foldMap {
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
          s -> architecture.randomUnitVar("segments" / s.symbol)
      }.toMap
    }

    private val zeroVec = architecture.zeroVec()

    protected def impl(l: Symbol): CompNode = {
      def encodeSeg(seg: Segment): CompNode = {
        segmentsMap.getOrElse(seg, dropoutImpl)
      }
      segmentName(l)
        .map(encodeSeg)
        .pipe(totalSafe(_, zeroVec))
    }

    protected def dropoutImpl: CompNode =
      architecture.randomVar('segments / '?)

    case class Segment(symbol: Symbol)
    def segmentName(symbol: Symbol): Vector[Segment] = {
      def splitCamelCase(s: String): Vector[Segment] = {
        s.replaceAll(
            String.format(
              "%s|%s|%s",
              "(?<=[A-Z])(?=[A-Z][a-z])",
              "(?<=[^A-Z])(?=[A-Z])",
              "(?<=[A-Za-z])(?=[^A-Za-z])"
            ),
            " "
          )
          .split("\\s+")
          .map(s => Segment(Symbol(s.toLowerCase.replaceAll("[0-9]", ""))))
          .toVector
      }

      symbol.name.split("_+").toVector.flatMap(splitCamelCase)
    }

    def nameUsages(name: Symbol): Map[Segment, Int] = {
      segmentName(name).foldMap(s => Map(s -> 1))
    }
  }
}
