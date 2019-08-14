package lambdanet.architecture

import botkop.numsca.Tensor
import funcdiff._

import lambdanet._
import lambdanet.train.Datum
import lambdanet.translation.PredicateGraph._

import collection.concurrent.TrieMap
import scala.util.Random

trait LabelEncoder {
  def name: String

  protected def dropoutProb: Real

  private val random = new Random()

  def newEncoder(useDropout: Boolean): Symbol => CompNode = {
    val map = new TrieMap[Symbol, CompNode]()

    def shouldDropout(): Boolean =
      if (useDropout) random.synchronized {
        random.nextDouble() < dropoutProb
      } else false

    l: Symbol => {
      map.getOrElseUpdate(l, impl(l, shouldDropout))
    }
  }

  protected def impl(
      label: Symbol,
      shouldDropout: () => Boolean
  ): CompNode
}

object LabelEncoder {

  case class RandomLabelEncoder(architecture: NNArchitecture)
      extends LabelEncoder {
    def name: String = "RandomLabelEncoder"

    def dropoutProb: Real = 0.0

    protected def impl(
        label: Symbol,
        shouldDropout: () => Boolean
    ): CompNode = {
      const(architecture.randomUnitVec())

    }
  }

  case class ConstantLabelEncoder(architecture: NNArchitecture)
      extends LabelEncoder {
    def name: String = "ConstLabelEncoder"

    private val zeroVec: Tensor = architecture.zeroVec()

    protected def dropoutProb: Real = 0.0

    protected def impl(
        label: Symbol,
        shouldDropout: () => Boolean
    ): CompNode = {
      zeroVec
    }
  }

  import scala.collection.GenSeq

  /**
    * @param dropoutThreshold if a label appears more than this number of times
    *                         in the training set, it won't be dropped out during
    *                         training time.
    */
  case class TrainableLabelEncoder(
      trainSet: Vector[Datum],
      coverageGoal: Double,
      architecture: NNArchitecture,
      dropoutProb: Real,
      dropoutThreshold: Int
  ) extends LabelEncoder {
    import cats.implicits._

    def name: String = "TrainableLabelEncoder"

    private val (labelsMap, commonLabels) = {
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
      }.toMap -> labels.collect {
        case (l, freq) if freq > dropoutThreshold => l
      }.toSet
    }

    def isLibLabel(label: Symbol): Boolean = labelsMap.contains(label)

    protected def impl(
        label: Symbol,
        shouldDropout: () => Boolean
    ): CompNode = {
      if (!commonLabels.contains(label) && shouldDropout()) dropoutImpl
      else labelsMap.getOrElse(label, dropoutImpl)
    }

    protected def dropoutImpl: CompNode =
      architecture.randomVar('label / '?)

    private def nameUsages(name: Symbol): Map[Symbol, Int] = {
      Map(name -> 1)
    }

  }

  /**
    * @param dropoutThreshold if a segment appears more than this number of times
    *                         in the training set, it won't be dropped out during
    *                         training time.
    */
  case class SegmentedLabelEncoder(
      trainSet: Vector[Datum],
      coverageGoal: Double,
      architecture: NNArchitecture,
      dropoutProb: Real,
      dropoutThreshold: Int
  ) extends LabelEncoder {
    import cats.implicits._

    def name: String = "SegmentedLabelEncoder"

    private val (segmentsMap, commonSegments) = {
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

      val commonSegments =
        segments.takeWhile(_._2 > dropoutThreshold).map(_._1).toSet
      val segmentsMap = segments.map {
        case (s, _) =>
          s -> architecture.randomUnitVar("segments" / s.symbol)
      }.toMap
      (segmentsMap, commonSegments)
    }

    private val zeroVec = architecture.zeroVec()

    protected def impl(label: Symbol, shouldDropout: () => Boolean): CompNode = {
      def dropoutImpl: CompNode =
        architecture.randomVar('segments / '?)

      def encodeSeg(seg: Segment): CompNode = {
        if(!commonSegments.contains(seg) && shouldDropout()) dropoutImpl
        else segmentsMap.getOrElse(seg, dropoutImpl)
      }

      segmentName(label)
        .map(encodeSeg)
        .pipe(totalSafe(_, zeroVec))
    }

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
