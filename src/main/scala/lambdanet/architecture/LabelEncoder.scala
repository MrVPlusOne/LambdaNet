package lambdanet.architecture

import botkop.numsca.Tensor
import funcdiff.SimpleMath.Coverage
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
      dropoutThreshold: Int,
      randomLabelId: () => Int
  ) extends LabelEncoder {
    def name: String = "TrainableLabelEncoder"

    private val (labelsMap, commonLabels) = {
      val (labels, _) =
        selectSegmentsBasedOnUsages(trainSet, nameUsages, coverageGoal)

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
      def dropoutImpl: CompNode = {
        val i = randomLabelId()
        architecture.randomUnitVar('segments / Symbol(s"?$i"))
      }
      if (!commonLabels.contains(label) && shouldDropout()) dropoutImpl
      else labelsMap.getOrElse(label, dropoutImpl)
    }

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
      symbolPath: SymbolPath,
      trainSet: Vector[Datum],
      coverageGoal: Double,
      architecture: NNArchitecture,
      dropoutProb: Real,
      dropoutThreshold: Int,
      randomLabelId: () => Int
  ) extends LabelEncoder {
    import cats.implicits._

    def name: String = "SegmentedLabelEncoder"

    private val (segmentsMap, commonSegments) = {
      val (segments, _) =
        selectSegmentsBasedOnUsages(trainSet, nameToSegUsages, coverageGoal)

      val commonSegments =
        segments.takeWhile(_._2 > dropoutThreshold).map(_._1).toSet
      val segmentsMap = segments.map {
        case (s, _) =>
          s -> architecture.randomUnitVar("segments" / s.symbol)
      }.toMap
      (segmentsMap, commonSegments)
    }

    protected def impl(
        label: Symbol,
        shouldDropout: () => Boolean
    ): CompNode = {
      def dropoutImpl: CompNode = {
        val i = randomLabelId()
        architecture.randomUnitVar('segments / Symbol(s"?$i"))
      }

      def encodeSeg(seg: Segment): CompNode = {
        if (!commonSegments.contains(seg) && shouldDropout()) dropoutImpl
        else segmentsMap.getOrElse(seg, dropoutImpl)
      }

      val segs = segmentName(label)
      if (segs.isEmpty) architecture.zeroVec()
      else
        segs
          .map(encodeSeg)
          .pipe(stackRows)  //todo: don't need to transform trainable segs
          .pipe(architecture.nonLinearLayer(symbolPath / 'encodeSeg))
          .pipe(sum(_, 0))
    }

  }

  case class Segment(symbol: Symbol)
  def segmentName(symbol: Symbol): Vector[Segment] = {
    def segmentOpt(s: String) = {
      val s1 = s.toLowerCase.replaceAll("[0-9]", "")
      if (s1.isEmpty) None
      else Some(Segment(Symbol(s1)))
    }

    def handleSpecial(segs: Vector[Segment]): Vector[Segment] = {
      if (segs.length >= 2 && segs.head == Segment('$)) {
        val symbol1 = Symbol(segs.head.symbol.name + segs(1).symbol.name)
        Segment(symbol1) +: segs.drop(2)
      } else segs
    }

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
        .toVector
        .flatMap(s => segmentOpt(s).toVector)
        .pipe(handleSpecial)
    }

    if (symbol.name.endsWith("Token")) Vector(Segment(symbol))
    else symbol.name.split("_+").toVector.flatMap(splitCamelCase)
  }

  def nameToSegUsages(name: Symbol): Map[Segment, Int] = {
    import cats.implicits._
    segmentName(name).foldMap(s => Map(s -> 1))
  }

  def selectSegmentsBasedOnUsages[Seg](
      dataSet: Vector[Datum],
      nameUsages: Symbol => Map[Seg, Int],
      coverageGoal: Double
  ): (Seq[(Seg, Int)], Coverage) = {
    import cats.implicits._

    val totalUsages = dataSet.foldMap { p =>
      val predsUsage = p.predictor.graph.predicates.toVector.collect {
        case DefineRel(_, expr) =>
          expr.allLabels.toVector.foldMap(nameUsages)
        case HasName(_, name) =>
          nameUsages(name)
      }.combineAll

      val annotsUsage = p.nodesToPredict.toVector.foldMap {
        case (_, t) => t.allLabels.toVector.foldMap(nameUsages)
      }

      Vector(predsUsage, annotsUsage).combineAll
    }

    val (segments, achieved) =
      SM.selectBasedOnFrequency(totalUsages.toSeq, coverageGoal)
    printResult(s"number of segments selected: ${segments.length}")
    printResult(s"coverage achieved: $achieved")
    printResult(s"Fist 100 segs: ${segments.take(100)}")
    (segments, achieved)
  }
}
