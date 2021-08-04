package lambdanet.architecture

import botkop.numsca.Tensor
import funcdiff.SimpleMath.Coverage
import funcdiff._
import lambdanet._
import lambdanet.train.ProcessedProject
import lambdanet.translation.PredicateGraph._
import lambdanet.translation.QLang

import collection.concurrent.TrieMap
import scala.util.Random

trait LabelEncoder {
  def name: String

  protected def dropoutProb: Real

  private val random = new Random()

  def newEncoder(useDropout: Boolean)(implicit mode: GraphMode): Symbol => CompNode = {
    val map = new TrieMap[Symbol, CompNode]()

    def shouldDropout(): Boolean =
      if (useDropout) random.synchronized {
        random.nextDouble() < dropoutProb
      } else false

    (l: Symbol) => {
      map.getOrElseUpdate(l, impl(l, shouldDropout))
    }
  }

  protected def impl(
      label: Symbol,
      shouldDropout: () => Boolean
  )(implicit mode: GraphMode): CompNode
}

object LabelEncoder {

  case class RandomLabelEncoder(architecture: NNArchitecture) extends LabelEncoder {
    def name: String = "RandomLabelEncoder"

    def dropoutProb: Real = 0.0

    protected def impl(
        label: Symbol,
        shouldDropout: () => Boolean
    )(implicit mode: GraphMode): CompNode = {
      const(architecture.randomUnitVec())

    }
  }

  case class ConstantLabelEncoder(architecture: NNArchitecture) extends LabelEncoder {
    def name: String = "ConstLabelEncoder"

    private val zeroVec: Tensor = architecture.zeroVec()

    protected def dropoutProb: Real = 0.0

    protected def impl(
        label: Symbol,
        shouldDropout: () => Boolean
    )(implicit mode: GraphMode): CompNode = {
      zeroVec
    }
  }

  object TrainableLabelEncoder {

    /**
      * @param dropoutThreshold if a label appears more than this number of times
      *                         in the training set, it won't be dropped out during
      *                         training time.
      */
    def fromData(
        trainSet: Vector[ProcessedProject],
        coverageGoal: Double,
        architecture: NNArchitecture,
        dropoutProb: Real,
        dropoutThreshold: Int,
        labelTransformation: Symbol => Symbol,
        randomLabelId: () => Int
    ): TrainableLabelEncoder = {
      def nameUsages(name: Symbol): Map[Symbol, Int] = {
        Map(labelTransformation(name) -> 1)
      }

      val (labelsMap: Map[Symbol, CompNode], commonLabels: Set[Symbol]) = {
        val (labels, _) =
          selectSegmentsBasedOnUsages(trainSet, nameUsages, coverageGoal)

        labels.map {
          case (s, _) =>
            s -> architecture.randomVar('label / s)
        }.toMap -> labels.collect {
          case (l, freq) if freq > dropoutThreshold => l
        }.toSet
      }

      TrainableLabelEncoder(
        architecture,
        dropoutProb,
        labelTransformation,
        randomLabelId,
        labelsMap,
        commonLabels
      )
    }
  }

  @SerialVersionUID(1086019288003381497L)
  case class TrainableLabelEncoder(
      architecture: NNArchitecture,
      dropoutProb: Real,
      labelTransformation: Symbol => Symbol,
      randomLabelId: () => Int,
      labelsMap: Map[Symbol, CompNode],
      commonLabels: Set[Symbol]
  ) extends LabelEncoder {
    def name: String = "TrainableLabelEncoder"

    def isLibLabel(label: Symbol): Boolean = labelsMap.contains(label)

    protected def impl(
        label0: Symbol,
        shouldDropout: () => Boolean
    )(implicit mode: GraphMode): CompNode = {
      def dropoutImpl: CompNode = {
        val i = randomLabelId()
        architecture.randomUnitVar('label / Symbol(s"?$i"))
      }
      val label = labelTransformation(label0)
      if (!commonLabels.contains(label) && shouldDropout()) dropoutImpl
      else labelsMap.getOrElse(label, dropoutImpl)
    }

  }

  object SegmentedLabelEncoder {

    /**
      * @param dropoutThreshold if a segment appears more than this number of times
      *                         in the training set, it won't be dropped out during
      *                         training time.
      */
    def fromData(
        symbolPath: SymbolPath,
        trainSet: Vector[ProcessedProject],
        coverageGoal: Double,
        architecture: NNArchitecture,
        dropoutProb: Real,
        dropoutThreshold: Int,
        randomLabelId: () => Int
    ): SegmentedLabelEncoder = {

      val (segmentsMap, commonSegments) = {
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

      SegmentedLabelEncoder(
        symbolPath,
        architecture,
        dropoutProb,
        randomLabelId,
        segmentsMap,
        commonSegments
      )
    }
  }

  @SerialVersionUID(7296992186902228726L)
  case class SegmentedLabelEncoder(
      symbolPath: SymbolPath,
      architecture: NNArchitecture,
      dropoutProb: Real,
      randomLabelId: () => Int,
      segmentsMap: Map[Segment, CompNode],
      commonSegments: Set[Segment]
  ) extends LabelEncoder {

    def name: String = "SegmentedLabelEncoder"

    protected def impl(
        label: Symbol,
        shouldDropout: () => Boolean
    )(implicit mode: GraphMode): CompNode = {
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
          .pipe(stackRows) //todo: don't need to transform trainable segs
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
      dataSet: Vector[ProcessedProject],
      nameUsages: Symbol => Map[Seg, Int],
      coverageGoal: Double
  ): (Seq[(Seg, Int)], Coverage) = {
    import cats.implicits._

    val totalUsages = dataSet.foldMap { p =>
      val predsUsage = p.graph.predicates.toVector.collect {
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
    printResult(s"Fist 10 segs: ${segments.take(10)}")
    printResult(s"Last 10 segs: ${segments.takeRight(10)}")
    (segments, achieved)
  }
}
