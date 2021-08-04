package lambdanet.train

import lambdanet.architecture.LabelEncoder
import lambdanet.architecture.LabelEncoder.{Segment, SegmentedLabelEncoder}
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{PNode, PTyVar, PType, ProjNode}

object NamingBaseline {

  type Name = Vector[Segment]
  type Stats = Map[(LabelCat.Value, String), Counted[Correct]]

  def test(dataSet: DataSet) = {
    import dataSet.{testSet, trainSet}
    import LabelEncoder.{selectSegmentsBasedOnUsages, nameToSegUsages}

    val (segs, _) = selectSegmentsBasedOnUsages(
      trainSet,
      nameToSegUsages,
      coverageGoal = 0.98
    )
    val segSet = segs.map(_._1).toSet
    def transformName(name: Name): Name = {
      name.filter(segSet.contains)
    }

    val stats: Stats = {
      import cats.implicits._
      testSet.par
        .map(
          datum => testOnDatum(datum, useOracle = false, transformName).result //|+|
          //testOnDatum(datum, useOracle = true, transformName).result
        )
        .seq
        .combineAll
    }
    println("Naive segment baseline:")
    stats.toVector.sortBy(_._1).foreach {
      case ((cat, name), count) =>
        println(s"$cat-$name (${count.count}): \t${toAccuracy(count)}")
    }
  }

  def nodeName(n: PNode): Name = {
    n.nameOpt.toVector.flatMap(name => LabelEncoder.segmentName(name))
  }

  def typeName(ty: PredicateGraph.PType): Name = {
    ty match {
      case PTyVar(n) => nodeName(n)
      case _         => Vector()
    }
  }

  def nameSimilarity(n1: Name, n2: Name): Int = {
    n1.toSet.intersect(n2.toSet).size
  }

  def nameSimilarity(n1: Symbol, n2: Symbol): Int = {
    nameSimilarity(LabelEncoder.segmentName(n1), LabelEncoder.segmentName(n2))
  }

  case class testOnDatum(
      datum: ProcessedProject,
      useOracle: Boolean,
      transformName: Name => Name
  ) {

    def predict(threshold: Double): Map[ProjNode, (TruthPosition, PType)] = {
      import cats.implicits._

      val predSpace = datum.predictionSpace
      val allCands = predSpace.allTypes.toVector
        .map { ty =>
          ty -> transformName(typeName(ty))
        }
      val libCands = allCands.filter(_._1.madeFromLibTypes)
      val projCands = allCands.filterNot(_._1.madeFromLibTypes)
      val predictions = datum.nodesToPredict.toVector.flatMap {
        case (n, label) =>
          val name = transformName(nodeName(n.n))
          val candidates =
            if (useOracle)
              if (label.madeFromLibTypes) libCands else projCands
            else allCands
          val truthPosition = candidates
            .map { case (ty, n1) => ty -> nameSimilarity(name, n1) }
            .filter { case (_, score) => score >= threshold }
            .sortBy(-_._2)
            .indexWhere { case (ty, _) => ty == label }
          if (truthPosition < 0) {
            Vector()
          } else Vector(n -> (truthPosition, label))
      }.toMap
      predictions
    }

    def result: Stats = {
      import cats.implicits._

      predict(0.0).toVector.foldMap {
        case (_, (truthPosition, label)) =>
          val cat = LabelCat.fromLib(label.madeFromLibTypes)
          val oracleFlag = if (useOracle) "*" else ""
          Map(
            (cat, s"top1$oracleFlag") -> Counted.fromBool(truthPosition < 1),
            (cat, s"top5$oracleFlag") -> Counted.fromBool(truthPosition < 5)
          )
      }
    }
  }

}
