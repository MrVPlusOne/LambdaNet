package lambdanet.train

import lambdanet.architecture.LabelEncoder
import lambdanet.architecture.LabelEncoder.{Segment, SegmentedLabelEncoder}
import lambdanet.train
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{PNode, PTyVar, ProjNode}

object NamingBaseline {

  type Name = Vector[Segment]
  type Stats = Map[(LabelCat.Value, String), Counted[LibCorrect]]

  def test(dataSet: DataSet) = {
    import dataSet.{testSet, trainSet}
    import LabelEncoder.{selectSegmentsBasedOnUsages, nameToSegUsages}

    val (segs, _) = selectSegmentsBasedOnUsages(trainSet, nameToSegUsages, coverageGoal = 0.98)
    val segSet = segs.map(_._1).toSet
    def transformName(name: Name): Name = {
      name.filter(segSet.contains)
    }

    val stats: Stats = {
      import cats.implicits._
      testSet.par
        .map(
          datum =>
            testOnDatum(datum, useOracle = false, transformName).result |+|
              testOnDatum(datum, useOracle = true, transformName).result
        )
        .seq
        .combineAll
    }
    println("Naive segment baseline:")
    stats.toVector.sortBy(_._1).foreach {
      case ((cat, name), count) =>
        println(s"$cat-$name: \t${toAccuracy(count)}")
    }
  }

  case class testOnDatum(datum: Datum, useOracle: Boolean, transformName: Name => Name) {

    def result: Stats = {
      import cats.implicits._

      val predSpace = datum.predictor.predictionSpace
      val allCands = predSpace.allTypes.toVector
        .map { ty =>
          ty -> typeName(ty)
        }
      val libCands = allCands.filter(_._1.madeFromLibTypes)
      val projCands = allCands.filterNot(_._1.madeFromLibTypes)
      datum.nodesToPredict.toVector.foldMap {
        case (n, label) =>
          val name = nodeName(n.n)
          val candidates =
            if (useOracle)
              if (label.madeFromLibTypes) libCands else projCands
            else allCands
          val truthPosition = candidates
            .map { case (ty, n1) => ty -> nameSimilarity(name, n1) }
            .sortBy(-_._2)
            .indexWhere { case (ty, _) => ty == label }
          assert(truthPosition >= 0)
          val cat = LabelCat.fromLib(label.madeFromLibTypes)
          val oracleFlag = if (useOracle) "*" else ""
          Map(
            (cat, s"top1$oracleFlag") -> Counted.fromBool(truthPosition < 1),
            (cat, s"top5$oracleFlag") -> Counted.fromBool(truthPosition < 5)
          )
      }
    }

    def nodeName(n: PNode): Name = {
      n.nameOpt.toVector.flatMap(name =>
        LabelEncoder.segmentName(name)
      ).pipe(transformName)
    }

    def typeName(ty: PredicateGraph.PType): Name = {
      ty match {
        case PTyVar(n) => nodeName(n).pipe(transformName)
        case _         => Vector()
      }
    }

    def nameSimilarity(n1: Name, n2: Name): Double = {
      n1.toSet.intersect(n2.toSet).size
    }

  }

}
