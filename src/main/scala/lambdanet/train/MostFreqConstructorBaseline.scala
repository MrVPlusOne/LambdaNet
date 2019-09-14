package lambdanet.train

import lambdanet._
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{PNode, PTyVar, PType, ProjNode}

object MostFreqConstructorBaseline {

  type Stats = Map[String, Counted[Correct]]

  def test(dataSet: DataSet, useByFreq: Boolean) = {
    import dataSet.testSet
    import cats.implicits._

    val stats: Stats = testSet.par
      .map { datum =>
        useDatum(datum)
          .pipe(x => if (useByFreq) x.predictByFreq else x.predictByAssignments)
          .toVector
          .foldMap {
            case (_, (pos, _)) =>
              Map(
                "top1" -> Counted.fromBool(pos < 1),
                "top5" -> Counted.fromBool(pos < 5)
              )
          }
      }
      .seq
      .combineAll

    if (useByFreq)
      println("Most frequent constructor baseline: ")
    else
      println("Constructor assignment baseline: ")
    stats.toVector.sortBy(_._1).foreach {
      case (name, count) =>
        println(s"$name: \t${toAccuracy(count)}")
    }
  }

  case class useDatum(datum: Datum) {
    import cats.implicits._

    def predictByFreq: Map[ProjNode, (TruthPosition, PType)] = {
      val allProjTypes = datum.predictor.predictionSpace.projTypeVec
      val sortedTypes = allProjTypes.sortBy(-constructorFreqs(_))
      val typeSet = sortedTypes.toSet
      datum.nodesToPredict.collect {
        case (n, label) if typeSet.contains(label) =>
          val pos = sortedTypes.indexOf(label)
          assert(pos >= 0)
          n -> (pos, label)
      }
    }

    def predictByAssignments: Map[ProjNode, (TruthPosition, PType)] = {
      datum.nodesToPredict.collect {
        case (n, label) if !label.madeFromLibTypes =>
          val pos = constructorAssignments(n.n).toVector
            .indexOf(label)
            .pipe(x => if (x < 0) Int.MaxValue else x)
          assert(pos >= 0)
          n -> (pos, label)
      }
    }

    import PredicateGraph.BinaryRel
    import PredicateGraph.BinaryRelCat

    private def allPreds = datum.predictor.graph.predicates.toVector

    val constructorFreqs: PType => Int = {
      val map = allPreds.collect {
        case BinaryRel(_, rhs, BinaryRelCat.fixType) if rhs.isType =>
          Map((PTyVar(rhs): PType) -> 1)
      }.combineAll
      (t: PType) => map.getOrElse(t, 0)
    }

    val constructorAssignments: PNode => Set[PTyVar] = {
      val map = allPreds.collect {
        case BinaryRel(n, rhs, BinaryRelCat.fixType) if rhs.isType =>
          Map(n -> Set(PTyVar(rhs)))
      }.combineAll

      (n: PNode) => map.getOrElse(n, Set())
    }
  }

}
