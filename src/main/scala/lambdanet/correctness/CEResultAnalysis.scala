package lambdanet.correctness

import ammonite.{ops => amm}
import cats.Monoid
import cats.implicits._
import lambdanet.SM
import lambdanet.correctness.CrossEntropyExperiment.Params
import lambdanet.correctness.CrossEntropyMethod.CEResult
import lambdanet.translation.PredicateGraph._
import lambdanet.translation.PredicateGraphLoader.libDefs

object CEResultAnalysis {
  def main(args: Array[String]): Unit = {
    val outputPath = amm.pwd / "CE_Results" / args(0)
    val resultsPath = outputPath / "ceResult.serialized"
    val param =
      SM.readObjectFromFile[Params]((outputPath / "params.serialized").toIO)
    val inputPath = amm.pwd / "data" / param.relPathUnderData
    val (graph, nodeAnnots, results) = InputUtils.loadGraphAndPredict(inputPath)
    val monoid = implicitly[Monoid[Map[PNode, Vector[TyPredicate]]]]
    val predicates = graph.predicates.collect {
      case p: BinaryRel => p
      case p: DefineRel => p
    }
    val predicatesOf: Map[PNode, Vector[TyPredicate]] =
      predicates
        .map { predicate =>
          predicate.allNodes.map(node => node -> Vector(predicate)).toMap
        }
        .fold(monoid.empty)(monoid.combine)

    val ceResult = {
      SM.readObjectFromFile[CEResult[Assignment, TypeDistrs]](resultsPath.toIO)
    }
    println(ceResult.param.values.map(_.topValue).count(_ == PAny))
    val checker = TypeChecker(graph, libDefs)
    val bad =
      ceResult.elites.iterator
        .map(x => (x, checker.violate(x)))
        .filter(_._2.nonEmpty)
        .take(5)
    bad.foreach {
      case (assignment, set) =>
        val relatedNodes: Set[PNode] = set.flatMap { case (a, b) => Set(a, b) }
        println("Assignment:")
        assignment.filterKeys(relatedNodes.contains).foreach(println)
        println("Violated constraints:")
        set.foreach {
          case (a, b) =>
            checker.defaultContext.isSubtype(a, b, assignment)
            println(a, b)
        }

        println
    }

    val groundTruth = GroundTruth(nodeAnnots, toPlainType = true)
    val groundTruthDifference: Assignment.Difference =
      Assignment.diff(results, groundTruth.truth, ceResult.elites.head)
    groundTruthDifference.diff.toArray.sortBy(_._1.getId).foreach {
      case (node, (gold, pred)) =>
        println(s"$node -> ${(gold, pred)}")
        predicatesOf.get(node).foreach(_.foreach(println))
        println
    }
  }
}
