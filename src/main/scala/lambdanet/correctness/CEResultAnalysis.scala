package lambdanet.correctness

import ammonite.{ops => amm}
import cats.Monoid
import cats.implicits._
import lambdanet.SM
import lambdanet.correctness.CrossEntropyExperiment.Params
import lambdanet.correctness.CrossEntropyMethod.CEResult
import lambdanet.translation.PredicateGraph.{BinaryRel, DefineRel, PNode, TyPredicate}

object CEResultAnalysis {
  def main(args: Array[String]): Unit = {
    val outputPath = amm.pwd / "CE_Results" / args(0)
    val resultsPath =  outputPath / "ceResult.serialized"
    val param = SM.readObjectFromFile[Params]((outputPath / "params.serialized").toIO)
    val inputPath = amm.pwd / "data" / param.relPathUnderData
    val (graph, nodeAnnots, results) = InputUtils.loadGraphAndPredict(inputPath)
    val monoid = implicitly[Monoid[Map[PNode, Vector[TyPredicate]]]]
    val predicatesOf: Map[PNode, Vector[TyPredicate]] = graph.predicates.collect {
      case p: BinaryRel => p
      case p: DefineRel => p
    }.map { predicate =>
      predicate.allNodes.map(node => node -> Vector(predicate)).toMap
    }.fold(monoid.empty)(monoid.combine)

    val ceResult = SM.readObjectFromFile[CEResult[Assignment, TypeDistrs]](resultsPath.toIO)

    val groundTruth = GroundTruth(nodeAnnots, toPlainType = true)
    val groundTruthDifference: Assignment.Difference =
      Assignment.diff(results, groundTruth.truth, ceResult.elites.head)
    groundTruthDifference.diff.foreach { case (node, (gold, pred)) =>
      println(s"$node -> ${(gold, pred)}")
      predicatesOf.get(node).foreach(_.foreach(println))
      println
    }
  }
}
