package lambdanet.correctness

import ammonite.ops.RelPath
import ammonite.{ops => amm}
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PNode, PType}
import lambdanet.translation.PredicateGraphLoader.libDefs

import scala.util.Random

object CrossEntropyExperiment {
  case class Params(
      relPathUnderData: RelPath,
      seed: Option[Long],
      numSamples: Int,
      numElites: Int,
      maxIters: Int,
      smoothing: Double,
      stopIters: Int
  )

  def run(unseededParams: Params): Unit = {
    import CrossEntropyTypeInference._
    val params = unseededParams.copy(
      seed = Some(unseededParams.seed.getOrElse(Random.nextLong()))
    )
    Random.setSeed(params.seed.get)
    val inputPath = amm.pwd / "data" / params.relPathUnderData
    val (graph, nodeAnnots, results) = InputUtils.loadGraphAndPredict(inputPath)
    val checker = TypeChecker(graph, libDefs)
    val projectNodes = graph.nodes.filter(_.fromProject)
    checker.subtypesToCheck.foreach(println)
    checker.binaryRels.foreach(println)
    println()

    val objective =
      (distrs: Map[PNode, TopNDistribution[PType]]) =>
        Objective.NegativeLogLikelihood(distrs).prob(_)

    val parents =
      checker.subtypesToCheck.groupBy(_._1).mapValuesNow(_.map(_._2))
    val children =
      checker.subtypesToCheck.groupBy(_._2).mapValuesNow(_.map(_._1))
    println(children)
    val assignmentGen =
      AssignmentGen(projectNodes, checker.defaultContext, parents, children)

    val updateTypeDistrs = UpdateTypeDistrs(params.smoothing)
    val isConverged = IsConverged(params.stopIters)

    val ceResult = CrossEntropyMethod.ceMinimize(
      objective,
      results,
      assignmentGen,
      updateTypeDistrs,
      params.numSamples,
      params.numElites,
      isConverged,
      params.maxIters
    )
    if (ceResult.converged) {
      println("Successfully converged")
    } else {
      println("Failed to converge")
    }
    println()

    val best = ceResult.elites.head
    ceResult.param.foreach(println)
    val groundTruth = GroundTruth(nodeAnnots, toPlainType = true)
    best.foreach(println)
    println("Violated constraints:")
    println("======Difference between ground truth and best sample======")
    val groundTruthDifference =
      Assignment.diff(results, groundTruth.truth, best)
    println(groundTruthDifference)
    checker.violate(best).foreach(println)
    println()
  }
}
