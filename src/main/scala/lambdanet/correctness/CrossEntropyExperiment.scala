package lambdanet.correctness

import java.io.InvalidClassException
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import ammonite.ops.RelPath
import ammonite.{ops => amm}
import lambdanet.correctness.Objective.{AverageNegativeLogLikelihood, NegativeLogLikelihood}
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{DefineRel, PNode, PType}
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
      stopIters: Int,
      objectiveClass: String,
      generatorClass: String,
      updateClass: String,
      callbackClass: String
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
    graph.predicates.collect { case p: DefineRel => p }.foreach(println)
    println()

    val objectiveConstructor = params.objectiveClass match {
      case "lambdanet.correctness.Objective.NegativeLogLikelihood$" =>
        NegativeLogLikelihood
      case "lambdanet.correctness.Objective.AverageNegativeLogLikelihood$" =>
        AverageNegativeLogLikelihood
    }
    val objective =
      (distrs: Map[PNode, TopNDistribution[PType]]) =>
        objectiveConstructor(distrs)

    val parents: Map[PNode, Set[Either[PNode, PNode]]] =
      checker.subtypesToCheck.groupBy(_._1).mapValuesNow(_.map(x => Right(x._2)))
    val children: Map[PNode, Set[Either[PNode, PNode]]] =
      checker.subtypesToCheck.groupBy(_._2).mapValuesNow(_.map(x => Left(x._1)))
    val subtypingNodes = parents ++ children

    val sameNodesByAccess = Heuristics.accessNodesAreTheSame(checker.defaultContext.typeUnfold)
    val standaloneNodes = projectNodes.collect {
      case x if !sameNodesByAccess.flatten.contains(x) => Set(x)
    }
    val sameNodes = sameNodesByAccess ++ standaloneNodes
    println("======Same nodes======")
    sameNodes.filter(_.size > 1).foreach(println)

    val assignmentGen = params.generatorClass match {
      case "lambdanet.correctness.CrossEntropyTypeInference.AssignmentGen$" =>
        AssignmentGen(projectNodes, checker.defaultContext, subtypingNodes, sameNodes)
      case _ =>
        throw new InvalidClassException(
          params.generatorClass,
          "generator class is not supported"
        )
    }

    val updateTypeDistrs = params.updateClass match {
      case "lambdanet.correctness.CrossEntropyTypeInference.UpdateTypeDistrs$" =>
        UpdateTypeDistrs(params.smoothing)
      case _ =>
        throw new InvalidClassException(
          params.updateClass,
          "update class is not supported"
        )
    }

    val isConverged = params.callbackClass match {
      case "lambdanet.correctness.CrossEntropyTypeInference.IsConverged$" =>
        IsConverged(params.stopIters)
      case _ =>
        throw new InvalidClassException(
          params.callbackClass,
          "callback class is not supported"
        )
    }

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
    checker.violate(best).foreach(println)
    println("======Difference between ground truth and best sample======")
    val groundTruthDifference =
      Assignment.diff(results, groundTruth.truth, best)
    println(groundTruthDifference)
    println()

    val fmt = DateTimeFormatter.ofPattern("uuMMdd_HHmm")
    val currentTime = LocalDateTime.now().format(fmt)
    val outputPath = amm.pwd / "CE_Results" / currentTime
    OutputUtils.save(
      outputPath,
      "ceResult.serialized",
      ceResult.asInstanceOf[Serializable]
    )
    OutputUtils.save(
      outputPath,
      "params.serialized",
      params.asInstanceOf[Serializable]
    )
  }
}
