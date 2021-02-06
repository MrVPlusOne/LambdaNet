package lambdanet.correctness

import ammonite.ops.RelPath
import ammonite.{ops => amm}
import lambdanet.correctness.CrossEntropyTypeInference.{AssignmentGen, IsConverged, UpdateTypeDistrs}
import lambdanet.correctness.Objective.{AverageNegativeLogLikelihood, HammingLoss, NegativeLogLikelihood}
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{BinaryRel, BinaryRelCat}
import lambdanet.translation.PredicateGraphLoader.libDefs

import scala.util.Random

object BeamSearchExperiment {
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

  def run(): Unit = {
    val unseededParams = Params(
      RelPath("tests/public"),
      seed = None,
      numSamples = 1000,
      numElites = 1000,
      maxIters = 1,
      smoothing = 0.7,
      stopIters = 20,
      objectiveClass = Objective.AverageNegativeLogLikelihood.getClass.getCanonicalName,
      generatorClass = AssignmentGen.getClass.getCanonicalName,
      updateClass = UpdateTypeDistrs.getClass.getCanonicalName,
      callbackClass = IsConverged.getClass.getCanonicalName
    )
    import CrossEntropyTypeInference._
    val params = unseededParams.copy(
      seed = Some(unseededParams.seed.getOrElse(Random.nextLong()))
    )
    Random.setSeed(params.seed.get)
    val inputPath = amm.pwd / "data" / params.relPathUnderData
    val (graph, nodeAnnots, results) = InputUtils.loadGraphAndPredict(inputPath)
    val checker = TypeChecker(graph, libDefs)
    val projectNodes = graph.nodes.filter(_.fromProject)
//    checker.subtypesToCheck.foreach(println)
//    checker.binaryRels.foreach(println)
//    graph.predicates.collect { case p: DefineRel => p }.foreach(println)
    println()

    val objectiveConstructor = params.objectiveClass match {
      case "lambdanet.correctness.Objective.NegativeLogLikelihood$" =>
        NegativeLogLikelihood
      case "lambdanet.correctness.Objective.AverageNegativeLogLikelihood$" =>
        AverageNegativeLogLikelihood
      case "lambdanet.correctness.Objective.HammingLoss$" =>
        HammingLoss
    }
    val objective = objectiveConstructor(results)

    val allPTypes: Set[PredicateGraph.PType] =
      results.flatMap(_._2.distr.map(_._2))(collection.breakOut)
    val shallowSubtype = ShallowSubtype(checker, allPTypes)

    val fixedTypes =
      Heuristics.fixTypesByAccess(
        checker.defaultContext.typeUnfold,
        libDefs.nodeMapping
      ) ++ Heuristics.fixTypesByFixType(graph.predicates.collect {
        case p @ BinaryRel(_, _, BinaryRelCat.fixType) => p
      })
//    println("======Fixed types======")
//    fixedTypes.foreach(println)
    val sameNodesByAccess =
      Heuristics.accessNodesAreTheSame(checker.defaultContext.typeUnfold)
    val standaloneNodes = projectNodes.collect {
      case x if !sameNodesByAccess.flatten.contains(x) => Set(x)
    }
    val sameNodes = sameNodesByAccess ++ standaloneNodes
//    println("======Same nodes======")
//    sameNodes.filter(_.size > 1).foreach(println)

    val validTypes =
      Heuristics.validTypesWithAnyAssignment(results, sameNodes, checker)
//    validTypes.foreach(println)

    val beamSearch = new BeamSearch(
      shallowSubtype,
      sameNodes.toArray,
      validTypes.mapValuesNow(_.toArray),
      fixedTypes,
      objective
    )
    val elites = beamSearch.search(results, 5)

    val groundTruth = GroundTruth(nodeAnnots, toPlainType = true)
    val accuracy = Accuracy(groundTruth)
    elites.foreach {
      best =>
        println(Assignment.diff(results, groundTruth.truth, best))
        println
    }
  }

  def main(args: Array[String]): Unit = {
    run()
  }
}