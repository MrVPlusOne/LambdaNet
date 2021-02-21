package lambdanet.correctness

//import $ivy.`org.plotly-scala::plotly-render:0.5.4`
import ammonite.ops.RelPath
import ammonite.{ops => amm}
import lambdanet.LibDefs
import lambdanet.correctness.Objective.{AverageNegativeLogLikelihood, HammingLoss, NegativeLogLikelihood}
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph._
import lambdanet.translation.PredicateGraphLoader.libDefs
import plotly.Plotly._
import plotly._
import plotly.element._
import plotly.layout.{Axis, Layout}

import java.io.{BufferedOutputStream, FileOutputStream, InvalidClassException, PrintStream}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
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

  def run(unseededParams: Params, defaultLibDefs: Option[LibDefs] = None): Unit = {
    import CrossEntropyTypeInference._
    val params = unseededParams.copy(
      seed = Some(unseededParams.seed.getOrElse(Random.nextLong()))
    )
    Random.setSeed(params.seed.get)
    val inputPath = amm.pwd / "data" / params.relPathUnderData
    val (graph, nodeAnnots, results) = InputUtils.loadGraphAndPredict(inputPath)

    val fmt = DateTimeFormatter.ofPattern("uuMMdd_HHmm")
    val currentTime = LocalDateTime.now().format(fmt)
    val outputPath = amm.pwd / "BS_Results" / currentTime
    if (!amm.exists(outputPath)) {
      amm.mkdir(outputPath)
    }
    System.setOut(
      new PrintStream(
        new BufferedOutputStream(new FileOutputStream((outputPath / "output.txt").toIO))
      )
    )

    val checker = TypeChecker(graph, defaultLibDefs.getOrElse(libDefs))
    val projectNodes = graph.nodes.filter(_.fromProject)
//    checker.subtypesToCheck.foreach(println)
//    checker.binaryRels.foreach(println)
//    graph.predicates.collect { case p: DefineRel => p }.foreach(println)

    val objectiveConstructor = params.objectiveClass match {
      case "lambdanet.correctness.Objective.NegativeLogLikelihood$" =>
        NegativeLogLikelihood
      case "lambdanet.correctness.Objective.AverageNegativeLogLikelihood$" =>
        AverageNegativeLogLikelihood
      case "lambdanet.correctness.Objective.HammingLoss$" =>
        HammingLoss
    }
    val objective = objectiveConstructor(results)

    val allPTypes: Set[PredicateGraph.PType] = results.flatMap(_._2.distr.map(_._2))(collection.breakOut)
    val shallowSubtype = ShallowSubtype(checker, allPTypes)

    val fixedTypes =
      Heuristics.fixTypesByAccess(
        checker.defaultContext.typeUnfold,
        libDefs.nodeMapping
      ) ++ Heuristics.fixTypesByFixType(graph.predicates.collect {
        case p @ BinaryRel(_, _, BinaryRelCat.fixType) => p
      })
    println("======Fixed types======")
    fixedTypes.foreach(println)
    val sameNodesByAccess =
      Heuristics.accessNodesAreTheSame(checker.defaultContext.typeUnfold)
    val standaloneNodes = projectNodes.collect {
      case x if !sameNodesByAccess.flatten.contains(x) => Set(x)
    }
    val sameNodes = sameNodesByAccess ++ standaloneNodes
    println("======Same nodes======")
    sameNodes.filter(_.size > 1).foreach(println)

    val validTypes =
      Heuristics.validTypesWithAnyAssignment(results, sameNodes, checker)
    validTypes.foreach(println)

    val assignmentGen = params.generatorClass match {
      case "lambdanet.correctness.CrossEntropyTypeInference.AssignmentGen$" =>
        AssignmentGen(
          projectNodes,
          shallowSubtype,
          sameNodes,
          validTypes,
          fixedTypes
        )
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

    val groundTruth = GroundTruth(nodeAnnots, toPlainType = true)
    val accuracy = Accuracy(groundTruth)
    val sampleAccuracy = new SampleAccuracy(params.maxIters, accuracy)
    val sampleScore = new SampleScore(params.maxIters)
    val metrics = Seq(sampleAccuracy, sampleScore)

    val ceResult = CrossEntropyMethod.ceMinimize(
      objective,
      results,
      assignmentGen,
      updateTypeDistrs,
      params.numSamples,
      params.numElites,
      isConverged,
      params.maxIters,
      metrics
    )
    if (ceResult.converged) {
      println("Successfully converged")
    } else {
      println("Failed to converge")
    }
    println()

    val best = ceResult.elites.head
    val meanAccuracy = sampleAccuracy.mean.last
    println(s"Average accuracy: $meanAccuracy")
    best.foreach(println)
    println("Violated constraints:")
    checker.violate(best).foreach(println)
    println("======Difference between ground truth and best sample======")
    val groundTruthDifference: Assignment.Difference =
      Assignment.diff(results, groundTruth.truth, best)
    println(s"${groundTruthDifference.diff.size} differences found")
    println(s"${groundTruthDifference.diff
      .count { case (node, (gold, _)) => results(node).typeProb.contains(gold) }} differences remain after filtering unpredicted types")
    println(groundTruthDifference)
    println()

    //noinspection ScalaDeprecation
    val scorePlot = Scatter(
      sampleScore.epochs,
      sampleScore.mean,
      error_y = Error.Data(array = sampleScore.stdev, visible = true),
      name = params.objectiveClass.split('.').last.stripSuffix("$"),
      mode = ScatterMode(ScatterMode.Lines)
    )
    //noinspection ScalaDeprecation
    val accuracyPlot = Scatter(
      sampleAccuracy.epochs,
      sampleAccuracy.mean,
      error_y = Error.Data(array = sampleAccuracy.stdev, visible = true),
      name = "Accuracy",
      yaxis = AxisReference.Y2
    )
    val plots = Seq(accuracyPlot, scorePlot)
    val path = outputPath / "plots.html"
    if (!amm.exists(outputPath)) {
      amm.mkdir(outputPath)
    }
    //noinspection ScalaDeprecation
    plots.plot(
      path.toString(),
      Layout(
        title = "Cross entropy results",
        height = 900,
        yaxis = Axis(
          anchor = AxisAnchor.Reference(AxisReference.Y1),
          domain = (0.52, 1)
        ),
        yaxis2 = Axis(
          anchor = AxisAnchor.Reference(AxisReference.Y2),
          domain = (0, 0.48)
        )
      )
    )

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
    OutputUtils.save(
      outputPath,
      "accuracy.serialized",
      sampleAccuracy.asInstanceOf[Serializable]
    )
    OutputUtils.save(
      outputPath,
      "score.serialized",
      sampleScore.asInstanceOf[Serializable]
    )
  }
}
