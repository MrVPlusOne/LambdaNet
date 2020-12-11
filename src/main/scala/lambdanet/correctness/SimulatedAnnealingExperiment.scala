package lambdanet.correctness

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import ammonite.ops.RelPath
import ammonite.{ops => amm}
import lambdanet.correctness.SimulatedAnnealing.IntermediateValues
import lambdanet.translation.PredicateGraphLoader.libDefs
import plots.{CommonOptions, PlotlyBackend}

import scala.util.Random

object SimulatedAnnealingExperiment {
  case class Parameters(
    seed: Option[Long],
    schedule: Schedule,
    numEpochs: Int,
    numSamples: Int
  )

  def run(relPathUnderData: RelPath, unseededParams: Parameters): Unit = {
    val params = unseededParams.copy(
      seed = Some(unseededParams.seed.getOrElse(Random.nextLong()))
    )
    Random.setSeed(params.seed.get)
    val inputPath = amm.pwd / "data" / relPathUnderData
    val outputPath = amm.pwd / "SA_results"
    val (graph, results) = InputUtils.loadGraphAndPredict(inputPath)
    val checker = TypeChecker(graph, libDefs)
    val schedule = params.schedule

    val fmt = DateTimeFormatter.ofPattern("uuMMdd_HHmm")

    val criterion = AverageNegativeLogLikelihood(results)

    // TODO: fix this by providing an encoder
    // amm.write(outputPath / s"$currentTime.json", params.asJson.toString())
    val mostLikely = results.map { case (k, v) => (k, v.distr(0)._2) }
    println(criterion.prob(mostLikely))
    checker.violate(mostLikely).foreach(println)
    //    val pNodes = results.keySet.map(x => (x.getId, x)).toMap
    //    val pTypes = results.values.flatMap(_.distr.map(_._2)).collect{ case (x: PTyVar) => x }.toSet.map((x: PTyVar) => (x.node.nameOpt.map(_.name).getOrElse(x.node.getId.toString), x)).toMap
    val maybeGroundTruth = InputUtils.loadGroundTruth(inputPath)
    if (maybeGroundTruth.nonEmpty) {
      val groundTruth = maybeGroundTruth.get
      println("======Difference between ground truth and most likely assignment======")
      println(SimulatedAnnealing.diff(groundTruth, mostLikely))
      println(s"Average NLL of ground truth: ${criterion.prob(groundTruth)}")
    }

    val runs =
      Iterator.fill(params.numSamples) {
        SimulatedAnnealing.searchWithLog(
          graph,
          results,
          // TODO: store these two in parameters as well
          WrongFirstOneDifferenceRandomNeighbor(results, checker).randomNeighbor,
          PatchAnyCorrection(checker, results).correct,
          schedule,
          numEpochs = params.numEpochs,
          f = criterion.prob
          , reboot = false
        )
      }.reduce { (a, b) =>
        val (pred1, x) = a
        val (pred2, y) = b
        val best = if (criterion.prob(pred1) < criterion.prob(pred2)) pred1 else pred2
        val combined = IntermediateValues(x.epochs, (x.ys, y.ys).zipped.map(_ + _), (x.bestYs, y.bestYs).zipped.map(_ + _), (x.nodeAccuracy, y.nodeAccuracy).zipped.map(_ + _), (x.constraintAccuracy, y.constraintAccuracy).zipped.map(_ + _))
        (best, combined)
      }
    val (best, sumLogValues) = runs
    if (maybeGroundTruth.nonEmpty) {
      println("======Difference between ground truth and final result======")
      println(SimulatedAnnealing.diff(maybeGroundTruth.get, best))
    }
    println(s"Average NLL of final result: ${criterion.prob(best)}")
    val averageLogValues: IntermediateValues = sumLogValues.copy(ys = sumLogValues.ys.map(_ / params.numSamples), bestYs = sumLogValues.bestYs.map(_ / params.numSamples))
    if (!amm.exists(outputPath)) {
      amm.mkdir(outputPath)
    }
    val plotly = PlotlyBackend()
    val ys = averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.ys)
    val bestYs = averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.bestYs)
    val nodeAccuracy = averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.nodeAccuracy)
    val likelihoodPlot = plotly.linePlot(
      Seq(ys, bestYs),
      CommonOptions(
        plotName = Some("Negative log-likelihoods over time on simple"),
        axesNames =
          (Some("Epoch"), Some("Negative log-likelihood (less is better)")),
        legends = Some(Seq("current", "best"))
      )
    )
    val accuracyPlot = plotly.linePlot(
      Seq(nodeAccuracy),
      CommonOptions(
        plotName = Some("Accuracy"),
        axesNames =
          (Some("Epoch"), Some("Accuracy")),
        legends = Some(Seq("node"))
      )
    )
    val currentTime = LocalDateTime.now().format(fmt)
    plotly.save(plotly.column(likelihoodPlot, accuracyPlot), outputPath / s"$currentTime.html", "Simulated Annealing")
  }

  def main(args: Array[String]): Unit = {
    val params = Parameters(None, LogSchedule(0.1), numEpochs = 100000, numSamples = 10)
    val inputPath = RelPath("tests/c1")
    run(inputPath, params)
  }
}
