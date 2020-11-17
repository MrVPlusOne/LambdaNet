package lambdanet.correctness

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import ammonite.ops.RelPath
import ammonite.{ops => amm}
import lambdanet.translation.PredicateGraphLoader.libDefs
import plots.{CommonOptions, PlotlyBackend}

import scala.util.Random

object SimulatedAnnealingExperiment {
  case class Parameters(
    seed: Option[Long],
    schedule: Schedule,
    numEpochs: Int
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
    val currentTime = LocalDateTime.now().format(fmt)

    // TODO: fix this by providing an encoder
    // amm.write(outputPath / s"$currentTime.json", params.asJson.toString())

    val (correctPrediction, logValues) =
      SimulatedAnnealing.searchWithLog(
        graph,
        results,
        // TODO: store these two in parameters as well
        OneDifferenceRandomNeighbor(results).randomNeighbor,
        PatchAnyCorrection(checker, results).correct,
        schedule,
        numEpochs = params.numEpochs,
        f = NegativeLogLikelihood(results).prob
      )
    if (!amm.exists(outputPath)) {
      amm.mkdir(outputPath)
    }
    val plotly = PlotlyBackend()
    val ys = logValues.epochs.map(_.toDouble).zip(logValues.ys)
    val bestYs = logValues.epochs.map(_.toDouble).zip(logValues.bestYs)
    val plot = plotly.linePlot(
      Seq(ys, bestYs),
      CommonOptions(
        plotName = Some("Negative log-likelihoods over time on simple"),
        axesNames =
          (Some("Epoch"), Some("Negative log-likelihood (less is better)")),
        legends = Some(Seq("current", "best"))
      )
    )
    plotly.save(plot, outputPath / s"$currentTime.html", "Simulated Annealing")
  }

  def main(args: Array[String]): Unit = {
    val params = Parameters(None, LogSchedule(10), 10000)
    val inputPath = RelPath("tests/simple")
    run(inputPath, params)
  }
}
