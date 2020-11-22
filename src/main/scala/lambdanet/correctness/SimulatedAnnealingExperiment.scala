package lambdanet.correctness

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import ammonite.ops.RelPath
import ammonite.{ops => amm}
import lambdanet.correctness.SimulatedAnnealing.IntermediateValues
import lambdanet.translation.PredicateGraph.{PNode, PTyVar}
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
    val mostLikely = results.map { case (k, v) => (k, v.distr(0)._2) }
    println(mostLikely)
    println(AverageNegativeLogLikelihood(results).prob(mostLikely))
    checker.violate(mostLikely).foreach(println)
    val pNodes = results.keySet.map(x => (x.getId, x)).toMap
    val pTypes = results.values.flatMap(_.distr.map(_._2)).collect{ case (x: PTyVar) => x }.toSet.map((x: PTyVar) => (x.node.nameOpt.map(_.name).getOrElse(x.node.getId.toString), x)).toMap
    pTypes.foreach(println)
    val groundTruth = mostLikely
      .updated(pNodes(5), pTypes("C1"))
      .updated(pNodes(16), pTypes("Number"))
      .updated(pNodes(14), pTypes("Boolean"))
      .updated(pNodes(15), pTypes("String"))
      .updated(pNodes(13), pTypes("Window"))
    checker.violate(groundTruth).foreach(println)
    println(AverageNegativeLogLikelihood(results).prob(groundTruth))
    throw new Exception

    val trials = 10
    val sumLogValues =
      Iterator.fill(trials) {
        SimulatedAnnealing.searchWithLog(
          graph,
          results,
          // TODO: store these two in parameters as well
          OneDifferenceRandomNeighbor(results).randomNeighbor,
          PatchAnyCorrection(checker, results).correct,
          schedule,
          numEpochs = params.numEpochs,
          f = AverageNegativeLogLikelihood(results).prob
          , reboot = false
        )
      }.map(_._2).reduce { (x, y) =>
        IntermediateValues(x.epochs, (x.ys, y.ys).zipped.map(_ + _), (x.bestYs, y.bestYs).zipped.map(_ + _))
      }
    val averageLogValues: IntermediateValues = sumLogValues.copy(ys = sumLogValues.ys.map(_ / trials), bestYs = sumLogValues.bestYs.map(_ / trials))
    if (!amm.exists(outputPath)) {
      amm.mkdir(outputPath)
    }
    val plotly = PlotlyBackend()
    val ys = averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.ys)
    val bestYs = averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.bestYs)
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
    val params = Parameters(None, LogSchedule(0.2), 20000)
    val inputPath = RelPath("tests/c1")
    run(inputPath, params)
  }
}
