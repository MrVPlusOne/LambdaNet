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
    numSamples: Int,
    reboot: Boolean
  )

  def run(relPathUnderData: RelPath, unseededParams: Parameters): Unit = {
    val params = unseededParams.copy(
      seed = Some(unseededParams.seed.getOrElse(Random.nextLong()))
    )
    Random.setSeed(params.seed.get)
    val inputPath = amm.pwd / "data" / relPathUnderData
    val outputPath = amm.pwd / "SA_results"
    val (graph, nodeAnnots, results) = InputUtils.loadGraphAndPredict(inputPath)
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
    val groundTruth = GroundTruth(nodeAnnots, toPlainType = true)
    val accuracy = Accuracy(groundTruth)
    //    println("======Difference between ground truth and most likely assignment======")
    //    println(SimulatedAnnealing.diff(groundTruth, mostLikely))
    println(s"size of mostLikely: ${mostLikely.size}")
    println(s"size of ground truth: ${groundTruth.truth.size}")
    println(
      s"number of nodes whose true types are not in predictions:" +
        s"${
          groundTruth.truth.count {
            case (node, typ) => !results(node).distr.exists(_._2 == typ)
          }
        }"
    )
    //    println(s"Average NLL of ground truth: ${criterion.prob(groundTruth.truth)}")

    val runs =
      Iterator
        .fill(params.numSamples) {
          SimulatedAnnealing.searchWithLog(
            graph,
            results,
            // TODO: store these two in parameters as well
            WrongFirstOneDifferenceRandomNeighbor(results, checker).randomNeighbor,
            PatchAnyCorrection(checker, results).correct,
            schedule,
            numEpochs = params.numEpochs,
            f = criterion.prob,
            reboot = params.reboot,
            accuracy = accuracy
          )
        }
        .reduce { (a, b) =>
          val (pred1, x) = a
          val (pred2, y) = b
          val best =
            if (criterion.prob(pred1) < criterion.prob(pred2)) pred1 else pred2
          val combined = IntermediateValues(
            x.epochs,
            (x.ys, y.ys).zipped.map(_ + _),
            (x.bestYs, y.bestYs).zipped.map(_ + _),
            (x.nodeAccuracy, y.nodeAccuracy).zipped.map(_ + _),
            (x.proportionOfAny, y.proportionOfAny).zipped.map(_ + _)
          )
          (best, combined)
        }
    val (best, sumLogValues) = runs
    println("======Difference between ground truth and final result======")
    val difference = SimulatedAnnealing.diff(groundTruth.truth, best)
    difference.foreach {
      case (node, (gold, pred)) =>
        println(s"$node -> ($gold, $pred): (${
          results(node).distr
            .find(_._2 == gold)
            .map(_._1)
        }, ${results(node).distr.find(_._2 == pred).map(_._1)})")
    }
    val stat =
      difference.groupBy(_._2).mapValuesNow(_.size).toIndexedSeq.sortBy(-_._2)
    stat.foreach(println)
    println(s"${difference.size} differences found")
    println(s"Average NLL of final result: ${criterion.prob(best)}")
    val averageLogValues: IntermediateValues = sumLogValues.copy(
      ys = sumLogValues.ys.map(_ / params.numSamples),
      bestYs = sumLogValues.bestYs.map(_ / params.numSamples),
      nodeAccuracy = sumLogValues.nodeAccuracy.map(_ / params.numSamples),
      proportionOfAny = sumLogValues.proportionOfAny.map(_ / params.numSamples)
    )
    if (!amm.exists(outputPath)) {
      amm.mkdir(outputPath)
    }
    val plotly = PlotlyBackend()
    val ys = averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.ys)
    val bestYs =
      averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.bestYs)
    val nodeAccuracy =
      averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.nodeAccuracy)
    val proportionOfAny =
      averageLogValues.epochs
        .map(_.toDouble)
        .zip(averageLogValues.proportionOfAny)
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
        axesNames = (Some("Epoch"), Some("Accuracy")),
        legends = Some(Seq("node"))
      )
    )
    val anyPlot = plotly.linePlot(
      Seq(proportionOfAny),
      CommonOptions(
        plotName = Some("Proportion of nodes with Any type"),
        axesNames = (Some("Epoch"), Some("Proportion of PAny")),
      )
    )
    val currentTime = LocalDateTime.now().format(fmt)
    plotly.save(
      plotly.column(likelihoodPlot, accuracyPlot, anyPlot),
      outputPath / s"$currentTime.html",
      "Simulated Annealing"
    )
  }

  def main(args: Array[String]): Unit = {
    val params = Parameters(
      None,
      LogSchedule(0.02),
      numEpochs = 1,
      numSamples = 1,
      reboot = false
    )
    val inputPath = RelPath("tests/public")
    run(inputPath, params)
  }
}
