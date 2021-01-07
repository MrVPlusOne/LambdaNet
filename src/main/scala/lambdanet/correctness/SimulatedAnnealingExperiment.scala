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
      penaltyCoefficient: Double,
      reboot: Boolean
  )

  case class ParametersGrid(
      seed: Option[Long],
      schedule: Seq[Schedule],
      numEpochs: Seq[Int],
      numSamples: Seq[Int],
      penaltyCoefficient: Seq[Double],
      reboot: Seq[Boolean]
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

    val criterion =
      PenalizedAverageNLL(results, checker, params.penaltyCoefficient)

    // TODO: fix this by providing an encoder
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
        s"${groundTruth.truth.count {
          case (node, typ) => !results(node).distr.exists(_._2 == typ)
        }}"
    )
    //    println(s"Average NLL of ground truth: ${criterion.prob(groundTruth.truth)}")

    val runs =
      Iterator
        .fill(params.numSamples) {
          SimulatedAnnealing.searchWithLog(
            graph,
            results,
            // TODO: store these two in parameters as well
            WeightedOneDifferenceRandomNeighbor(results).randomNeighbor,
            if (params.penaltyCoefficient == 0.0)
              WeightedPatchAnyCorrection(checker, results).correct
            else
              NoCorrection.correct,
            WeightedPatchAnyCorrection(checker, results).correct,
            schedule,
            numEpochs = params.numEpochs,
            f = criterion.prob,
            reboot = params.reboot,
            accuracy = accuracy,
            checker = checker
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
            (x.nll, y.nll).zipped.map(_ + _),
            (x.penalty, y.penalty).zipped.map(_ + _),
            (x.nodeAccuracy, y.nodeAccuracy).zipped.map(_ + _),
            (x.correctedNodeAccuracy, y.correctedNodeAccuracy).zipped
              .map(_ + _),
            (x.proportionOfAny, y.proportionOfAny).zipped.map(_ + _),
            (x.proportionOfNodesCoveredByAny, y.proportionOfNodesCoveredByAny).zipped
              .map(_ + _),
          )
          (best, combined)
        }
    val (best, sumLogValues) = runs

    println("======Difference between most likely and final result======")
    val mostLikelyDifference = Assignment.diff(results, mostLikely, best)
    println("Raw difference:")
    println(mostLikelyDifference)
    val mostLikelyDifferenceStat = Assignment.DiffStats(mostLikelyDifference)
    println("Difference stats:")
    println(mostLikelyDifferenceStat)
    println("======Difference between ground truth and final result======")
    val groundTruthDifference =
      Assignment.diff(results, groundTruth.truth, best)
    println("Raw difference:")
    println(groundTruthDifference)
    val groundTruthDifferenceStat = Assignment.DiffStats(groundTruthDifference)
    println("Difference stats:")
    println(groundTruthDifferenceStat)
    println(s"${groundTruthDifference.diff.size} differences found")
    println(s"Average NLL of final result: ${criterion.prob(best)}")

    val averageLogValues: IntermediateValues = sumLogValues.copy(
      ys = sumLogValues.ys.map(_ / params.numSamples),
      bestYs = sumLogValues.bestYs.map(_ / params.numSamples),
      nll = sumLogValues.nll.map(_ / params.numSamples),
      penalty = sumLogValues.penalty.map(_ / params.numSamples),
      nodeAccuracy = sumLogValues.nodeAccuracy.map(_ / params.numSamples),
      correctedNodeAccuracy =
        sumLogValues.correctedNodeAccuracy.map(_ / params.numSamples),
      proportionOfAny = sumLogValues.proportionOfAny.map(_ / params.numSamples),
      proportionOfNodesCoveredByAny =
        sumLogValues.proportionOfNodesCoveredByAny.map(_ / params.numSamples),
    )
    if (!amm.exists(outputPath)) {
      amm.mkdir(outputPath)
    }
    // fixme: Cannot display plots with 300K entries
    val plotly = PlotlyBackend()
    val ys = averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.ys)
    val bestYs =
      averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.bestYs)
    val nll =
      averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.nll)
    val penalty =
      averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.penalty)
    val nodeAccuracy =
      averageLogValues.epochs.map(_.toDouble).zip(averageLogValues.nodeAccuracy)
    val correctedNodeAccuracy =
      averageLogValues.epochs
        .map(_.toDouble)
        .zip(averageLogValues.correctedNodeAccuracy)
    val proportionOfAny =
      averageLogValues.epochs
        .map(_.toDouble)
        .zip(averageLogValues.proportionOfAny)
    val proportionOfNodesCoveredByAny =
      averageLogValues.epochs
        .map(_.toDouble)
        .zip(averageLogValues.proportionOfNodesCoveredByAny)
    val likelihoodPlot = plotly.linePlot(
      Seq(ys, bestYs, nll, penalty),
      CommonOptions(
        plotName = Some("Negative log-likelihoods over time on simple"),
        axesNames =
          (Some("Epoch"), Some("Negative log-likelihood (less is better)")),
        legends = Some(Seq("current", "best", "NLL", "penalty"))
      )
    )
    val accuracyPlot = plotly.linePlot(
      Seq(nodeAccuracy, correctedNodeAccuracy, proportionOfAny),
      CommonOptions(
        plotName = Some("Accuracy"),
        axesNames = (Some("Epoch"), Some("Accuracy")),
        legends = Some(Seq("raw", "projected", "Proportion of Any"))
      )
    )
//    val anyPlot = plotly.linePlot(
//      Seq(proportionOfAny),
//      CommonOptions(
//        plotName = Some("Proportion of nodes with Any type"),
//        axesNames = (Some("Epoch"), Some("Proportion of PAny")),
//        legends = Some(
//          Seq("Proportion of Any nodes", "Proportion of nodes covered by Any")
//        )
//      )
//    )
    val currentTime = LocalDateTime.now().format(fmt)
    plotly.save(
      plotly.column(likelihoodPlot, accuracyPlot),
      outputPath / s"$currentTime.html",
      "Simulated Annealing"
    )
//    implicit val fooDecoder: Decoder[Parameters] = deriveDecoder[Parameters]
//    amm.write(outputPath / s"$currentTime.json", params.asJson.toString())
  }

  def run(relPathUnderData: RelPath, paramsGrid: ParametersGrid): Unit = {
    val seed = paramsGrid.seed
    val allParams = for {
      schedule <- paramsGrid.schedule
      numEpochs <- paramsGrid.numEpochs
      numSamples <- paramsGrid.numSamples
      penaltyCoefficient <- paramsGrid.penaltyCoefficient
      reboot <- paramsGrid.reboot
    } yield Parameters(
      seed = seed,
      schedule = schedule,
      numEpochs = numEpochs,
      numSamples = numSamples,
      penaltyCoefficient = penaltyCoefficient,
      reboot = reboot
    )
    allParams.foreach(params => run(relPathUnderData, params))
  }

  def main(args: Array[String]): Unit = {
    val params = Parameters(
      None,
      LogSchedule(0.02),
      numEpochs = 1,
      numSamples = 1,
      penaltyCoefficient = 0,
      reboot = false
    )
    val inputPath = RelPath("tests/public")
    run(inputPath, params)
  }
}
