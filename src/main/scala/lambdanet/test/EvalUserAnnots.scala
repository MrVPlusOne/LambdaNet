package lambdanet.test

import ammonite.ops._
import funcdiff.SimpleMath.{mean, readObjectFromFile, stdDev}
import lambdanet.PrepareRepos.{ParsedRepos, parsedReposDir}
import lambdanet.train.{DataSet, toAccuracy}
import lambdanet.train.Training.AnnotsSampling
import lambdanet.{Configs, Model, announced}
import me.tongfei.progressbar.ProgressBar

import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Evaluates model accuracy on the test set by randomly dropping some of
  * the user annotations and conditioning the prediction on the rest.
  */
object EvalUserAnnots {
  def loadModelData(modelDir: Path, predictAny: Boolean, onlyPredictLibType: Boolean) = {
    val modelPath = modelDir / "model.serialized"
    val model = announced("Loading model") {
      readObjectFromFile[Model](modelPath)
    }
    val repos = ParsedRepos.readFromDir(parsedReposDir(predictAny))
    val dataSet = DataSet.makeDataSet(repos, onlyPredictLibType, predictAny)
    (model, dataSet)
  }

  def main(args: Array[String]): Unit = {
    val modelDir = pwd / RelPath(
      "running-results/NewData-GAT1-fc2AnnotsSampling(0.0,0.81)--decay-lossAgg_sum-encodeSignature-6/saved/epoch40"
    )
    val (model, dataSet) = loadModelData(modelDir, predictAny = false, onlyPredictLibType = false)
    val annotsRatios = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 0.9)
    val repeats = 10

    val numOfThreads = Configs().numOfThreads()
    val taskSupport = new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads))
    val table = new StringBuilder()
    table.append("Ratio, Lib Acc, Proj Acc, Total Acc\n")

    val steps = annotsRatios.length * repeats * dataSet.testSet.size
    val prog = new ProgressBar("Evaluating", steps)
    for (ratio <- annotsRatios) yield {
      import cats.implicits._
      val (libAcc, projAcc, totalAcc) = (0 until repeats).map { _ =>
        val (lib, proj, total) = dataSet.testSet.foldMap { p =>
          val fr = model
            .forwardStep(
              p,
              annotsSampling = AnnotsSampling(ratio, ratio),
              shouldDropout = false,
              maxLibRatio = None,
              maxBatchSize = None,
              projWeight = 1.0,
              taskSupport = Some(taskSupport),
            )
            ._2
          prog.step()
          (fr.libCorrect, fr.projCorrect, fr.libCorrect |+| fr.projCorrect)
        }
        (toAccuracy(lib), toAccuracy(proj), toAccuracy(total))
      }.unzip3
      table.append(s"$ratio, ")
      table.append(Vector(libAcc, projAcc, totalAcc).map(Gaussian.apply).mkString(", "))
      table.append("\n")
    }
    val result = table.toString()
    println(result)
    write(pwd / "EvalUserAnnots.csv", result)
  }

  case class Gaussian(mean: Double, std: Double) {
    override def toString: String = {
      val precision = (if (std < 1.0) math.ceil(-math.log10(std)).toInt else 1) + 1
      s"%.${precision}f±%.${precision}f".format(mean, std)
    }
  }

  object Gaussian {
    def apply(xs: Seq[Double]): Gaussian =
      Gaussian(mean(xs), stdDev(xs))
  }
}
