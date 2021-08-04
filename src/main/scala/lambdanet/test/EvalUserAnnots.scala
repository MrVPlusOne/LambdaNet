package lambdanet.test

import funcdiff.{GraphMode, ModeEval}
import ammonite.ops._
import funcdiff.SimpleMath.{mean, readObjectFromFile, stdDev}
import lambdanet.PrepareRepos.{ParsedRepos, parsedReposDir}
import lambdanet.train.{DataSet, toAccuracy}
import lambdanet.train.Training.AnnotsSampling
import lambdanet.{Configs, Model, announced, printInfo}
import me.tongfei.progressbar.ProgressBar

import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Evaluates model accuracy on the test set by randomly dropping some of
  * the user annotations and conditioning the prediction on the rest.
  */
object EvalUserAnnots {
  def loadModelData(modelDir: Path) = {
    val modelPath = modelDir / "model.serialized"
    val model = announced("Loading model") {
      readObjectFromFile[Model](modelPath)
    }
    import model.config.{predictAny, onlyPredictLibType}
    val repos = ParsedRepos.readFromDir(parsedReposDir(predictAny))
    val dataSet = DataSet.makeDataSet(repos, onlyPredictLibType, predictAny)
    (model, dataSet)
  }

  def main(args: Array[String]): Unit = {
    val modelDir = pwd / RelPath(
      "running-results/UserAnnot-v1.3-GAT1-fc2AnnotsSampling(0.0,0.81)--decay-lossAgg_sum-encodeSignature-6/saved/bestModel",
    )
    printInfo(s"Evaluating model at $modelDir...")
    val (model, dataSet) = loadModelData(modelDir)
    val annotsRatios = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 0.9)
    val repeats = 10

    val numOfThreads = Configs().numOfThreads()
    val taskSupport = new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads))
    val table = new StringBuilder()
    table.append("Ratio, Total Acc, Lib Acc, Proj Acc\n")
    implicit val m: GraphMode = ModeEval

    val steps = annotsRatios.length * repeats * dataSet.testSet.size
    val prog = new ProgressBar("Evaluating", steps)
    for (ratio <- annotsRatios) yield {
      import cats.implicits._
      val (totalAcc, libAcc, projAcc) = (0 until repeats).map { _ =>
        val (total, lib, proj) = dataSet.testSet.foldMap { p =>
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
          (fr.totalCorrect, fr.libCorrect, fr.projCorrect)
        }
        (toAccuracy(total) * 100, toAccuracy(lib) * 100, toAccuracy(proj) * 100)
      }.unzip3
      table.append(s"$ratio, ")
      table.append(Vector(totalAcc, libAcc, projAcc).map(Gaussian.apply).mkString(", "))
      table.append("\n")
      val result = table.mkString
      println("-----")
      println(result)
      println("-----")
      write.over(modelDir / "EvalUserAnnots.csv", table.mkString)
    }
    prog.close()
  }

  case class Gaussian(mean: Double, std: Double) {
    override def toString: String = {
      val precision = if (std < 1.0) math.ceil(-math.log10(std)).toInt else 1
      s"%.${precision}f±%.${precision + 1}f".format(mean, std)
    }
  }

  object Gaussian {
    def apply(xs: Seq[Double]): Gaussian =
      Gaussian(mean(xs), stdDev(xs))
  }
}
