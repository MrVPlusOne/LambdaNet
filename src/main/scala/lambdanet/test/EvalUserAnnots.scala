package lambdanet.test

import TestUtils._
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

  def main(args: Array[String]): Unit = {
    val modelDir = Configs().modelDir()
    printInfo(s"Evaluating model at $modelDir...")
    val (model, repos, dataSet) = loadModelData(modelDir)
    val annotsRatios = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 0.9)
    val repeats = 10

    val taskSupport = Model.mkTaskSupport(Configs().numOfThreads())
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
        (toAccuracy(total)*100, toAccuracy(lib)*100, toAccuracy(proj)*100)
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
}
