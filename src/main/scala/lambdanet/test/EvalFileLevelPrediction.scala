package lambdanet.test

import funcdiff.{GraphMode, ModeEval}
import lambdanet.Configs
import lambdanet.test.TestUtils.loadModelData

import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.ForkJoinTaskSupport

class EvalFileLevelPrediction {
  def main(args: Array[String]): Unit = {
    val modelDir = Configs().modelDir()
    val (model, repos, _) = loadModelData(modelDir)
    val annotsRatio = 0.5

    val numOfThreads = Configs().numOfThreads()
    val taskSupport = new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads))
    implicit val m: GraphMode = ModeEval

    import cats.implicits._
    repos.testSet.foreach { p =>
      // group nodes by their originating files
      p.qModules.foreach { mod =>
        mod.mapping
      }
    }

  }
}
