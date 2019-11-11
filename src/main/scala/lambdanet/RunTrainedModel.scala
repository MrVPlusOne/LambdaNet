package lambdanet

import ammonite.ops._
import funcdiff.{Optimizer, ParamCollection, SimpleMath}
import lambdanet.train.{DataSet, Datum, Timeouts, TrainingLoop, TrainingState}
import lambdanet.utils.QLangDisplay

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool

object RunTrainedModel {

  def runTrainedModel(
      paramPath: Path,
      sourcePath: Path,
      outputPath: Path,
      numOfThreads: Int = 8
  ) = {
    import PrepareRepos._

    val repos = DataSet.loadRepos(toyMode = false)
    val libDefs = repos.libDefs
    val testProject =
      prepareProject(
        libDefs,
        sourcePath / up,
        sourcePath,
        skipSet = Set("node_modules", "__tests__", "test", "tests"),
        shouldPruneGraph = false
      )
    val repos1 = repos.copy(devSet = List(), testSet = List(testProject))
    val dataSet = DataSet.makeDataSet(
      repos1,
      Some(new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads))),
      useSeqModel = false,
      toyMode = false,
      testSetUseInferred = true
    )

    val pc = ParamCollection.fromFile(paramPath)
    val model = announced("Loading model...") {
      TrainingLoop
        .config(numOfThreads = 8, pwd / "test-trained", None)
        .makeModel(pc, dataSet)
    }

    val datum = dataSet.testSet.head
    val (_, fwd, pred) = model
      .forward(
        datum,
        shouldDownsample = false,
        shouldDropout = false,
        maxBatchSize = None
      )
      .get
    QLangDisplay.renderProjectToDirectory(
      datum.projectName.toString,
      datum.qModules,
      pred,
      datum.predictionSpace.allTypes
    )(outputPath / "predictions")

    import train.toAccuracy
    println("libAccuracy: " + toAccuracy(fwd.libCorrect))
    println("projectAccuracy: " + toAccuracy(fwd.projCorrect))
  }

  def renameToTs() = {
    val r = pwd / RelPath("../lambda-repos/javascript-algorithms-ts")
    for { f <- ls.rec(r) if f.ext == "js" } {
      def changeExtension(name: String, old: String, newExt: String): String = {
        name.dropRight(old.length) + newExt
      }
      mv(
        f,
        r / RelPath(changeExtension(f.relativeTo(r).toString(), "js", "ts"))
      )
    }
  }

  def main(args: Array[String]): Unit = {
    val paramPath = pwd / RelPath(
      "../lambda-repos/newParsing-GAT1-fc2-newSim-decay-6/params.serialized"
//      "../lambda-repos/newParsing-lib-GAT1-fc2-newSim-decay-6/params.serialized"
    )
    val sourcePath = pwd / RelPath("../lambda-repos/js-algorithms")
//    val sourcePath = pwd / RelPath("data/comparison")

    val outputPath = sourcePath

    import scala.concurrent.duration._
    Timeouts.forwardTimeout = 1000.seconds
    Timeouts.optimizationTimeout = 1000.seconds
    runTrainedModel(paramPath, sourcePath, outputPath)
  }
}
