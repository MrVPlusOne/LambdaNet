package lambdanet.correctness

import ammonite.ops.Path
import ammonite.{ops => amm}
import lambdanet.PrepareRepos.ParsedProject
import lambdanet.PrepareRepos.ParsedRepos.Meta
import lambdanet.{SM, announced}

object Benchmark {
  trait Experiment {
    case class BenchmarkResults()
    def run(project: ParsedProject, results: TypeDistrs, parentDir: Path): BenchmarkResults
  }

  def predictAll(projects: Seq[ParsedProject]): Seq[TypeDistrs] = {
    projects
      .map(project => NewestModelService.service.predictOnGraph(project.pGraph))
  }

  def predictionPath(dir: Path, chunkNo: Int): Path =
    dir / s"results$chunkNo"

  def saveResults(results: Seq[TypeDistrs], dir: Path, chunkNo: Int): Unit = {
    val filepath = predictionPath(dir, chunkNo)
    announced(s"Saving results chunk $chunkNo to $filepath")(
      SM.saveObjectToFile(filepath)(results.asInstanceOf[Serializable])
    )
  }

  def existsResults(meta: Meta, predictionResultsPath: Path): Boolean = {
    (0 until meta.chunkNum).exists { chunkNo =>
      val filepath = predictionPath(predictionResultsPath, chunkNo)
      amm.exists(filepath)
    }
  }

  def loadChunk(path: Path, chunkNo: Int): Seq[ParsedProject] = {
    val chunkPath = path / s"chunk$chunkNo"
    SM.readObjectFromFile[Seq[ParsedProject]](chunkPath.toIO)
  }

  def run(testSetPath: Path, predictionResultsPath: Path, outputPath: Path/*, experiment: Experiment*/): Unit = {
    if (!amm.exists(predictionResultsPath)) {
      amm.mkdir(predictionResultsPath)
    }
    if (!amm.exists(outputPath)) {
      amm.mkdir(outputPath)
    }
//    System.setOut(
//      new PrintStream(
//        new BufferedOutputStream(new FileOutputStream((outputPath / "output.txt").toIO))
//      )
//    )
    val meta = SM.readObjectFromFile[Meta]((testSetPath / "meta").toIO)
    if (!existsResults(meta, predictionResultsPath)) {
      for (chunkNo <- 0 until meta.chunkNum) {
        val chunk = loadChunk(testSetPath, chunkNo)
        val predictionResults = predictAll(chunk)
        saveResults(predictionResults, predictionResultsPath, chunkNo)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val testSetPath = amm.pwd / "data" / "parsedRepos-with_any-test"
    run(testSetPath, testSetPath, amm.pwd / "benchmark")
  }
}
