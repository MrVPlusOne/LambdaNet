package lambdanet

import ammonite.ops.Path
import ammonite.{ops => amm}
import funcdiff.ParamCollection
import funcdiff.SimpleMath.{readObjectFromFile, saveObjectToFile}
import lambdanet.PrepareRepos.ParsedRepos
import lambdanet.architecture.GATArchitecture
import lambdanet.train.{DataSet, TopNDistribution}
import lambdanet.translation.PredicateGraph

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.Random

object TypeInferenceService {

  case class ModelConfig(
      dimMessage: Int = 32,
      gatHeads: Int = 1,
      seed: Long = 1,
  )

  def loadModel(
      paramPath: Path,
      modelCachePath: Path,
      modelConfig: ModelConfig,
      numOfThreads: Int,
      parsedReposDir: Path = amm.pwd / 'data / "parsedRepos",
  ): Model =
    if (amm.exists(modelCachePath)) {
      announced(s"Load model from the cache: $modelCachePath") {
        readObjectFromFile[Model](modelCachePath.toIO)
      }
    } else {
      import modelConfig._

      println(
        s"No model file found under '$modelCachePath', creating new model..."
      )

      val pc = announced("Load model weights")(
        ParamCollection.fromFile(paramPath)
      )

      val dataSet = announced("Process data set") {
        val repos = ParsedRepos.readFromDir(parsedReposDir)
        DataSet.makeDataSet(
          repos,
          Some(new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads))),
          useSeqModel = false,
          toyMode = false,
          onlyPredictLibType = false
        )
      }
      val model = announced("Create model") {
        val architecture = GATArchitecture(gatHeads, dimMessage, pc)
        Model.fromData(dataSet, architecture, new Random(seed))
      }

      announced(s"Save model to '$modelCachePath'") {
        saveObjectToFile(modelCachePath.toIO)(model)
      }
      model
    }

  case class PredictionResults(
      map: Map[PredicateGraph.PNode, TopNDistribution[PredicateGraph.PType]]
  ) {
    def prettyPrint(): Unit = {
      val byFile = map.keys.groupBy(_.srcSpan.get.srcFile).toSeq.sortBy(_._1)
      byFile.foreach {
        case (file, nodes) =>
          println(s"=== File: $file ===")
          nodes.toSeq.sortBy(_.srcSpan.get.start).foreach { n =>
            val span = n.srcSpan.get.showShort()
            val rankedList = map(n).distr.zipWithIndex
              .map {
                case ((p, ty), i) => {
                  val acc = "%.2f".format(p * 100)
                  s"[${i + 1}]($acc%) ${ty.showSimple}"
                }
              }
              .mkString(", ")
            println(s"$span: $rankedList")
          }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val modelDir = amm.pwd / "models" / "newParsing-GAT1-fc2-newSim-decay-6"
    val paramPath = modelDir / "params.serialized"
    val modelCachePath = modelDir / "model.serialized"
    val modelConfig = ModelConfig()

    val model =
      loadModel(paramPath, modelCachePath, modelConfig, numOfThreads = 8)

    val service = model.PredictionService(numOfThreads = 8, predictTopK = 5)
    printResult("Type Inference Service successfully started.")
    printResult(s"Current working directory: ${amm.pwd}")
    while (true) {
      print("Enter project path: ")
      System.out.flush()
      try {
        val line = scala.io.StdIn.readLine()
        require(line.strip().nonEmpty, "Specified path should not be empty.")
        val sourcePath = Path(line, amm.pwd)
        val results = service.predictOnProject(sourcePath, warnOnErrors = false)
        PredictionResults(results).prettyPrint()
      } catch {
        case e: Throwable =>
          println(s"Got exception: ${e.getMessage}")
          e.printStackTrace(System.out)
      }
    }
  }
}
