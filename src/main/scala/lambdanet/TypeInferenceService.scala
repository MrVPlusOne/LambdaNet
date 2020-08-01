package lambdanet

import ammonite.ops.Path
import ammonite.{ops => amm}
import amm.RelPath
import funcdiff.ParamCollection
import funcdiff.SimpleMath.{readObjectFromFile, saveObjectToFile}
import lambdanet.PrepareRepos.ParsedRepos
import lambdanet.architecture.GATArchitecture
import lambdanet.train.DataSet

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
      announced("Load model from cache") {
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
          //todo: change this if you want to predict user defined types
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

  def main(args: Array[String]): Unit = {
    val modelDir = amm.pwd / "models" / "newParsing-GAT1-fc2-newSim-decay-6"
    val paramPath = modelDir / "params.serialized"
    val modelCachePath = modelDir / "model.serialized"
    val modelConfig = ModelConfig()

    loadModel(paramPath, modelCachePath, modelConfig,
      numOfThreads = 8,
    )
    println("Model load!")
  }
}
