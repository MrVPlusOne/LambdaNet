package lambdanet.utils

import ammonite.ops.pwd
import ammonite.{ops => amm}
import funcdiff.{DebugTime, SymbolPath}
import lambdanet.{SM, TypeInferenceService}
import lambdanet.TypeInferenceService.{ModelConfig, loadModel}

object PrecomputeResults {
  val modelDir = TypeInferenceService.newestModelDir
  val paramPath = modelDir / "params.serialized"
  val modelCachePath = modelDir / "model.serialized"
  val modelConfig = ModelConfig()

  val model =
    loadModel(paramPath, modelCachePath, modelConfig, numOfThreads = 8)
  val service = model.PredictionService(numOfThreads = 8, predictTopK = 5)

  def precompute(testName: String, overwrite: Boolean = false): Unit = {
    val inputPath = pwd / "data" / "tests" / testName
    val outputPath = inputPath / "results.serialized"
    if (amm.exists(outputPath) && !overwrite)
      return
    val results = service.predictOnProject(inputPath, warnOnErrors = false)
    SM.saveObjectToFile(outputPath.toIO)(results.asInstanceOf[Serializable])
  }

  def main(args: Array[String]): Unit = {
    args.foreach(precompute(_))
  }
}
