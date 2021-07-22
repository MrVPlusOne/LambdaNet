package lambdanet.utils

import ammonite.ops.pwd
import ammonite.{ops => amm}
import lambdanet.{Model, SM, TypeInferenceService}

object PrecomputeResults {
  val modelDir = ModelFormatConversion.newestModelDir
  val modelPath = modelDir / "model.serialized"

  val model = SM.readObjectFromFile[Model](modelPath)
  val service = model.PredictionService(numOfThreads = 8, predictTopK = 5)

  def precompute(testName: String, overwrite: Boolean = false): Unit = {
    val inputPath = pwd / "data" / "tests" / testName
    val outputPath = inputPath / "results.serialized"
    if (amm.exists(outputPath) && !overwrite)
      return
    val results = service.predictOnProject(
      inputPath,
      warnOnErrors = false
    )
    SM.saveObjectToFile(outputPath.toIO)(results.asInstanceOf[Serializable])
  }

  def main(args: Array[String]): Unit = {
    args.foreach(precompute(_))
  }
}
