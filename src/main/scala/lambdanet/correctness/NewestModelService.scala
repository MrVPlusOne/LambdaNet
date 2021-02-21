package lambdanet.correctness

import ammonite.ops.Path
import lambdanet.TypeInferenceService.{ModelConfig, loadModel}
import lambdanet.{Model, TypeInferenceService}

object NewestModelService {
  val modelDir: Path = TypeInferenceService.newestModelDir
  val paramPath: Path = modelDir / "params.serialized"
  val modelCachePath: Path = modelDir / "model.serialized"
  val modelConfig: ModelConfig = ModelConfig()

  lazy val model: Model =
    loadModel(paramPath, modelCachePath, modelConfig, numOfThreads = 8)
  lazy val service: model.PredictionService =
    model.PredictionService(numOfThreads = 8, predictTopK = Int.MaxValue)
}
