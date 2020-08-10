package lambdanet

import ammonite.{ops => amm}
import amm.{Path, RelPath}
import lambdanet.TypeInferenceService.ModelConfig

object JavaAPI {
  def pwd: Path = amm.pwd

  def relPath(path: String): RelPath =
    RelPath(path)

  def absPath(path: String): Path =
    Path(path)

  def joinPath(head: Path, tail: String): Path = head / RelPath(tail)

  def defaultModelConfig: ModelConfig = ModelConfig()

  def predictionService(model: Model, numOfThreads: Int, predictTopK: Int) =
    model.PredictionService(numOfThreads, predictTopK)
}
