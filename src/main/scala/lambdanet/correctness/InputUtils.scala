package lambdanet.correctness

import ammonite.ops.Path
import ammonite.{ops => amm}
import lambdanet.TypeInferenceService.{ModelConfig, loadModel}
import lambdanet.translation.{PredicateGraph, PredicateGraphLoader}
import lambdanet.{Model, SM, TypeInferenceService}

object InputUtils {
  val modelDir: Path = TypeInferenceService.newestModelDir
  val paramPath: Path = modelDir / "params.serialized"
  val modelCachePath: Path = modelDir / "model.serialized"
  val modelConfig: ModelConfig = ModelConfig()

  lazy val model: Model =
    loadModel(paramPath, modelCachePath, modelConfig, numOfThreads = 8)
  lazy val service: model.PredictionService =
    model.PredictionService(numOfThreads = 8, predictTopK = Int.MaxValue)

  def loadGraphAndPredict(inputPath: Path): (PredicateGraph, TypeDistrs) = {
    val graph = PredicateGraphLoader.load(inputPath)
    val resultsPath = inputPath / "results.serialized"
    val results =
      if (amm.exists(resultsPath)) {
        SM.loadObjectFromFile[TypeDistrs](resultsPath.toIO)
      } else {
        val res = service.predictOnGraph(graph, nodeSelector = _.fromProject)
        SM.saveObjectToFile(resultsPath.toIO)(res.asInstanceOf[Serializable])
        res
      }
    (graph, results)
  }

  def loadGroundTruth(inputPath: Path): Option[Assignment] = {
    val groundTruthPath = inputPath / "ground_truth.serialized"
    if (amm.exists(groundTruthPath)) {
      Some(SM.loadObjectFromFile[Assignment](groundTruthPath.toIO))
    } else {
      None
    }
  }
}
