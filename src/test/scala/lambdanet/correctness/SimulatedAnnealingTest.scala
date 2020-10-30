package lambdanet.correctness

import lambdanet.translation.PredicateGraphLoader
import org.scalatest.WordSpec
import ammonite.{ops => amm}
import lambdanet.SM
import lambdanet.TypeInferenceService.{ModelConfig, loadModel}
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PAny, PNode}

class SimulatedAnnealingTest extends WordSpec {
  def prepare(name: String): Unit = {
    val inputPath = amm.pwd / "data" / "tests" / name
    val modelDir = amm.pwd / "models" / "newParsing-GAT1-fc2-newSim-decay-6"
    val paramPath = modelDir / "params.serialized"
    val modelCachePath = modelDir / "model.serialized"
    val modelConfig = ModelConfig()

    val model =
      loadModel(paramPath, modelCachePath, modelConfig, numOfThreads = 8)
    val service = model.PredictionService(numOfThreads = 8, predictTopK = 5)

    val results = service.predictOnProject(inputPath, warnOnErrors = false)
  }

  def test(name: String): Unit = {
    val inputPath = amm.pwd / "data" / "tests" / name
    val graph = PredicateGraphLoader.load(inputPath)
    val resultsPath = inputPath / "results.serialized"
    val results =
      if (amm.exists(resultsPath)) {
        SM.loadObjectFromFile[TypeDistrs](resultsPath.toIO)
      } else {
        val modelDir = amm.pwd / "models" / "newParsing-GAT1-fc2-newSim-decay-6"
        val paramPath = modelDir / "params.serialized"
        val modelCachePath = modelDir / "model.serialized"
        val modelConfig = ModelConfig()

        val model =
          loadModel(paramPath, modelCachePath, modelConfig, numOfThreads = 8)
        val service = model.PredictionService(numOfThreads = 8, predictTopK = 5)

        val res = service.predictOnProject(inputPath, warnOnErrors = false)
        SM.saveObjectToFile(resultsPath.toIO)(res.asInstanceOf[Serializable])
        res
      }
    assert(results.keySet == graph.nodes)

    val checker = TypeChecker(graph)
    val schedule = (epoch: Int) => 2 * math.pow(0.8, epoch)
    val correctPrediction = SimulatedAnnealing.search(
      graph,
      results,
      OneDifferenceRandomNeighbor(results).randomNeighbor,
      LocalSearchCorrection(checker, results).correct,
      schedule,
      numEpochs = 100,
      f = LogLikelihood(results).prob
    )
    assert(checker.violate(correctPrediction).isEmpty)
  }

  "should find a correct assignment for simple" in {
    test("simple")
  }

  "results should be serializable" in {
    val results = Map(
      new PNode(1, None, false, false, None) -> TopNDistribution(Vector((1.0, PAny)))
    )
    val path = (amm.pwd / "testResults").toIO
    SM.saveObjectToFile(path)(results.asInstanceOf[Serializable])
    val recovered = SM.loadObjectFromFile[results.type](path)
    assert(recovered == results)
  }
}
