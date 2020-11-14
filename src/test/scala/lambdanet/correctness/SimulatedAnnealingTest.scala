package lambdanet.correctness

import lambdanet.translation.{PredicateGraph, PredicateGraphLoader}
import org.scalatest.WordSpec
import ammonite.{ops => amm}
import lambdanet.SM
import lambdanet.TypeInferenceService.{
  ModelConfig,
  PredictionResults,
  loadModel
}
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{
  BinaryRel,
  BinaryRelCat,
  DefineRel,
  PAny,
  PNode
}

class SimulatedAnnealingTest extends WordSpec {
  def test(name: String): Unit = {
    val inputPath = amm.pwd / "data" / "tests" / name
    val (graph, results) = CorrectnessTestUtils.loadGraphAndPredict(inputPath)
    assert(results.keySet == graph.projNodes)

    val checker = TypeChecker(graph)
    val schedule = (epoch: Int) => 20 * math.log(2) / math.log(epoch + 1)
    val correctPrediction =
      SimulatedAnnealing.search(
        graph,
        results,
        OneDifferenceRandomNeighbor(results).randomNeighbor,
        PatchAnyCorrection(checker, results).correct,
        schedule,
        numEpochs = 5000,
        f = LogLikelihood(results).prob
      )

    assert(checker.violate(correctPrediction) == Set.empty)
  }

  "should find a correct assignment for simple" in {
    test("simple")
  }

  "results should be serializable" in {
    val results = Map(
      new PNode(1, None, false, false, None) -> TopNDistribution(
        Vector((1.0, PAny))
      )
    )
    val path = (amm.pwd / "testResults").toIO
    SM.saveObjectToFile(path)(results.asInstanceOf[Serializable])
    val recovered = SM.loadObjectFromFile[results.type](path)
    assert(recovered == results)
  }
}
