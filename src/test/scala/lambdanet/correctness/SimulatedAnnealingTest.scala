package lambdanet.correctness

import ammonite.{ops => amm}
import lambdanet.SM
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PAny, PNode}
import org.scalatest.WordSpec

class SimulatedAnnealingTest extends WordSpec {
  def test(name: String): Unit = {
    val inputPath = amm.pwd / "data" / "tests" / name
    val (graph, results) = InputUtils.loadGraphAndPredict(inputPath)
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
        f = NegativeLogLikelihood(results).prob
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
