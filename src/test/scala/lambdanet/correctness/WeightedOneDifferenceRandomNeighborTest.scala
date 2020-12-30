package lambdanet.correctness

import lambdanet.correctness.WeightedOneDifferenceRandomNeighborTest.{genAssignment, genProposal}
import lambdanet.correctness.WeightedPatchAnyTest.node
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PNode, PTyVar, PType}
import org.scalatest.WordSpec

class WeightedOneDifferenceRandomNeighborTest extends WordSpec {
  "WeightedOneDifferenceRandomNeighbor" should {
    "pass simple example" in {
      val proposal = genProposal(Seq((2, 0.5), (1, 0.4), (3, 0.1)))
      val assignment = genAssignment(Seq((1, 1)))
      val neighbor = WeightedOneDifferenceRandomNeighbor(proposal)
      (1 to 10).foreach { _ =>
        val newNeighbor = neighbor.randomNeighbor(assignment)(node(1))
        assert(newNeighbor != assignment(node(1)))
        println(newNeighbor)
      }
    }
  }
}

object WeightedOneDifferenceRandomNeighborTest {
  def node(id: Int): PNode = PNode(id, None, isType = true, fromLib = false)

  def typ(id: Int): PType = PTyVar(node(id))

  def genProposal(
      weights: Seq[(Int, Double)]
  ): Map[PNode, TopNDistribution[PType]] =
    Map(node(1) -> TopNDistribution(weights.map {
      case (id, prob) => (prob, typ(id))
    }.toVector))

  def genAssignment(
      pairs: Seq[(Int, Int)]
  ): Assignment =
    pairs.map { case (nodeId, typeId) => (node(nodeId), typ(typeId)) }.toMap
}
