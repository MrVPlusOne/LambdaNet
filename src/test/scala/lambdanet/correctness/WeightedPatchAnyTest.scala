package lambdanet.correctness

import lambdanet.correctness.WeightedPatchAnyTest._
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PAny, PNode, PType}
import org.scalatest.WordSpec

class WeightedPatchAnyTest extends WordSpec {
  "weighted patch any" should {
    "output same result as example in slides" in {
      /* See page 13 of http://web.cs.iastate.edu/~cs511/handout10/Approx_VC.pdf
       * Common example used in many slides online
       * a is given id 1, b given 2, ...
       */
      val proposal = genProposal(Seq(1 -> 4, 2 -> 3, 3 -> 5, 4 -> 3))
      val badPairs = genBadPairs(Seq(1 -> 2, 1 -> 4, 4 -> 3, 1 -> 3, 2 -> 3))
      val assignment = proposal.mapValuesNow(_ => null)
      val patcher = WeightedPatchAnyImpl(proposal)
      assert(
        patcher.tighten(badPairs) ==
          Map(node(1) -> 0.0, node(2) -> 0, node(3) -> 3, node(4) -> 0)
      )
      assert(
        patcher.patchAny(badPairs, assignment) ==
          Map(
            node(1) -> PAny,
            node(2) -> PAny,
            node(3) -> null,
            node(4) -> PAny
          )
      )
    }
  }
}

object WeightedPatchAnyTest {
  def genProposal(
      weights: Seq[(Int, Double)]
  ): Map[PNode, TopNDistribution[PType]] =
    weights.map {
      case (id, nll) =>
        (
          node(id),
          TopNDistribution(Vector[(Double, PType)]((math.exp(-nll), PAny)))
        )
    }.toMap

  def genBadPairs(pairs: Seq[(Int, Int)]): Seq[(PNode, PNode)] =
    pairs.map(x => (node(x._1), node(x._2)))

  def node(id: Int): PNode = PNode(id, None, isType = false, fromLib = false)
}

case class WeightedPatchAnyImpl(proposal: TypeDistrs) extends WeightedPatchAny
