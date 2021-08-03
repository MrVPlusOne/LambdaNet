package lambdanet.correctness

import ammonite.{ops => amm}
import lambdanet.translation.PredicateGraphLoader.libDefs
import org.scalatest.WordSpec

class InconsistentLibDefs extends WordSpec {
  "Boolean nodes have two versions in LibDefs" in {
    val dir = amm.pwd / "data" / "tests" / "c1"
    val (graph, _, results) = InputUtils.loadGraphAndPredict(dir)

    val checker = TypeChecker(graph, libDefs)
    val node14 = graph.nodes.find(x => x.fromProject && x.getId == 14).get
    val booleanNodeInConstraint = checker.subtypesToCheck.find(x => x._1.nameOpt.contains('Boolean) && x._2 == node14).get._1
    val booleanInConstraint = libDefs.nodeMapping(booleanNodeInConstraint).typeOpt.get
    val booleanInPrediction = results(node14).distr.find(_._2.showSimple.contains("Boolean")).get._2
    assert(booleanInConstraint == booleanInPrediction)
  }
}
