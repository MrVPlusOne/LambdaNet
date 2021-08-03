package lambdanet.correctness

import ammonite.{ops => amm}
import lambdanet.translation.PredicateGraph.{BinaryRel, BinaryRelCat}
import lambdanet.translation.PredicateGraphLoader.libDefs
import org.scalatest.WordSpec

class StringNodeBug extends WordSpec {
  "the string node should have string type in LibDefs" in {
    val inputPath = amm.pwd / "data" / "tests" / "simple"
    val (graph, _, results) = InputUtils.loadGraphAndPredict(inputPath)
    val node32 = graph.nodes.find(_.getId == 32).get
    val stringNode = graph.nodes.find(_.nameOpt.contains('String)).get
    val binaryRels = graph.predicates.collect {
      // inheritance is always satisfied (from definition)
      case p: BinaryRel if p.category != BinaryRelCat.inheritance => p
    }
    val constraint = binaryRels.find(x => x.lhs == node32 && x.rhs.name == "String").get
    println("The required constraint:")
    println(constraint)

    val predicted = results(node32).distr.filter(_._2.showSimple.contains("String"))
    println(s"All types predicted for $node32 that have 'String' in the name:")
    println(predicted)
    val given = libDefs.nodeMapping(stringNode).typeOpt.get
    println(s"The type given by $stringNode in LibDefs:")
    println(given)
    assert(predicted.exists(_._2 == given))
  }
}
