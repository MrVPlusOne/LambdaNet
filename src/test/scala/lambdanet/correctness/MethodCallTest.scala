package lambdanet.correctness

import ammonite.{ops => amm}
import lambdanet.translation.PredicateGraph.{BinaryRel, DefineRel, PFuncCall, PMethodCall}
import lambdanet.translation.PredicateGraphLoader
import lambdanet.translation.PredicateGraphLoader.libDefs
import org.scalatest.WordSpec

class MethodCallTest extends WordSpec {
  "method calls" should {
    "ignore anonymous function" in {
      val graph = PredicateGraphLoader.load(amm.pwd / "data" / "tests" / "method-call").pGraph
      val defineRels = graph.predicates.collect { case p: DefineRel => p }
      graph.predicates.foreach(println)
      val methodCalls = PMethodCall.generate(defineRels, libDefs)
      assert(methodCalls.size == 1)
      assert(methodCalls.head.obj.name == "Add")
    }
  }
}
