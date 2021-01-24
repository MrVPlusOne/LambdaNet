package lambdanet.correctness

import ammonite.{ops => amm}
import lambdanet.translation.PredicateGraph.{DefineRel, PMethodCall}
import lambdanet.translation.PredicateGraphLoader
import org.scalatest.WordSpec

class MethodCallTest extends WordSpec {
  "method calls" should {
    "ignore anonymous function" in {
      val graph = PredicateGraphLoader.load(amm.pwd / "data" / "tests" / "method-call").pGraph
      val defineRels = graph.predicates.collect { case p: DefineRel => p }
      graph.predicates.foreach(println)
      val methodCalls = PMethodCall.generate(defineRels)
      assert(methodCalls.size == 1)
      val PMethodCall(obj, _, _, _, _) = methodCalls.head
      assert(obj.name == "Add")
    }
  }
}
