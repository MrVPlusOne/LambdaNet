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

    "test public-fragment" in {
      val graph = PredicateGraphLoader.load(amm.pwd / "data" / "tests" / "public-fragment").pGraph
      val defineRels = graph.predicates.collect { case p: DefineRel => p }
      graph.predicates.collect { case p: BinaryRel => p }.groupBy(_.category).foreach(_._2.foreach(println))
      defineRels.groupBy {
        case DefineRel(v, expr) => expr.getClass
      }.foreach(_._2.foreach(println))
      val methodCalls = PMethodCall.generate(defineRels, libDefs)
      methodCalls.foreach(println)
    }

    "test public" in {
      val graph = PredicateGraphLoader.load(amm.pwd / "data" / "tests" / "public").pGraph
      val defineRels = graph.predicates.collect { case p: DefineRel => p }
      val functionCalls = PFuncCall.generate(defineRels, libDefs)
      functionCalls.foreach(println)
    }
  }
}
