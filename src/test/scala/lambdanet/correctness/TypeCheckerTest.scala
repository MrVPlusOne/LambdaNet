package lambdanet.correctness

import ammonite.{ops => amm}
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph._
import lambdanet.translation.PredicateGraphLoader.libDefs
import org.scalatest.WordSpec

/**
  * Adapted from GTypeTest
  */
class TypeCheckerTest extends WordSpec {
  "the consistent-subtyping relation" should {
    "pass simple examples" in {
      // todo (Jiayi): Add tests for library types and basic types
      import TypeCheckerTest.SimpleProgram._
      val context = PTypeContext(graph, libDefs)
      val checker = TypeChecker(graph, libDefs)
      val correct = nodes.zip(types).toMap
      assert(context.isSubtype(pointNode, pointAliasNode, assignment = correct))
      assert(context.isSubtype(pointAliasNode, pointNode, assignment = correct))
      assert(context.isSubtype(point2DNode, pointNode, assignment = correct))
      assert(!context.isSubtype(pointNode, point2DNode, assignment = correct))
      assert(!context.isSubtype(pointNode, intNode, assignment = correct))
      assert(checker.violate(correct).isEmpty)

      val pointAliasIsInt = correct.updated(pointAliasNode, intType)
      assert(
        checker.violate(pointAliasIsInt) == Set(
          (pointNode, pointAliasNode)
        )
      )
    }

    "check library type in data/tests/simple" in {
      val inputPath = amm.pwd / "data" / "tests" / "simple"
      val (graph, _, results) = InputUtils.loadGraphAndPredict(inputPath)
      val node32 = graph.nodes.find(_.getId == 32).get
      println(results(node32).distr.filter(_._2.showSimple.contains("String")))
      val arrayType = results(node32).distr(2)._2
      val binaryRels = graph.predicates.collect {
        // inheritance is always satisfied (from definition)
        case p: BinaryRel if p.category != BinaryRelCat.inheritance => p
      }
      val stringNode =
        binaryRels.find(x => x.lhs == node32 && x.rhs.name == "String").get.rhs
      val pTypeContext = PTypeContext(graph, libDefs)
      val mostLikely = results
        .mapValuesNow(_.topValue)
      libDefs.nodeMapping
        .filter {
          case (n, annot) =>
            n.nameOpt.exists(_.name.contains("{String}"))
        }
        .foreach { case (n, annot) => println(n, annot) }
      assert(pTypeContext.isSubtype(node32, stringNode, mostLikely))
    }
  }
}

object TypeCheckerTest {
  class NodeFactory {
    var counter = 0
    def makeNode(symbol: Symbol): PNode = {
      counter += 1
      PNode(counter, Some(symbol), isType = true, fromLib = false)
    }

    def makeNodes(symbols: List[Symbol]): List[PNode] =
      symbols.map(makeNode)
  }

  object SimpleProgram {
    val point = 'Point
    val pointAlias = 'PointAlias
    val point2D = 'Point2D
    val int = 'int
    val pointMutator = 'PointMutator
    val pointAliasMutator = 'PointAliasMutator
    val point2DMutator = 'Point2DMutator

    val factory = new NodeFactory
    val nodes @ List(
      intNode,
      pointNode,
      pointAliasNode,
      point2DNode,
      pointMutatorNode,
      pointAliasMutatorNode,
      point2DMutatorNode
    ) =
      PNode(1, Some(int), isType = true, fromLib = true) ::
        factory.makeNodes(
          List(
            point,
            pointAlias,
            point2D,
            pointMutator,
            pointAliasMutator,
            point2DMutator
          )
        )
    val types @ pointType :: pointAliasType :: point2DType :: intType :: _ =
      nodes.map(PTyVar)
    val graph = PredicateGraph(
      nodes = nodes.toSet,
      predicates = Set(
        BinaryRel(pointNode, pointAliasNode, BinaryRelCat.equal),
        BinaryRel(point2DNode, pointNode, BinaryRelCat.subtype),
        DefineRel(
          pointNode,
          PObject(Map('x -> intNode, 'moveX -> pointMutatorNode))
        ),
        DefineRel(
          pointAliasNode,
          PObject(Map('x -> intNode, 'moveX -> pointAliasMutatorNode))
        ),
        DefineRel(
          point2DNode,
          PObject(
            Map(
              'x -> intNode,
              'moveX -> point2DMutatorNode,
              'y -> intNode,
              'moveY -> point2DMutatorNode,
            )
          )
        ),
        DefineRel(
          pointMutatorNode,
          PFunc(Vector(intNode), pointNode)
        ),
        DefineRel(
          pointAliasMutatorNode,
          PFunc(Vector(intNode), pointAliasNode)
        ),
        DefineRel(
          point2DMutatorNode,
          PFunc(Vector(intNode), point2DNode)
        )
      )
    )
  }
}
