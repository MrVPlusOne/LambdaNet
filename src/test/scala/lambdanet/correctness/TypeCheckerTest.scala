package lambdanet.correctness

import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph._
import org.scalatest.WordSpec

/**
  * Adapted from GTypeTest
  */
class TypeCheckerTest extends WordSpec {
  "the consistent-subtyping relation" should {
    "pass simple examples" in {
      import TypeCheckerTest.SimpleProgram._
      val context = PTypeContext(graph)
      val checker = TypeChecker(graph)
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