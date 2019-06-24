package lambdanet

import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import lambdanet.GType.API._
import lambdanet.GType._

class GTypeTest extends WordSpec with MyTest {
  "GType pretty print test cases" in {
    (List("a", "b") -: (List('c) -: any)).prettyPrint shouldBe "(a,b)->(c)->any"
    val abcd = List('a) -: List('b) -: List('c) -: 'd
    abcd.prettyPrint shouldBe "(a)->(b)->(c)->d"
    obj('f -> abcd).prettyPrint shouldBe "{f: (a)->(b)->(c)->d}"
  }

  "Print random GTypes" in {
    forAll(GType.simpleGTypeGen) { t =>
      println(s"<${t.astSize}> $t")
      t == t
    }.check()
  }

  "the consistent-subtyping relation" should {
    val (typeGen, contextGen) = (GType.simpleGTypeGen, GType.simpleContextGen)
    "be reflexive" in {
      checkProp(forAll(typeGen, contextGen) { (t, context) =>
        checkSubtype(t, t, context).nonEmpty
      })
    }

    "pass simple examples" in {
      object SamplePrograms {
        import lambdanet.Surface._
        /*
         * let Point = { x: int, moveX: int => Point }
         *
         * let mkPoint: int -> Point =
         *   (x: int) => { x: x0, moveX: (dx: int) => mkPoint (x0 + dx) }
         *
         * let Point2D = { x: int, moveX: int => Point2D, y: int, moveY: int => Point2D }
         * */

        val point = 'Point
        val point2D = 'Point2D
        val numArray = 'NumArray

        val typeContext = TypeContext(
          baseTypes = Set('int, 'number, 'string, boolType.id),
          typeUnfold = Map(
            point -> obj('x -> 'int, 'moveX -> (List('int) -: point)),
            'PointAlias -> obj(
              'x -> 'int,
              'moveX -> (List('int) -: 'PointAlias),
            ),
            point2D -> obj(
              'x -> 'int,
              'moveX -> (List('int) -: point2D),
              'y -> 'int,
              'moveY -> (List('int) -: point2D),
            ),
            numArray -> obj(
              'length -> 'int,
              'slice -> (List('int, 'int) -: numArray),
              'access -> (List('int) -: 'number),
              'push -> (List('number) -: numArray),
            ),
            'Comparator -> obj(
              'equal -> (List(any, any) -: boolType),
            ),
          ),
          subRel = Set(
            ('int: GType) -> 'number,
          ),
        )

        val exprContext: ExprContext = {
          val varAssign = Map[Symbol, GType](
            'eq -> (List(any, any) -: boolType),
            'not -> (List(any) -: boolType),
            'lt -> (List('int, 'int) -: boolType),
            'plus -> (List('int, 'int) -: 'int),
            'minus -> (List('int, 'int) -: 'int),
            'times -> (List('int, 'int) -: 'int),
            'divide -> (List('number, 'number) -: 'number),
            'floor -> (List('number) -: 'int),
            'emptyArray -> numArray,
          )

          ExprContext(varAssign, typeContext)
        }

        case class Example(program: BlockStmt, holeTypeMap: Map[GTHole, GType])
        // @formatter:on
      }
      import SamplePrograms._

      val context = typeContext

      assert(context.isSubtype(point2D, point))
      assert(context.isSubtype(point, 'PointAlias))
      assert(!context.isSubtype(point, point2D))
      assert(!context.isSubtype(obj('x -> "int", 'y -> "int"), point2D))
      assert(context.isSubtype(point2D, obj('x -> "int", 'y -> "int")))
    }
  }

  "intersectionS" should {
    val typeGen = GType.simpleGTypeGen
    "remain unchanged when intersect self" in {
      checkProp(forAll(typeGen) { r =>
        r.intersectS(r) === r
      })
    }

    "remain unchaged when intersect any" in {
      checkProp(forAll(typeGen) { r =>
        r.intersectS(AnyType) === r
      })
      checkProp(forAll(typeGen) { r =>
        AnyType.intersectS(r) === r
      })
    }

  }

}
