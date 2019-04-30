package gtype

import org.scalatest.{Matchers, WordSpec}
import GType.API._
import org.scalacheck.Prop.{BooleanOperators, forAll}
import GType._

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
      import SamplePrograms._
      val context = typeContext

      assert(context.isSubtype(point2D, point))
      assert(context.isSubtype(point, 'PointAlias))
      assert(!context.isSubtype(point, point2D))
      assert(!context.isSubtype(obj('x -> "int", 'y -> "int"), point2D))
      assert(context.isSubtype(point2D, obj('x -> "int", 'y -> "int")))
    }
  }

  "type depth" should {
    import gtype.TypeAliasGraph._

    "properly deal with recursive types" in {
      val aliasGraph = Map[Symbol, TypeAliasing](
        'A1 -> ObjectAliasing(
          Map(
            'A1Filed -> 'A1
          )
        ),
        'B2 -> ObjectAliasing(
          Map(
            'B2Field -> 'B2, //depth 1
            'A1Ref -> 'A1 //depth 2
          )
        ),
        'C2 -> ObjectAliasing(
          Map(
            'D2Ref -> 'D2
          )
        ),
        'D2 -> ObjectAliasing(
          Map(
            'C2ref -> 'C2
          )
        )
      )

      def depth(n: Symbol) = typeDepth(n, aliasGraph)
      depth('A1) shouldBe 1
      depth('B2) shouldBe 2
      depth('C2) shouldBe 2
      depth('D2) shouldBe 2
    }
  }
}
