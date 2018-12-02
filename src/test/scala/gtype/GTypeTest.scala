package gtype

import org.scalatest.{Matchers, WordSpec}
import GType.API._
import org.scalacheck.Prop.{forAll, BooleanOperators}
import GType._

class GTypeTest extends WordSpec with MyTest {
  "GType pretty print test cases" in {
    (List("a", "b") -: (List('c) -: any)).prettyPrint shouldBe "(a,b)->(c)->*"
    val abcd = List("a") -: List("b") -: List("c") -: 'd
    abcd.prettyPrint shouldBe "(a)->(b)->(c)->d"
    obj('f -> abcd).prettyPrint shouldBe "{f: (a)->(b)->(c)->d}"
  }

  "Print random GTypes" in {
    forAll(GType.simpleGTypeGen) { t =>
      println(s"<${t.astSize}> $t")
      t == t
    }.check()
  }

  "Print random mini GTypes" in {
    checkProp(
      forAll(MiniLang.miniGen(anyRatio = 0.2)._1) { t =>
        println(t)
        t == t
      })
  }

  "the consistent-subtyping relation" should {
    val (typeGen, contextGen) = MiniLang.miniGen(anyRatio = 0.2)
    "be reflexive" in {
      checkProp(
        forAll(typeGen, contextGen) { (t, context) =>
          checkSubType(t, t, context).nonEmpty
        })
    }

    "pass simple examples" in {
      import SamplePrograms._
      val context = typeContext

      assert(checkSubType(point2D, point, context).nonEmpty)
      assert(checkSubType(point, point2D, context).isEmpty)
      assert(checkSubType(obj('x -> "int", 'y -> "int"), point2D, context).isEmpty)
      assert(checkSubType(point2D, obj('x -> "int", 'y -> "int"), context).nonEmpty)
    }
  }
}
