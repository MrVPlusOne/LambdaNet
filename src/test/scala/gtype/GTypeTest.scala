package gtype

import org.scalatest.{Matchers, WordSpec}
import GType.API._
import org.scalacheck.Prop.{forAll, BooleanOperators}
import GType._

class GTypeTest extends WordSpec with MyTest {
  "GType pretty print test cases" in {
    (("a" arrow "b") arrow ("c" arrow any)).prettyPrint shouldBe "(a->b)->c->?"
    val abcd = "a".arrow("b".arrow("c".arrow("d")))
    abcd.prettyPrint shouldBe "a->b->c->d"
    obj("f" -> abcd).prettyPrint shouldBe "[f: a->b->c->d]"
  }

  "Print random GTypes" in {
    forAll(GType.simpleGTypeGen) { t =>
      println(s"<${t.astSize}> $t")
      t == t
    }.check()
  }

  "Print random mini GTypes" in {
    checkProp(
      forAll(MiniLang.miniGTypeGen(anyRatio = 0.2)) { t =>
        println(t)
        t == t
      })
  }

  "the consistent relation" should {
    val typeGen = MiniLang.miniGTypeGen(anyRatio = 0.2)
    "be reflexive" in {
      checkProp(
        forAll(typeGen) { t =>
          consistent(t, t)
        })
    }

    "be symmetric" in {
      checkProp(
        forAll(typeGen, typeGen){ (t1, t2) =>
          consistent(t1, t2) == consistent(t2, t1)
        },
        p => p.withMaxSize(14)
      )
    }

    "hold for mask" in {
      checkProp(
        forAll(typeGen, typeGen){ (t1, t2) =>
          consistent(restrict(t1, t2), t1)
        }
      )
    }
  }
}
