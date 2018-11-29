package gtype

import org.scalatest.{Matchers, WordSpec}
import GType.API._
import org.scalacheck.Prop.forAll
import org.scalacheck.Test

class GTypeTest extends WordSpec with Matchers {
  "GType pretty print test cases" in {
    (("a" arrow "b") arrow ("c" arrow any)).prettyPrint shouldBe "(a->b)->c->?"
    val abcd = "a".arrow("b".arrow("c".arrow("d")))
    abcd.prettyPrint shouldBe "a->b->c->d"
    obj("f" -> abcd).prettyPrint shouldBe "[f: a->b->c->d]"
  }

  "Print random GTypes" in {
    val refl = forAll(GType.simpleGTypeGen()) { t =>
      println(t)
      t == t
    }

    val result = Test.check(Test.Parameters.default, refl)
    assert(result.passed, result.toString)
  }
}
