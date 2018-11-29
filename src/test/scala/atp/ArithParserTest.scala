package atp

import atp.ArithExpr._
import org.scalacheck.Prop.forAll
import org.scalacheck.Test
import org.scalatest.{FlatSpec, Matchers, WordSpec}

class ArithParserTest extends WordSpec with Matchers {

  "random arith expressions when pretty printed and parsed" should {
    "equals to themselves" in {
      val parsePrettyPrint = forAll(arithGen) { a: Expr =>
        ArithParser.parseExpr(a.show) == a
      }

      val result = Test.check(Test.Parameters.default, parsePrettyPrint)
      assert(result.passed, result.toString)
    }
  }
}
