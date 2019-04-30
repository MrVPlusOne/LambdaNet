package gtype

import org.scalacheck.{Prop, Test}
import org.scalatest.{Assertion, Matchers}

trait MyTest extends Matchers {
  def checkProp(
      prop: Prop,
      paramTrans: Test.Parameters => Test.Parameters = identity
  ): Assertion = {
    val p = paramTrans(Test.Parameters.default.withMaxSize(50))
    val result = Test.check(p, prop)
    assert(result.passed, result.toString)
  }
}
