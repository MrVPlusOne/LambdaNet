package gtype

import org.scalatest.WordSpec
import GType.API._
import GExpr.API._
import GExpr.boolType

class GExprTest extends WordSpec with MyTest {
  "sample programs" should {

//    "type check" in {
//      SamplePrograms.WellFormed.all.foreach{ case (name, program, expectType) =>
//        val (t, errors) = GExpr.typeCheckInfer(program, SamplePrograms.exprContext)
//        assert(errors.isEmpty, s"errors in example $name")
//        assert(t === expectType, s"errors in example $name")
//      }
//    }
  }
}