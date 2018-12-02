package gtype

import org.scalatest.WordSpec
import GStmt.API._

class GProgramTest extends WordSpec with MyTest {
  "sample programs" should {

    "type check" in {
      SamplePrograms.WellFormed.all.foreach{ case (name, program) =>
        val (_, errors) = GStmt.typeCheckStmt(program, SamplePrograms.exprContext, 'int)
        assert(errors.isEmpty, s"errors in example $name")
      }
    }
  }
}