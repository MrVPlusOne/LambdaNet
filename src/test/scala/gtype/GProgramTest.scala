package gtype

import org.scalatest.WordSpec
import GStmt.API._

class GProgramTest extends WordSpec with MyTest {
  "sample programs" should {

    "type check" in {
      SamplePrograms.Collection.all.foreach{ case (name, example) =>
        val errors = GStmt.typeCheckBlock(example.program, SamplePrograms.exprContext, 'int)
        assert(errors === example.errors, s"errors mismatch in example $name")
      }
    }
  }
}