package gtype

import org.scalatest.WordSpec
import GStmt.API._

class GProgramTest extends WordSpec with MyTest {
  "function applications" should {
    "be able to omit some arguments" in {
      import JSExamples.{number, exprContext, boolean, string, void}

      assert{
        GStmt.typeCheckBlock(
          BLOCK(
            'f.call('x, 'y, 'z),
            'f.call('x, 'y),
            'f.call('x),
            'f.call()
          ),
          ctx = exprContext.
            newVar('f, List(number, string, boolean) -: number).
            newVar('x, number).newVar('y, string).newVar('z, boolean),
          returnType = void
        ).isEmpty
      }
    }
  }

//  "sample programs" should {
//
//    "type check" in {
//      SamplePrograms.Collection.all.foreach { case (name, example) =>
//        println(s"test type checking example: $name")
//
//        val errors = GStmt.typeCheckBlock(example.program, SamplePrograms.exprContext, 'int)
//        assert(errors === example.errors, s"errors mismatch in example $name")
//      }
//    }
//  }

//  "JS examples" should {
//    "type check" in {
//      JSExamples.Collection.all.foreach { case (name, example) =>
//        println(s"test type checking example: $name")
//        val errors = GStmt.typeCheckBlock(example.program, JSExamples.exprContext, 'void)
//        assert(errors === example.errors, s"errors mismatch in example $name")
//      }
//    }
//  }
}