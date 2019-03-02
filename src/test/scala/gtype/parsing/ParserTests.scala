package gtype.parsing

import gtype._
import org.scalatest.WordSpec
import ammonite.ops._

class ParserTests extends WordSpec with MyTest {
  def testParsing(printResult: Boolean)(pairs: (String, Class[_])*): Unit = {
    val (lines0, cls) = pairs.unzip
    val lines = lines0.toArray
    val parsed = GStmtParsing.parseContent(lines.mkString("\n"))
    parsed.zip(cls).zipWithIndex.foreach {
      case ((r, c), i) =>
        assert(
          r.getClass == c,
          s"Failed for: '${lines(i)}'. expect type: $c, but get value:\n${r.prettyPrint(indent = 1)}"
        )
        if (printResult) {
          println(s"'${lines(i)}' parsed as: \n${r.prettyPrint(indent = 1)}")
        }
    }
  }

  "Expressions parsing test" in {
    val content =
      """
        |3;
        |"abc";
        |true;
        |false;
        |null;
        |[1,2,3];
        |f(a);
        |new A(1,2);
      """.stripMargin

    GStmtParsing.parseContent(content)
  }

  "Statements parsing test" in {
    testParsing(printResult = true)(
      """let x: number = 4;""" -> classOf[VarDef],
      """const y = "yyy";""" -> classOf[VarDef],
      """let x = {a: 1, b: {c: "x"}};""" -> classOf[VarDef],
      """let one = {a: 1}.a;""" -> classOf[VarDef],
      """let a = array[1];""" -> classOf[VarDef],
      """a + b;""" -> classOf[ExprStmt],
      """a = b + 1;""" -> classOf[AssignStmt],
      """x = (y = 3);""" -> classOf[AssignStmt],
      """!(a==b);""" -> classOf[ExprStmt],
      """v++;""" -> classOf[ExprStmt],
      """(1+1==2)? good: bad;""" -> classOf[ExprStmt],
      """if(true) yes else no; """ -> classOf[IfStmt],
      """while(1+2==2) { "no escape"} """ -> classOf[WhileStmt],
      """{i++; let v = 2;}""" -> classOf[BlockStmt],
      """{++i; i++}""" -> classOf[BlockStmt],
      """for(let x = 0; x < 5; x ++) { print(x) }""" -> classOf[BlockStmt]
    )

  }
}
