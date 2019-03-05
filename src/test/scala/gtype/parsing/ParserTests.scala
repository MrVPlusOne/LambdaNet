package gtype.parsing

import gtype._
import org.scalatest.WordSpec
import ammonite.ops._

class ParserTests extends WordSpec with MyTest {
  def testParsing(printResult: Boolean)(pairs: (String, Class[_])*): Unit = {
//    for ((source, cls) <- pairs) {
//      val parsed = GStmtParsing.parseContent(source)
//      assert(parsed.zip(cls).forall{ case (s, c) => s.getClass == c},
//        s"Failed for: '$source'. expect types: $cls, but get values:\n"+
//          parsed.map(_.prettyPrint(indent = 1)).mkString("\n")
//      )
//    }
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

  "Source file parsing test" in {
    val projectRoot = pwd / RelPath("data/ts-algorithms")
    val files = ls.rec(projectRoot)
      .filter(_.ext == "ts")
      .map(_.relativeTo(projectRoot))

    val modules = GStmtParsing.parseModulesFromFiles(files, Set(), projectRoot, Some(projectRoot / "parsed.json"))
    modules.foreach { module =>
      println(s"=== module: '${module.moduleName}' ===")
      module.stmts.foreach(println)
    }

    val modules2 = GStmtParsing.parseModulesFromJson(read(projectRoot / "parsed.json"))
    assert(modules == modules2, "Two parses do not match.")
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
      """for(let x = 0; x < 5; x ++) { print(x) }""" -> classOf[BlockStmt],
      """break;""" -> classOf[CommentStmt],
      """function foo(bar: number, z): boolean {
        |  return z;
        |}
      """.stripMargin -> classOf[FuncDef],
      """class Test1 {
        |  x;
        |  constructor(y: number){
        |    this.x = y;
        |  }
        |
        |  m1(x: boolean){
        |    this.x = x;
        |  }
        |}
      """.stripMargin -> classOf[ClassDef],
      """class Bare {
        | x: number;
        | y(z: number): number{
        |   return z;
        | }
      """.stripMargin -> classOf[ClassDef],
      "let inc = (x: number) => {return x + 1;};" -> classOf[BlockStmt],
      "let inc = x => x+1;" -> classOf[BlockStmt],
      "let f = (x) => (y) => x + y;" -> classOf[BlockStmt],
      "let {x,y} = {x: 10, y: 5};" -> classOf[BlockStmt],
      """switch(i){
        |  case 1:
        |    print(i); break;
        |  case a: a + 1;
        |  default:
        |    print("do nothing");
        |}
      """.stripMargin -> classOf[BlockStmt]
    )

  }
}
