package gtype.parsing

import gtype._
import org.scalatest.WordSpec
import ammonite.ops._
import infer.IRTranslation.{TranslationEnv, translateStmt}
import infer.{IRTranslation, PredicateGraphConstruction}
import ImportStmt._
import gtype.GModule.ProjectPath
import infer.PredicateGraphConstruction.PathMapping

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
    val parsed =
      new ProgramParsing().parseContent(lines.mkString("\n"))
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
    val files = ls
      .rec(projectRoot)
      .filter(_.ext == "ts")
      .map(_.relativeTo(projectRoot))

    val modules = new ProgramParsing().parseModulesFromFiles(
      files,
      Set(),
      projectRoot,
      Some(projectRoot / "parsed.json")
    )
    modules.foreach { module =>
      println(s"=== module: '${module.moduleName}' ===")
      module.stmts.foreach(println)
    }

    val modules2 = new ProgramParsing()
      .parseModulesFromJson(read(projectRoot / "parsed.json"))
    assert(modules == modules2, "Two parses do not match.")
  }

  "Expressions parsing test" in {
    val content =
      """
        |class Generics {
        |  id<T>(x: T): T {}
        |}
        |let x = {a: 1, b: {c: "x"}};
        |let myAdd: (x: number, y: number) => number = undefined;
        |let o: {b: any, a: number} = undefined;
        |let x: any = undefined;
        |3;
        |-2;
        |"abc";
        |true;
        |false;
        |null;
        |[1,2,3];
        |f(a);
        |new A(1,2);
      """.stripMargin

    val stmts = new ProgramParsing().parseContent(content)
    stmts.foreach(println)
    val env = new TranslationEnv()

    val irStmts = stmts.flatMap(s => IRTranslation.translateStmt(s)(Set(), env))
    println("=== IR ===")
    irStmts.foreach(println)
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

  "Imports parsing tests" in {
    def test(text: String, target: Vector[ImportStmt]): Unit = {
      assert(ImportPattern.parseImports(text) === target, s"parsing failed for '$text'")
    }

    test(
      """import A1 from "./ZipCodeValidator";""",
      Vector(ImportDefault(RelPath("./ZipCodeValidator"), 'A1))
    )
    test(
      """import * as pkg from "./ZipCodeValidator";""",
      Vector(ImportModule(RelPath("./ZipCodeValidator"), 'pkg))
    )
    test(
      """import {A,
        |B as B1} from "./ZipCodeValidator";""".stripMargin,
      Vector(
        ImportSingle('A, RelPath("./ZipCodeValidator"), 'A),
        ImportSingle('B, RelPath("./ZipCodeValidator"), 'B1)
      )
    )
    test(
      """import {
        |  A,
        |  B,
        |} from "./ZipCodeValidator";""".stripMargin,
      Vector(
        ImportSingle('A, RelPath("./ZipCodeValidator"), 'A),
        ImportSingle('B, RelPath("./ZipCodeValidator"), 'B)
      )
    )
    test(
      """import {foo as fool} from "./file1";""",
      Vector(ImportSingle('foo, RelPath("./file1"), 'fool))
    )
    test("""import "./my-module.js";""", Vector())

  }

  "Export Import tests" in {
    val root = pwd / RelPath("data/tests/export-import")
    infer.PredicateGraphConstruction
          .fromSourceFiles(root)
      .predModules
      .foreach { m =>
        println(m.display)
      }
  }

  "LibConst parsing tests" in {
    val root = pwd / RelPath("data/tests/lib-const")
    infer.PredicateGraphConstruction
      .fromSourceFiles(root)
      .predModules
      .foreach { m =>
        println(m.display)
      }
  }

  "EnvDef parsing" in {
    val r = %%('node, "./getEnvDefs.js", "String;")(pwd / 'scripts / 'ts)
    println(r)
  }

  "Project parsing integration test" in {
    val projectRoots = Seq(
      "data/train/mojiito-master/packages",
      "data/train/algorithms-train"
    ).map(p => pwd / RelPath(p))

    projectRoots.foreach(r => {
      infer.PredicateGraphConstruction
        .fromSourceFiles(r, pathMapping = new PathMapping {
          def map(currentPath: ProjectPath, pathToResolve: ProjectPath): ProjectPath = {
            val target = currentPath / pathToResolve
            if(target.startsWith(RelPath("core/src/facade"))){
              RelPath("facade/src") / target.segments.drop(3)
            }else{
              target
            }
          }
        })
    })

  }
}
