package lambdanet.utils

import lambdanet._
import org.scalatest.WordSpec
import ammonite.ops._
import ImportStmt._
import funcdiff.SimpleMath
import lambdanet.NeuralInference.Predictor
import lambdanet.train.DataSet.selectLibTypes
import lambdanet.translation.ImportsResolution.ErrorHandler
import lambdanet.translation.PredicateGraph.{DefineRel, LibTypeNode, PNode, PNodeAllocator, PType}
import lambdanet.translation.QLangTranslation
import lambdanet.utils.ProgramParsing.ImportPattern
import lambdanet.utils.QLangDisplay.AnnotPlace

class ParserTests extends WordSpec with MyTest {
  def testParsing(printResult: Boolean)(pairs: (String, Class[_])*): Unit = {
    val (lines0, cls) = pairs.unzip
    val lines = lines0.toArray
    val parsed =
      ProgramParsing().parseContent(lines.mkString("\n"))
    parsed.zip(cls).zipWithIndex.foreach {
      case ((r, c), i) =>
//        assert(
//          r.getClass == c,
//          s"Failed for: '${lines(i)}'. expect type: $c, but get value:\n${r
//            .prettyPrint(indent = 1)}"
//        )
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

    val modules = ProgramParsing.parseGModulesFromFiles(
      files,
      projectRoot
    )
    modules.foreach { module =>
      println(s"=== module: '${module.moduleName}' ===")
      module.stmts.foreach(println)
    }
  }

//  "Simple cases parsing test" in {
//    val content =
//      """
//        |export default (x: number) => x;
//        |const {f: [a,{w:y}], onChange} = this.props;
//        |({reducers = {}}: {
//        |  reducers?: object;
//        |}) =>
//        |  createStore(
//        |    combineReducers(reducers),
//        |    state,
//        |    compose(applyMiddleware(...middleware))
//        |  );
//        |export default {x};
//        |declare type ClassDecorator = <TFunction extends Function>(target: TFunction) => TFunction | void;
//        |interface Test{
//        |  g<T>(x:T): T
//        |
//        |  fight(locales?: string | string[]): number
//        |}
//        |
//        |interface Test{
//        |  again(): string
//        |}
//        |namespace foo {
//        |   namespace bar {
//        |       let z = 5;
//        |   }
//        |}
//        |let y = foo.bar.z;
//        |let z = bar.foo.z;
//        |
//        |type A = (_: number) => number;
//        |[a, b[c]] = something;
//        |class Generics {
//        |  u: number;
//        |  id<T>(x: T): T {}
//        |  f(y: number): string {}
//        |}
//        |export interface ClassType<T> extends Function {
//        |  new (...args: Array<any>): T;
//        |  constructor: Function | any[];
//        |  [propertyName: string]: any;
//        |  name: string;
//        |}
//        |let x = {a: 1, b: {c: "x"}};
//        |let myAdd: (x: number, y: number) => number = undefined;
//        |let o: {b: any, a: number} = undefined;
//        |let x: any = undefined;
//        |3;
//        |-2;
//        |"abc";
//        |true;
//        |false;
//        |null;
//        |[1,2,3];
//        |f(a);
//        |new A(1,2);
//      """.stripMargin ++ Seq(
//        """let [a,b,c] = array;""",
//        """let {x,y} = {x: 10, y: 5};""",
//        """let {x,y} = o1""",
//        """let x: number = 4;""",
//        """const y = "yyy";""",
//        """let x = {a: 1, b: {c: "x"}};""",
//        """let one = {a: 1}.a;""",
//        """let a = array[1];""",
//        """a + b;""",
//        """a = b + 1;""",
//        """x = (y = 3);""",
//        """!(a==b);""",
//        """v++;""",
//        """(1+1==2)? good: bad;""",
//        """if(true) yes else no; """,
//        """while(1+2==2) { "no escape"} """,
//        """{i++; let v = 2;}""",
//        """{++i; i++}""",
//        """for(let x = 0; x < 5; x ++) { print(x) }""",
//        """break;""",
//        """function foo(bar: number, z): boolean {
//          |  return z;
//          |}""".stripMargin,
//        """class Bare {
//          | x: number;
//          | y(z: number): number{
//          |   return z;
//          | }
//        """.stripMargin,
//        "let inc = (x: number) => {return x + 1;};",
//        "let inc = x => x+1;",
//        "let f = (x) => (y) => x + y;",
////        """"switch(i){
////          |  case 1:
////          |    print(i); break;
////          |  case a:
////          |    a + 1;
////          |  default:
////          |    print("do nothing");
////          |}
////        """".stripMargin
//      ).mkString("\n")
//
//    val stmts = ProgramParsing().parseContent(content)
//    stmts.foreach(println)
//  }

  "JSon parsing tests" in {
    assert(Js.True == ProgramParsing.parseJson("""/* abc some @text */ true"""))
  }

  "Imports parsing tests" in {
    def test(text: String, target: Vector[ImportStmt]): Unit = {
      assert(
        ImportPattern.parseImports(text) === target,
        s"parsing failed for '$text'"
      )
    }

    def relPath(str: String) = ReferencePath(RelPath(str), isRelative = true)

    test(
      """import A1 from "./ZipCodeValidator";""",
      Vector(ImportDefault(relPath("./ZipCodeValidator"), 'A1))
    )
    test(
      """import * as pkg from "./ZipCodeValidator";""",
      Vector(ImportModule(relPath("./ZipCodeValidator"), 'pkg))
    )
    test(
      """import { ISessionEvent } from "../eme/session";""",
      Vector(
        ImportSingle('ISessionEvent, relPath("../eme/session"), 'ISessionEvent)
      )
    )
    test(
      """import {A,
        |B as B1} from "./ZipCodeValidator";""".stripMargin,
      Vector(
        ImportSingle('A, relPath("./ZipCodeValidator"), 'A),
        ImportSingle('B, relPath("./ZipCodeValidator"), 'B1)
      )
    )
    test(
      """import {
        |  A,
        |  B,
        |} from "./ZipCodeValidator";""".stripMargin,
      Vector(
        ImportSingle('A, relPath("./ZipCodeValidator"), 'A),
        ImportSingle('B, relPath("./ZipCodeValidator"), 'B)
      )
    )
    test(
      """import {foo as fool} from "./file1";""",
      Vector(ImportSingle('foo, relPath("./file1"), 'fool))
    )
    test("""import "./my-module.js";""", Vector())

  }

  "Standard lib parsing" in {
    val (_, _, mapping, _) = QLangTranslation.parseDefaultModule()
    println("standard lib mapping: ")
    mapping.foreach(println)
  }

  "Export Import tests" in {
    val root = pwd / RelPath("data/tests/export-import")

    val sources = ls
      .rec(root)
      .filter { f =>
        f.ext == "ts"
      }
      .map(_.relativeTo(root))
    val parser = ProgramParsing
    val modules = parser.parseGModulesFromFiles(
      sources,
      root
    )

//    QLangTranslation.fromProject(modules, )
  }

  ".d.ts files parsing" in {
    val (baseCtx, libAllocator, _, _) = QLangTranslation.parseDefaultModule()
    baseCtx.publicSymbols.foreach(println)
  }

  "import-unknowns tests" in {
    import PrepareRepos._

    val libDefs =
      announced(s"loading library definitions from $libDefsFile...") {
        SimpleMath.readObjectFromFile[LibDefs](libDefsFile.toIO)
      }

    // fixme: TestDef is missing when resolving types
    val f = pwd / RelPath("data/tests/export-import")
    lambdanet.shouldWarn = true
    val (g, _, _, _) =
      prepareProject(libDefs, f, skipSet = Set(), shouldPruneGraph = false)
    g.predicates.foreach(println)
    g.predicates.collect {
      case DefineRel(p, expr) if p.nameOpt.contains('a) =>
        assert(expr.asInstanceOf[PNode].fromProject)
    }
  }

  "predicate graph tests" in {
    import PrepareRepos._

    val libDefs =
      announced(s"loading library definitions from $libDefsFile...") {
        SimpleMath.readObjectFromFile[LibDefs](libDefsFile.toIO)
      }

    val dir = pwd / RelPath("data/tests/public")
    val (g, qModules, irModules, annots) =
      prepareProject(libDefs, dir, skipSet = Set())
    val truth = annots.map { case (k, v) => k.n -> v }
    val projName = "public"

    QLangDisplay.renderProjectToDirectory(projName, qModules, truth, Set())(dir / "predictions")

    val rightPreds: Set[AnnotPlace] = truth.map{ case (k, v) => (k, v, projName) }.toSet
    QLangDisplay.renderPredictionIndexToDir(rightPreds, rightPreds, dir, "predictions")

    irModules.foreach { m =>
      SequenceModel
        .tokenizeModule(m, libDefs.nodeMapping ++ m.mapping)
        .tap(println)
    }

    g.predicates.toVector.sortBy(_.toString).foreach(println)

    println {
      PredicateGraphVisualization.asMamGraph(
        libDefs,
        annots,
        "\"SpringElectricalEmbedding\"",
        g
      )
    }

    val libTypesToPredict: Set[LibTypeNode] =
      selectLibTypes(libDefs, Seq(annots), coverageGoal = 0.95)

    println {
      Predictor(
        g,
        libTypesToPredict,
        libDefs,
        None
      ).visualizeNeuralGraph
    }

  }
}
