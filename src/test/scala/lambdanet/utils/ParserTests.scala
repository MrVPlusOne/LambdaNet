package lambdanet.utils

import lambdanet._
import org.scalatest.WordSpec
import ammonite.ops._
import ImportStmt._
import funcdiff.SimpleMath
import lambdanet.NeuralInference.Predictor
import lambdanet.train.DataSet.selectLibTypes
import lambdanet.train.TopNDistribution
import lambdanet.translation.ImportsResolution.ErrorHandler
import lambdanet.translation.PredicateGraph.{
  DefineRel,
  LibTypeNode,
  PNode,
  PNodeAllocator,
  PType
}
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
    val parsed =
      prepareProject(libDefs, f/up, f, skipSet = Set(), shouldPruneGraph = false)
    val g = parsed.pGraph
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

    val dir = pwd / RelPath(
      "data/tests/weirdInterfaces"
    )
    val parsed@ParsedProject(_, qModules, irModules, g) =
      prepareProject(
        libDefs,
        dir/up,
        dir,
        skipSet = Set(),
        errorHandler =
          ErrorHandler(ErrorHandler.StoreError, ErrorHandler.StoreError),
        shouldPrintProject = true
      ).mergeEqualities
    SM.saveObjectToFile((pwd/"data"/"testSerialization.serialized").toIO)(parsed)
    val annots = parsed.allUserAnnots
    val truth = annots.map { case (k, v) => k.n -> v }
    val projName = "gigobyte_ui-stack"

    QLangDisplay.renderProjectToDirectory(projName, qModules, truth.map {
      case (k, v) => k -> TopNDistribution(Vector(1.0 -> v))
    }, Set())(
      dir / "predictions"
    )

    val rightPreds: Set[AnnotPlace] = truth.map {
      case (k, v) => (k, v, RelPath(projName))
    }.toSet
    QLangDisplay.renderPredictionIndexToDir(
      rightPreds,
      rightPreds,
      dir,
      "predictions"
    )

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
        projName,
        g,
        libTypesToPredict,
        libDefs,
        None,
      ).visualizeNeuralGraph
    }

  }
}
