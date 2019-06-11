package funcdiff

import ammonite.ops._
import lambdanet.ProjectPath
import lambdanet.translation.ImportsResolution.{ErrorHandler, PathMapping}
import lambdanet.translation.{
  IRTranslation,
  ImportsResolution,
  PLangTranslation,
  PredicateGraphTranslation,
  QLangTranslation
}
import lambdanet.translation.PredicateGraph.PNodeAllocator
import lambdanet.utils.ProgramParsing
import lambdanet.utils.ProgramParsing.GProject

object PrepareRepos {

  case class LibDefs(
      baseCtx: ImportsResolution.ModuleExports,
      libAllocator: PNodeAllocator,
      libExports: Map[ProjectPath, ImportsResolution.ModuleExports]
  )

  def parseLibDefs() = {
    val declarationsDir = pwd / up / "lambda-repos" / "declarations"

    val (baseCtx, libAllocator, _) = QLangTranslation.parseDefaultModule()
    val libExports = {
      //    val files = ls(declarationsDir).filter(_.last.endsWith(".d.ts"))
      println("parsing GModules...")
      val GProject(_, modules, mapping0) = ProgramParsing
        .parseGProjectFromRoot(declarationsDir, declarationFileMod = true)
      val nodeFileMap = {
        val nodeFiles = ls(declarationsDir / "node")
          .filter(_.last.endsWith(".d.ts"))
          .map { f =>
            RelPath(f.last.replace(".d.ts", ""))
          }
        nodeFiles.map { f =>
          f -> RelPath("node") / f
        }.toMap
      }
      val mapping = new PathMapping {
        def map(
            currentPath: ProjectPath,
            pathToResolve: ProjectPath
        ): ProjectPath = {
          nodeFileMap.getOrElse(
            pathToResolve,
            mapping0.map(currentPath, pathToResolve)
          )
        }

        def aliases: Map[ProjectPath, ProjectPath] = mapping0.aliases
      }

      println("parsing PModules...")
      val pModules = modules
        .map(m => PLangTranslation.fromGModule(m, libAllocator))
        .map(m => m.path -> m)
        .toMap

      println("imports resolution...")
      ImportsResolution.resolveExports(
        pModules,
        Map(),
        mapping,
        maxIterations = 5,
        errorHandler = ErrorHandler.recoveryHandler()
      )
    }
    println("Declaration files parsed.")
    LibDefs(baseCtx, libAllocator, libExports)
  }

  def prepareProject(libDefs: LibDefs, root: Path) = {
    import libDefs._

//    val libMapping = {
//      libExports.map {
//        case (p, _) => p.last -> (declarationsDir / p).relativeTo(root)
//      }
//    }

    val skipSet = Set("__tests__", "dist")
    def filterTests(path: Path): Boolean = {
      path.segments.forall(!skipSet.contains(_))
    }

    val p = ProgramParsing.parseGProjectFromRoot(root, filter = filterTests)
    val allocator = new PNodeAllocator(forLib = false)
    val irTranslator = new IRTranslation(allocator)
    val irModules = QLangTranslation
      .fromProject(
        p.modules,
        baseCtx,
        Map(),
        allocator,
        p.pathMapping
      )
      .values
      .map(irTranslator.fromQModule)
      .toVector
    val graph = PredicateGraphTranslation.fromIRModules(irModules)
    println(graph)
  }

  def main(args: Array[String]): Unit = {
    val loadFromFile = true

    val libDefsFile = pwd / up / "lambda-repos" / "libDefs.serialized"

    val libDefs = if(loadFromFile) {
      val read = SimpleMath.readObjectFromFile[LibDefs](libDefsFile.toIO)
      println(s"library definitions read from $libDefsFile")
      read
    } else {
      val defs = parseLibDefs()
      SimpleMath.saveObjectToFile(libDefsFile.toIO)(defs)
      println(s"library definitions saved to $libDefsFile")
      defs
    }

    val projectsDir = pwd / up / "lambda-repos" / "projects"

    (ls ! projectsDir).foreach(f => if (f.isDir) prepareProject(libDefs, f))
  }

}
