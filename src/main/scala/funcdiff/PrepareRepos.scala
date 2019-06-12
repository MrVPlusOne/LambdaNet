package funcdiff

import ammonite.ops._
import lambdanet.ProjectPath
import lambdanet.translation.ImportsResolution.{
  ErrorHandler,
  ModuleExports,
  PathMapping
}
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

@SerialVersionUID(1)
case class LibDefs(
    baseCtx: ImportsResolution.ModuleExports,
    libAllocator: PNodeAllocator,
    libExports: Map[ProjectPath, ImportsResolution.ModuleExports]
)

object PrepareRepos {

  def parseLibDefs() = {
    val declarationsDir = pwd / up / "lambda-repos" / "declarations"

    val (baseCtx, libAllocator, _) = QLangTranslation.parseDefaultModule()
    val libExports = {
      //    val files = ls(declarationsDir).filter(_.last.endsWith(".d.ts"))
      println("parsing GModules...")
      val GProject(_, modules, mapping0, subProjects) = ProgramParsing
        .parseGProjectFromRoot(declarationsDir, declarationFileMod = true)
      val extraFilesMap = {
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
          extraFilesMap.getOrElse(
            pathToResolve,
            mapping0.map(currentPath, pathToResolve)
          )
        }

        def aliases: Map[ProjectPath, ProjectPath] = mapping0.aliases
      }

      println("parsing PModules...")
      val pModules =
        modules.map(m => PLangTranslation.fromGModule(m, libAllocator))

      println("imports resolution...")
      val exports = ImportsResolution.resolveExports(
        pModules,
        Map(),
        mapping,
        maxIterations = 5,
        errorHandler = ErrorHandler.recoveryHandler()
      )

      val namedExports = subProjects.map {
        case (name, path) =>
          name -> exports.getOrElse(
            path,
            exports.getOrElse(
              path / "index", {
                Console.err.println(
                  s"Couldn't find Exports located at $path for $name, ignore this named project."
                )
                ModuleExports.empty
              }
            )
          )
      }
      exports ++ namedExports
    }
    println("Declaration files parsed.")
    LibDefs(baseCtx, libAllocator, libExports)
  }

  def prepareProject(libDefs: LibDefs, root: Path) = {
    import libDefs._

    val skipSet = Set("__tests__", "dist")
    def filterTests(path: Path): Boolean = {
      path.segments.forall(!skipSet.contains(_))
    }

    val p = ProgramParsing.parseGProjectFromRoot(root, filter = filterTests)
    val allocator = new PNodeAllocator(forLib = false)
    val irTranslator = new IRTranslation(allocator)

    println(s"LibExports key set: ${libExports.keySet}")
    val irModules = QLangTranslation
      .fromProject(
        p.modules,
        baseCtx,
        libExports,
        allocator,
        p.pathMapping
      )
      .map(irTranslator.fromQModule)
    val graph = PredicateGraphTranslation.fromIRModules(irModules)
    println(graph)
  }

  def main(args: Array[String]): Unit = {
    val loadFromFile = true

    val libDefsFile = pwd / up / "lambda-repos" / "libDefs.serialized"

    val libDefs = if (loadFromFile) {
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
