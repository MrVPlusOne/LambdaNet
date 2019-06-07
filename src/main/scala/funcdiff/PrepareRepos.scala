package funcdiff

import ammonite.ops._
import lambdanet.ProjectPath
import lambdanet.TrainingProjects.Project
import lambdanet.translation.ImportsResolution.PathMapping
import lambdanet.translation.{
  IRTranslation,
  ImportsResolution,
  PLangTranslation,
  PredicateGraphTranslation,
  QLangTranslation
}
import lambdanet.translation.PredicateGraph.PNodeAllocator
import lambdanet.utils.ProgramParsing

object PrepareRepos {
  val declarationsDir = pwd / up / "lambda-repos" / "declarations"

  val (baseCtx, libAllocator, _) = QLangTranslation.parseDefaultModule()
  val libExports = {
//    val files = ls(declarationsDir).filter(_.last.endsWith(".d.ts"))
    val pModules = ProgramParsing
      .parseGModulesFromRoot(declarationsDir, allowDeclarationFiles = true)
      .map(m => PLangTranslation.fromGModule(m, libAllocator))
      .map(m => m.path -> m)
      .toMap

    ImportsResolution.resolveExports(
      pModules,
      Map(),
      PathMapping.identity,
      maxIterations = 1
    )
  }
  println("Declaration files parsed.")

  def prepareProject(root: Path) = {
    val libMapping = {
      libExports.map {
        case (p, _) => p.last -> (declarationsDir / p).relativeTo(root)
      }
    }

    def parseMapping() = {
      val mapping = (for {
        f <- ls.rec(root) if f.last == "package.json"
        mn <- parsePackageFile(f).moduleName
      } yield {
        mn -> (f / up / "src" / "index").relativeTo(root)
      }).toMap
      new PathMapping {
        def map(
            currentPath: ProjectPath,
            pathToResolve: ProjectPath
        ): ProjectPath = {
          mapping.get(pathToResolve) match {
            case Some(index) => index
            case _           => currentPath / pathToResolve
          }
        }
      }
    }

    def filterTests(path: Path): Boolean = {
      !path.segments.contains("__tests__")
    }

    val mapping = parseMapping()
    val modules =
      ProgramParsing.parseGModulesFromRoot(root, filter = filterTests)
    val p = Project(root, modules, mapping)
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
  }

  def main(args: Array[String]): Unit = {
    val projectsDir = pwd / up / "lambda-repos" / "projects"

    (ls ! projectsDir).foreach(f => if (f.isDir) prepareProject(f))
  }

  case class PackageFile(
      moduleName: Option[ProjectPath]
  )
  def parsePackageFile(path: Path): PackageFile = {
    import ProgramParsing._
    val map = asObj(parseJsonFromFile(path))
    PackageFile(
      moduleName = map.get("name").map(s => RelPath(asString(s)))
    )
  }
}
