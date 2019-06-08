package lambdanet

import ammonite.ops.{Path, RelPath, pwd}
import lambdanet.Surface.GModule
import lambdanet.translation.ImportsResolution.PathMapping
import lambdanet.translation.OldPredicateGraphConstruction._
import lambdanet.utils.ProgramParsing
import lambdanet.utils.ProgramParsing.DeclarationModule

object TrainingProjects {

  case class Project(
      root: Path,
      modules: Vector[GModule],
      pathMapping: PathMapping
  )

  lazy val standardLibs: Vector[DeclarationModule] = parseStandardLibs()

  def parseStandardLibs(): Vector[DeclarationModule] = {
    val root = pwd / RelPath("data/libraries")
    val libFile = RelPath("default.lib.d.ts")
    val modules = {
      val parser = ProgramParsing
      val r = parser.parseGModulesFromFiles(Seq(libFile), root)
      r
    }
    modules.map { m =>
      ProgramParsing.extractDeclarationModule(m.stmts)
    }
  }

  val projectRoots: Vector[Path] = Vector(
    //testing
    "data/train/algorithms-test",
    "data/train/TypeScript-Algorithms-and-Data-Structures-master/ts",
    "data/train/philmander-typescript-algorithms/src",
    //training
    "data/train/typehtml-master/src",
    "data/train/mojiito-master/packages",
    "data/train/algorithms-train"
  ).map(p => pwd / RelPath(p))

  val allProjects: Vector[Project] = {
    val defaultMapping = PathMapping.identity
    val roots = Map[String, PathMapping](
      "data/train/algorithms-test" -> defaultMapping,
      "data/train/TypeScript-Algorithms-and-Data-Structures-master/ts" -> defaultMapping,
      "data/train/philmander-typescript-algorithms/src" -> defaultMapping,
      //training
      "data/train/typehtml-master/src" -> defaultMapping,
      "data/train/mojiito-master/packages" -> new PathMapping {
        def map(
            currentPath: ProjectPath,
            pathToResolve: ProjectPath
        ): ProjectPath = {
          val target = currentPath / pathToResolve
          if (target.startsWith(RelPath("core/src/facade"))) {
            RelPath("facade/src") / target.segments.drop(3)
          } else if (target
                       .startsWith(RelPath("platform-browser/src/facade"))) {
            RelPath("facade/src") / target.segments.drop(3)
          } else if (pathToResolve == RelPath("mojiito-facade")) {
            RelPath("facade/index")
          } else if (pathToResolve == RelPath("mojiito-core")) {
            RelPath("core/src/core")
          } else {
            target
          }
        }
      },
      "data/train/algorithms-train" -> defaultMapping
    )

    roots.map {
      case (s, pm) =>
        val path = pwd / RelPath(s)
        Project(path, ProgramParsing.parseGModulesFromRoot(path), pm)
    }.toVector
  }

  def parsedProjects: Vector[ParsedProject] = {
    projectRoots.map(r => {
      fromRootDirectory(
        r,
        pathMapping = new PathMapping {
          def map(
              currentPath: ProjectPath,
              pathToResolve: ProjectPath
          ): ProjectPath = {
            val target = currentPath / pathToResolve
            if (target.startsWith(RelPath("core/src/facade"))) {
              RelPath("facade/src") / target.segments.drop(3)
            } else if (target
                         .startsWith(RelPath("platform-browser/src/facade"))) {
              RelPath("facade/src") / target.segments.drop(3)
            } else if (pathToResolve == RelPath("mojiito-facade")) {
              RelPath("facade/index")
            } else if (pathToResolve == RelPath("mojiito-core")) {
              RelPath("core/src/core")
            } else {
              target
            }
          }
        }
      )
    })
  }
}
