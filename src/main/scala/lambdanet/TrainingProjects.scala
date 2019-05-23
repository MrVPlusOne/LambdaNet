package lambdanet

import ammonite.ops.{RelPath, pwd}
import lambdanet.translation.PredicateGraphConstruction._
import lambdanet.utils.ProgramParsing
import lambdanet.utils.ProgramParsing.DeclarationModule

object TrainingProjects {

  lazy val standardLibs: Vector[DeclarationModule] = parseStandardLibs()

  def parseStandardLibs(): Vector[DeclarationModule] = {
    val root = pwd / RelPath("data/libraries")
    val libFile = RelPath("default.lib.d.ts")
    val modules = {
      val parser = new ProgramParsing()
      val r = parser.parseModulesFromFiles(Seq(libFile), Set(libFile), root)
      assert(parser.tHoleContext.holeTypeMap.isEmpty)
      r
    }
    modules.toVector.map { m =>
      ProgramParsing.extractDeclarationModule(m)
    }
  }

  val projectRoots = Vector(
    //testing
    "data/train/algorithms-test",
    "data/train/TypeScript-Algorithms-and-Data-Structures-master/ts",
    "data/train/philmander-typescript-algorithms/src",
    //training
    "data/train/typehtml-master/src",
    "data/train/mojiito-master/packages",
    "data/train/algorithms-train"
  ).map(p => pwd / RelPath(p))

  def parsedProjects: Vector[ParsedProject] = {
    projectRoots.map(r => {
      fromRootDirectory(
        r,
        pathMapping = new PathMapping {
          def map(currentPath: ProjectPath, pathToResolve: ProjectPath): ProjectPath = {
            val target = currentPath / pathToResolve
            if (target.startsWith(RelPath("core/src/facade"))) {
              RelPath("facade/src") / target.segments.drop(3)
            } else if (target.startsWith(RelPath("platform-browser/src/facade"))) {
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
