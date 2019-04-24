package infer

import ammonite.ops.{RelPath, pwd}
import gtype.GModule.ProjectPath
import infer.PredicateGraphConstruction.PathMapping

object TrainingProjects {
  val projectRoots = Vector(
    "data/train/TypeScript-Algorithms-and-Data-Structures-master/ts",
    "data/train/philmander-typescript-algorithms/src",
    "data/train/typehtml-master/src",
    "data/train/mojiito-master/packages",
    "data/train/algorithms-train"
  ).map(p => pwd / RelPath(p))

  def parsedProjects: Vector[PredicateGraphConstruction.ParsedProject] = {
    projectRoots.map(r => {
      infer.PredicateGraphConstruction
        .fromSourceFiles(
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
