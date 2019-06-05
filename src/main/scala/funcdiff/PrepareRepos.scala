package funcdiff

import ammonite.ops._
import lambdanet.TrainingProjects.Project
import lambdanet.translation.ImportsResolution.PathMapping
import lambdanet.translation.{
  IRTranslation,
  PredicateGraphTranslation,
  QLangTranslation
}
import lambdanet.translation.PredicateGraph.PNodeAllocator
import lambdanet.utils.ProgramParsing

object PrepareRepos {
  val declarationsDir = pwd / up / "lambda-repos" / "declarations"

  val (baseCtx, libAllocator, _) = QLangTranslation.parseDefaultModule()

  def prepareProject(root: Path) = {
    val modules = ProgramParsing.parseGModulesFromRoot(root)
    val p = Project(root, modules, PathMapping.identity)
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
}
