package lambdanet.correctness

import ammonite.ops.Path
import ammonite.{ops => amm}
import lambdanet.PrepareRepos.ParsedProject
import lambdanet.SM
import lambdanet.translation.PredicateGraph.PNode
import lambdanet.translation.{PAnnot, PredicateGraph, PredicateGraphLoader}

object InputUtils {
  def loadGraphAndPredict(
      inputPath: Path
  ): (PredicateGraph, Map[PNode, PAnnot], TypeDistrs) = {
    val resultsPath = inputPath / "results.serialized"
    val project = PredicateGraphLoader.load(inputPath)
    loadGraphAndPredict(resultsPath, project)
  }

  def loadGraphAndPredict(
      resultsPath: Path,
      project: ParsedProject
  ): (PredicateGraph, Map[PNode, PAnnot], TypeDistrs) = {
    val graph = project.pGraph
    val nodeAnnots = project.rawAllUserAnnots
    val results =
      if (amm.exists(resultsPath)) {
        SM.readObjectFromFile[TypeDistrs](resultsPath.toIO)
      } else {
        val res =
          NewestModelService.service.predictOnGraph(graph, nodeSelector = _.fromProject)
        SM.saveObjectToFile(resultsPath.toIO)(res.asInstanceOf[Serializable])
        res
      }
    (graph, nodeAnnots, results)
  }

  def loadGroundTruth(inputPath: Path): Option[Assignment] = {
    val groundTruthPath = inputPath / "ground_truth.serialized"
    if (amm.exists(groundTruthPath)) {
      Some(SM.loadObjectFromFile[Assignment](groundTruthPath.toIO))
    } else {
      None
    }
  }
}
