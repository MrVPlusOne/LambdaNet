package lambdanet.correctness

import ammonite.ops.Path
import ammonite.{ops => amm}
import lambdanet.TypeInferenceService.{ModelConfig, loadModel}
import lambdanet.{Model, SM, TypeInferenceService}
import lambdanet.translation.PredicateGraph.PNode
import lambdanet.translation.{PAnnot, PredicateGraph, PredicateGraphLoader}

object InputUtils {
  val modelDir: Path = TypeInferenceService.newestModelDir
  val paramPath: Path = modelDir / "params.serialized"
  val modelCachePath: Path = modelDir / "model.serialized"
  val modelConfig: ModelConfig = ModelConfig()

  lazy val model: Model =
    loadModel(paramPath, modelCachePath, modelConfig, numOfThreads = 8)
  lazy val service: model.PredictionService =
    model.PredictionService(numOfThreads = 8, predictTopK = Int.MaxValue)

  def loadGraphAndPredict(inputPath: Path): (PredicateGraph, Map[PNode, PAnnot], TypeDistrs) = {
    val project = PredicateGraphLoader.load(inputPath)
    val graph = project.pGraph
    val nodeSeqAnnots: Map[PNode, Seq[PAnnot]] = project.qModules.map(_.mapping.toSeq).reduce(_ ++ _).groupBy(_._1).mapValuesNow(_.map(_._2))
    nodeSeqAnnots.foreach { case (node, annot) =>
      assert(annot.size == 1, s"$node has multiple annotations: $annot")
    }
    val nodeAnnots = nodeSeqAnnots.mapValuesNow(_.head)
    val resultsPath = inputPath / "results.serialized"
    val results =
      if (amm.exists(resultsPath)) {
        SM.loadObjectFromFile[TypeDistrs](resultsPath.toIO)
      } else {
        val res = service.predictOnGraph(graph, nodeSelector = _.fromProject)
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
