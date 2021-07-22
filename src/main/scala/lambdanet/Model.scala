package lambdanet

import ammonite.{ops => amm}
import ammonite.ops.Path
import lambdanet.NeuralInference.Predictor
import lambdanet.PrepareRepos.{ParsedProject, parseProject, parsedReposDir}
import lambdanet.TypeInferenceService.PredictionResults
import lambdanet.architecture.LabelEncoder.TrainableLabelEncoder
import lambdanet.architecture.{LabelEncoder, NNArchitecture}
import lambdanet.train.DataSet.nonGenerify
import lambdanet.train.Training.{AnnotsSampling, ForwardResult}
import lambdanet.train.{
  Counted,
  DataSet,
  Loss,
  LossAggMode,
  ProcessedProject,
  ProjectLabelStats,
  ProjectStats,
  TopNDistribution
}
import lambdanet.translation.ImportsResolution.ErrorHandler
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{LibTypeNode, PNode, PType, ProjNode}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.Random

case object Model {

  def fromData(
      dataSet: DataSet,
      gnnIterations: Int,
      architecture: NNArchitecture,
      lossAggMode: LossAggMode.Value,
      encodeLibSignature: Boolean,
      random: Random,
  ): Model = {
    import dataSet._

    val rand = new Random(1)

    val Seq(labelEncoder, nameEncoder) =
      Seq("labelEncoder", "nameEncoder").map { name =>
        import lambdanet.architecture.LabelEncoder.SegmentedLabelEncoder
        SegmentedLabelEncoder.fromData(
          name,
          trainSet,
          coverageGoal = 0.98,
          architecture,
          dropoutProb = 0.1,
          dropoutThreshold = 1000,
          randomLabelId(rand, vocabSize = 50),
        )
      }

    val labelCoverage: TrainableLabelEncoder =
      TrainableLabelEncoder.fromData(
        trainSet,
        coverageGoal = 0.90,
        architecture,
        dropoutProb = 0.1,
        dropoutThreshold = 500,
        identity,
        randomLabelId(rand, vocabSize = 50),
      )

    Model(
      gnnIterations,
      architecture,
      libDefs,
      libTypesToPredict,
      labelEncoder,
      nameEncoder,
      labelCoverage,
      random,
      lossAggMode,
      encodeLibSignature,
    )
  }

  def randomLabelId(rand: Random, vocabSize: Int): () => Int =
    () =>
      rand.synchronized {
        rand.nextInt(vocabSize)
      }

}

@SerialVersionUID(2L)
case class Model(
    gnnIterations: Int,
    architecture: NNArchitecture,
    libDefs: LibDefs,
    libTypesToPredict: Set[LibTypeNode],
    labelEncoder: LabelEncoder,
    nameEncoder: LabelEncoder,
    labelCoverage: TrainableLabelEncoder,
    random: Random,
    lossAggMode: LossAggMode.Value,
    encodeLibSignature: Boolean,
) {

  import lambdanet.train.{ConfusionMatrix, Correct, DecodingResult, confusionMatrix}

  def description: String = {
    s"""Model------
       |gnnIterations: $gnnIterations
       |architecture: ${architecture.arcName}
       |labelEncoder: ${labelEncoder.name}
       |nameEncoder: ${nameEncoder.name}
       |labelCoverage: ${labelCoverage.name}
       |lossAggMode: ${lossAggMode}
       |encodeLibSignature: $encodeLibSignature
       |------Model""".stripMargin
  }

  def predictForNodes(
      nodesToPredict: Vector[ProjNode],
      predictor: Predictor,
      predictTopK: Int,
  ): Map[PNode, TopNDistribution[PType]] = {
    val predSpace = predictor.stats.predictionSpace
    val decoding = announced("run predictor") {
      predictor
        .run(
          architecture,
          nodesToPredict,
          gnnIterations,
          labelEncoder,
          labelCoverage.isLibLabel,
          nameEncoder,
          labelDropout = false,
          encodeLibSignature,
        )
        .result
    }
    val predVec = decoding
      .topNPredictionsWithCertainty(predictTopK)
      .map { _.map(predSpace.typeOfIndex) }
    nodesToPredict.map(_.n).zip(predVec).toMap
  }

  def forwardStep(
      datum: ProcessedProject,
      annotsSampling: AnnotsSampling,
      shouldDropout: Boolean,
      maxLibRatio: Option[Double],
      maxBatchSize: Option[Int],
      projWeight: Double,
      taskSupport: Option[ForkJoinTaskSupport],
      announceTimes: Boolean = false,
  ): (Loss, ForwardResult, Map[PNode, TopNDistribution[PType]]) = {
    NeuralInference.checkOMP()

    val predSpace = datum.predictionSpace

    val (kept, dropped) = annotsSampling.randomSplit(datum.nodesToPredict, random)
    val (keptInferred, _) = annotsSampling.randomSplit(datum.visibleAnnotations, random)
    val inputAnnots = kept ++ keptInferred

    val downsampled = maxLibRatio match {
      case Some(ratio) => ProjectLabelStats(dropped).downsampleLibAnnots(ratio, random)
      case None        => dropped
    }
    assert(downsampled.size <= dropped.size)

    val (toPredict, groundTruths) = downsampled.toVector
      .pipe(random.shuffle(_))
      .take(maxBatchSize.getOrElse(Int.MaxValue))
      .unzip
    assert(toPredict.size <= downsampled.size)

    val targets = groundTruths.map(predSpace.indexOfType)

    val predictor = datum.mkPredictor(inputAnnots, taskSupport)

    val decoding = announced("run predictor", announceTimes) {
      predictor
        .run(
          architecture,
          toPredict,
          gnnIterations,
          labelEncoder,
          labelCoverage.isLibLabel,
          nameEncoder,
          shouldDropout,
          encodeLibSignature,
        )
        .result
    }

    val (correctness, confMat, typeAccs) =
      announced("compute training accuracy", announceTimes) {
        analyzeDecoding(
          decoding,
          groundTruths,
          predSpace
        )
      }
    assert(correctness.length == groundTruths.length)

    val loss = decoding.toLoss(
      targets,
      projWeight,
      predSpace.libTypeVec.length,
      lossAggMode
    )

    val grouped = toPredict
      .map(_.n)
      .zip(groundTruths)
      .zip(correctness)
      .groupBy(_._2)
      .mapValuesNow { pairs =>
        pairs.map { case ((n, ty), _) => (n, ty, datum.projectName) }.toSet
      }

    val totalCount = groundTruths.length
    assert(toPredict.length == grouped.values.map(_.size).sum)
    val fwd = ForwardResult(
      Counted(totalCount, loss.value.squeeze() * totalCount),
      kept.keySet.map(k => (k, datum.projectName)),
      dropped.keySet.map(k => (k, datum.projectName)),
      grouped.getOrElse(true, Set()),
      grouped.getOrElse(false, Set()),
      confMat,
      typeAccs,
    ).tap(r => assert(r.isConsistent))

    val predictions = {
      val predVec = decoding
        .topNPredictionsWithCertainty(6)
        .map { _.map(predSpace.typeOfIndex) }
      toPredict.map(_.n).zip(predVec).toMap
    }

    (loss, fwd, predictions)
  }

  case class PredictionService(
      numOfThreads: Int,
      predictTopK: Int,
      handler: ErrorHandler = ErrorHandler.alwaysStoreError,
  ) {
    val taskSupport: Option[ForkJoinTaskSupport] =
      if (numOfThreads > 1)
        Some(new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads)))
      else None

    lazy val nonGenerifyIt = nonGenerify(libDefs)

    def predictOnGraph(
        pGraph: PredicateGraph,
        nodeSelector: PNode => Boolean = _ => true,
        onlyPredictLibType: Boolean = false,
        predictAny: Boolean = true,
    ): Map[PNode, TopNDistribution[PType]] = {
      val stats = ProjectStats.computeProjectStats(
        pGraph,
        libTypesToPredict,
        libDefs,
        onlyPredictLibType,
        predictAny
      )
      val predictor = Predictor(
        pGraph,
        stats,
        libDefs,
        taskSupport,
        onlyPredictLibType,
        predictAny,
      )
      val nodes =
        pGraph.projNodes.filter(nodeSelector).map(ProjNode)
      predictForNodes(nodes.toVector, predictor, predictTopK)
    }

    def predictOnProject(
        sourcePath: Path,
        skipSet: Set[String] = Set("node_modules"),
        predictAny: Boolean,
        onlyPredictLibType: Boolean,
        alsoPredictNonSourceNodes: Boolean = false,
        warnOnErrors: Boolean,
    ): PredictionResults = {
      val project = announced("parse project") {
        parseProject(
          libDefs,
          sourcePath / amm.up,
          sourcePath,
          skipSet = skipSet,
          shouldPruneGraph = false,
          errorHandler = handler,
          warnOnErrors = warnOnErrors,
          predictAny = predictAny,
        )
      }

      def checkSource(node: PredicateGraph.PNode): Boolean =
        alsoPredictNonSourceNodes || node.srcSpan.nonEmpty

      // all user annotations will be used
      val userAnnotations = project.allUserAnnots.mapValuesNow(nonGenerifyIt)

      val graph = project.pGraph.addUserAnnotations(userAnnotations)
      val stats = ProjectStats.computeProjectStats(
        project.pGraph,
        libTypesToPredict,
        libDefs,
        onlyPredictLibType,
        predictAny,
      )
      val predictor = Predictor(
        graph,
        stats,
        libDefs,
        taskSupport,
        onlyPredictLibType,
        predictAny,
      )

      val nodesToPredict = project.allAnnots.keySet.collect {
        case n if n.fromProject && checkSource(n) => ProjNode(n)
      } -- project.nonInferredUserAnnots.keySet
      val prediction = predictForNodes(nodesToPredict.toVector, predictor, predictTopK)
      val srcLines = project.srcTexts.mapValuesNow(_.split("\n"))
      PredictionResults(prediction, srcLines)
    }

    def predictOnProject(
        sourcePath: Path,
        warnOnErrors: Boolean,
        skipSet: Array[String],
    ): PredictionResults = {
      predictOnProject(
        sourcePath,
        skipSet = skipSet.toSet,
        warnOnErrors = warnOnErrors,
        predictAny = false,
        onlyPredictLibType = false,
      )
    }

  }

  private def analyzeDecoding(
      results: DecodingResult,
      groundTruths: Vector[PType],
      predictionSpace: PredictionSpace
  ): (
      Vector[Boolean],
      Counted[ConfusionMatrix],
      Map[PType, Counted[Correct]]
  ) = {
    val predictions = results.topPredictions
    val targets = groundTruths.map(predictionSpace.indexOfType)
    val correctness = predictions.zip(targets).map { case (x, y) => x == y }
    val targetFromLibrary = groundTruths.map { _.madeFromLibTypes }

    val confMat = {
      def toCat(isLibType: Boolean): Int = if (isLibType) 0 else 1
      val predictionCats = predictions.map { i =>
        toCat(predictionSpace.isLibType(i))
      }
      val truthCats = targetFromLibrary.map(toCat)
      val mat = confusionMatrix(predictionCats, truthCats, categories = 2)
      Counted(predictionCats.length, mat)
    }

    val typeAccs =
      groundTruths.zip(correctness).groupBy(_._1).mapValuesNow { bools =>
        Counted(bools.length, bools.count(_._2))
      }

    (correctness, confMat, typeAccs)
  }

}
