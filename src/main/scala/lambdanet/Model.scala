package lambdanet

import funcdiff.{GraphMode, ModeEval, ModeTraining}
import ammonite.{ops => amm}
import ammonite.ops.Path
import lambdanet.NeuralInference.Predictor
import lambdanet.PrepareRepos.parseProject
import lambdanet.Surface.GModule
import lambdanet.TypeInferenceService.PredictionResults
import lambdanet.architecture.LabelEncoder.TrainableLabelEncoder
import lambdanet.architecture.{LabelEncoder, NNArchitecture}
import lambdanet.train.DataSet.nonGenerify
import lambdanet.train.Training.{AnnotsSampling, ForwardResult, ModelConfig}
import lambdanet.train._
import lambdanet.translation.ImportsResolution.ErrorHandler
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{LibTypeNode, PNode, PType, ProjNode}

import java.util.concurrent.{ForkJoinPool, ForkJoinWorkerThread}
import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.Random

@SerialVersionUID(-639226779165037135L)
object Model {

  def fromData(
      config: ModelConfig,
      dataSet: DataSet,
      architecture: NNArchitecture,
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
      config: ModelConfig,
      architecture,
      libDefs,
      libTypesToPredict,
      labelEncoder,
      nameEncoder,
      labelCoverage,
      random,
    )
  }

  def randomLabelId(rand: Random, vocabSize: Int): () => Int =
    () =>
      rand.synchronized {
        rand.nextInt(vocabSize)
      }

  def mkTaskSupport(nThreads: Int): ForkJoinTaskSupport = {
    val factory = new ForkJoinPool.ForkJoinWorkerThreadFactory {
      def newThread(pool: ForkJoinPool): ForkJoinWorkerThread = {
        new ForkJoinWorkerThread(pool) {
          setContextClassLoader(this.getClass.getClassLoader)
        }
      }
    }
    new ForkJoinTaskSupport(new ForkJoinPool(nThreads, factory, null, false))
  }
}

//noinspection TypeAnnotation
@SerialVersionUID(3L)
case class Model(
    config: ModelConfig,
    architecture: NNArchitecture,
    libDefs: LibDefs,
    libTypesToPredict: Set[LibTypeNode],
    labelEncoder: LabelEncoder,
    nameEncoder: LabelEncoder,
    labelCoverage: TrainableLabelEncoder,
    random: Random,
) {
  import config.{predictAny, onlyPredictLibType}
  def gnnIterations = config.gnnIterations
  def encodeLibSignature = config.encodeLibSignature
  def lossAggMode = config.lossAggMode

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
        )(ModeEval)
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
  )(
      implicit mode: GraphMode
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
      if (numOfThreads > 1) Some(Model.mkTaskSupport(numOfThreads))
      else None

    lazy val nonGenerifyIt = nonGenerify(libDefs)

    def predictOnGraph(
        pGraph: PredicateGraph,
        nodeSelector: PNode => Boolean = _ => true,
    ): Map[PNode, TopNDistribution[PType]] = {
      implicit val m: GraphMode = ModeEval
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
        gModulesOpt: Option[Vector[GModule]] = None,
        skipSet: Set[String] = Set("node_modules"),
        alsoPredictNonSourceNodes: Boolean = false,
        shouldPruneGraph: Boolean = false,
        warnOnErrors: Boolean,
    ): PredictionResults = {
      val project = announced("parse project") {
        parseProject(
          libDefs,
          sourcePath / amm.up,
          sourcePath,
          predictAny,
          gModulesOpt,
          skipSet = skipSet,
          shouldPruneGraph = shouldPruneGraph,
          errorHandler = handler,
          warnOnErrors = warnOnErrors,
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

    /** Convenient method for Java API. */
    def predictOnProject(
        sourcePath: Path,
        warnOnErrors: Boolean,
        skipSet: Array[String],
    ): PredictionResults = {
      predictOnProject(
        sourcePath,
        skipSet = skipSet.toSet,
        warnOnErrors = warnOnErrors,
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
