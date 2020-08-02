package lambdanet

import ammonite.{ops => amm}
import ammonite.ops.Path
import lambdanet.NeuralInference.Predictor
import lambdanet.PrepareRepos.{ParsedProject, parseProject}
import lambdanet.SequenceModel.SeqArchitecture
import lambdanet.architecture.LabelEncoder.TrainableLabelEncoder
import lambdanet.architecture.{LabelEncoder, NNArchitecture}
import lambdanet.train.DataSet.{nonGenerify, selectLibTypes}
import lambdanet.train.TrainingLoop.{ForwardResult, maxLibRatio, projWeight}
import lambdanet.train.TrainingState.iterationNum
import lambdanet.train.{
  Counted,
  DataSet,
  Loss,
  LossModel,
  ProcessedProject,
  TopNDistribution
}
import lambdanet.translation.ImportsResolution.ErrorHandler
import lambdanet.translation.PredicateGraph.{LibTypeNode, PNode, PType}
import lambdanet.utils.QLangDisplay

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.Random

case object Model {

  def fromData(
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
      architecture,
      libDefs,
      libTypesToPredict,
      labelEncoder,
      nameEncoder,
      labelCoverage,
      random
    )
  }

  def randomLabelId(rand: Random, vocabSize: Int): () => Int =
    () =>
      rand.synchronized {
        rand.nextInt(vocabSize)
      }

}

case class Model(
    architecture: NNArchitecture,
    libDefs: LibDefs,
    libTypesToPredict: Set[LibTypeNode],
    labelEncoder: LabelEncoder,
    nameEncoder: LabelEncoder,
    labelCoverage: TrainableLabelEncoder,
    random: Random,
    lossModel: LossModel = LossModel.NormalLoss,
) {

  import lambdanet.train.{
    ConfusionMatrix,
    Correct,
    DecodingResult,
    confusionMatrix
  }

  def forward(
      datum: ProcessedProject,
      shouldDownsample: Boolean,
      shouldDropout: Boolean,
      maxBatchSize: Option[Int]
  ): (Loss, ForwardResult, Map[PNode, TopNDistribution[PType]]) = {
    import datum._

    val predSpace = datum.predictionSpace

    val annotsToUse =
      if (shouldDownsample) datum.downsampleLibAnnots(maxLibRatio, random)
      else nodesToPredict

    val (nodes, groundTruths) = annotsToUse.toVector
      .pipe(random.shuffle(_))
      .take(maxBatchSize.getOrElse(Int.MaxValue))
      .unzip
    val targets = groundTruths.map(predSpace.indexOfType)

    val decodingVec = announced("run predictor") {
      datum.predictor match {
        case Left(seqPredictor) =>
          seqPredictor
            .run(
              architecture.asInstanceOf[SeqArchitecture],
              nameEncoder,
              nodes,
              shouldDropout
            )
            .pipe(Vector(_))
        case Right(predictor) =>
          predictor
            .run(
              architecture,
              nodes,
              iterationNum,
              labelEncoder,
              labelCoverage.isLibLabel,
              nameEncoder,
              shouldDropout
            )
            .result
      }
    }
    val decoding = decodingVec.last

    val (correctness, confMat, typeAccs) =
      announced("compute training accuracy") {
        analyzeDecoding(
          decoding,
          groundTruths,
          predSpace
        )
      }

    val loss = lossModel.predictionLoss(
      decodingVec.par
        .map(_.toLoss(targets, projWeight, predSpace.libTypeVec.length))
    )

    val totalCount = groundTruths.length
    val mapped = nodes
      .map(_.n)
      .zip(groundTruths)
      .zip(correctness)
      .groupBy(_._2)
      .mapValuesNow { pairs =>
        pairs.map { case ((n, ty), _) => (n, ty, datum.projectName) }.toSet
      }

    val fwd = ForwardResult(
      Counted(totalCount, loss.value.squeeze() * totalCount),
      mapped.getOrElse(true, Set()),
      mapped.getOrElse(false, Set()),
      confMat,
      typeAccs
    ).tap(r => assert(r.isConsistent))

    val predictions = {
      val predVec = decoding
        .topNPredictionsWithCertainty(6)
        .map { _.map(predSpace.typeOfIndex) }
      nodes.map(_.n).zip(predVec).toMap
    }

    (loss, fwd, predictions)
  }

  case class PredictionService(
      numOfThreads: Int,
      handler: ErrorHandler = ErrorHandler.alwaysStoreError,
  ) {
    val taskSupport: Option[ForkJoinTaskSupport] =
      if (numOfThreads > 1)
        Some(new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads)))
      else None

    def predict(
        datum: ProcessedProject,
        outputPath: Path,
        maxBatchSize: Option[Int]
    ): Unit = {
      val (_, fwd, pred) = forward(
        datum,
        shouldDownsample = false,
        shouldDropout = false,
        maxBatchSize = maxBatchSize
      )

      QLangDisplay.renderProjectToDirectory(
        datum.projectName.toString,
        datum.qModules,
        pred,
        datum.predictionSpace.allTypes
      )(outputPath / "predictions")

      import train.toAccuracy
      println("libAccuracy: " + toAccuracy(fwd.libCorrect))
      println("projectAccuracy: " + toAccuracy(fwd.projCorrect))
    }

    def processProject(
        sourcePath: Path,
        skipSet: Set[String] = Set("node_modules"),
        useInferred: Boolean = true,
        onlyPredictLibType: Boolean = false,
    ): ProcessedProject = {

      val project =
        parseProject(
          libDefs,
          sourcePath / amm.up,
          sourcePath,
          skipSet = skipSet,
          shouldPruneGraph = false,
          errorHandler = handler
        )

      val ParsedProject(path, qModules, _, g) = project
      val predictor = Predictor(
        path,
        g,
        libTypesToPredict,
        libDefs,
        taskSupport,
        onlyPredictLibType
      )
      val typeTransform = nonGenerify(libDefs)
      val annots1 =
        (if (useInferred) project.allUserAnnots
         else project.nonInferredUserAnnots)
          .mapValuesNow(typeTransform)
          .filter { x =>
            predictor.predictionSpace.allTypes.contains(x._2)
          }
      ProcessedProject(path, annots1, g, qModules.map { m =>
        m.copy(mapping = m.mapping.mapValuesNow(_.map(typeTransform)))
      }, Right(predictor))
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
