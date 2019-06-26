package lambdanet.train

import lambdanet._
import java.util.concurrent.ForkJoinPool

import ammonite.ops.{Path}
import botkop.numsca
import botkop.numsca.Tensor
import funcdiff.{
  CompNode,
  DebugTime,
  LayerFactory,
  Optimizer,
  Optimizers,
  ParamCollection,
  SimpleMath,
  SymbolPath,
  TensorExtension,
  crossEntropyOnSoftmax,
  mean,
}
import lambdanet.NewInference.Predictor
import lambdanet.TrainingCenter.Timeouts
import lambdanet.translation.PredicateGraph._
import lambdanet.utils.EventLogger
import lambdanet.utils.EventLogger.PlotConfig
import lambdanet.{PredicateGraphWithCtx, PredictionSpace, printWarning}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.util.Random

object TrainingLoop {

  /** Remember to use these VM options to increase memory limits.
    * VM Options: -Xms2G -Xmx8G -Dorg.bytedeco.javacpp.maxbytes=18G -Dorg.bytedeco.javacpp.maxphysicalbytes=27G */
  def main(args: Array[String]): Unit = {

    run(
      maxTrainingEpochs = 100,
      numOfThreads = Runtime.getRuntime.availableProcessors(),
    ).result()
  }

  case class run(
      maxTrainingEpochs: Int,
      numOfThreads: Int,
  ) {
    def result(): Unit = {
      val trainingState = loadTrainingState()
      val dataSet = loadDataSet()
      trainOnProjects(dataSet, trainingState)
    }

    //noinspection TypeAnnotation
    case class trainOnProjects(
        dataSet: DataSet,
        trainingState: TrainingState,
    ) {
      import TensorExtension.oneHot
      import dataSet._
      import trainingState._

      val logger: EventLogger = mkEventLogger()
      val layerFactory =
        new LayerFactory(SymbolPath.empty / Symbol("TrainingLoop"), pc)
      val stepsPerEpoch = trainSet.length max testSet.length

      (trainingState.epoch to maxTrainingEpochs)
        .foreach { epoch =>
          announced(s"epoch $epoch") {
            trainStep(epoch)
            testStep(epoch)
          }
        }

      def trainStep(epoch: Int): Unit = {
        trainSet.zipWithIndex.foreach {
          case (datum, i) =>
            val startTime = System.nanoTime()
            announced(s"train on $datum") {
              val (loss, (libAcc, projAcc)) = forward(datum)

              val step = epoch * stepsPerEpoch + i
              logger.log("loss", step, loss.value)
              logger.log("libAcc", step, Tensor(libAcc))
              logger.log("projAcc", step, Tensor(projAcc))

              announced("optimization") {
                optimizer.minimize(
                  loss,
                  pc.allParams, //todo: consider only including the involved
                  backPropInParallel =
                    Some(parallelCtx -> Timeouts.optimizationTimeout),
                  weightDecay = Some(5e-5)
                )
              }

              val timeInSec = (System.nanoTime() - startTime).toDouble / 1e9
              logger.log("iter-time", step, Tensor(timeInSec))
            }
        }
        println(DebugTime.show)
      }

      def testStep(epoch: Int): Unit =
        if (epoch % 5 == 0) announced("test on dev set") {
          testSet.zipWithIndex.foreach { case (datum, i) =>
            val step = epoch * stepsPerEpoch + i
            announced(s"test on $datum") {
              val (_, (libAcc, projAcc)) = forward(datum)
//              logger.log("loss", step, loss.value)
              logger.log("test-libAcc", step, Tensor(libAcc))
              logger.log("test-projAcc", step, Tensor(projAcc))
            }
          }
          // todo: compute weighted average loss and accuracy
        }

      type Logits = CompNode
      type Loss = CompNode
      type LibAccuracy = Double
      type ProjAccuracy = Double
      private def forward(datum: Datum): (Loss, (LibAccuracy, ProjAccuracy)) = {
        import datum._

        val nodesToPredict = userAnnotations.keys.toVector
        val predSpace = PredictionSpace(
          libTypes ++ predictor.projectNodes.collect {
            case ProjNode(n) if n.isType => PTyVar(n)
          },
        )

        val logits = announced("run predictor") {
          predictor
            .run(dimMessage, layerFactory, nodesToPredict, iterationNum)
            .result
        }

        val groundTruths = nodesToPredict.map(userAnnotations)
        val accuracies = announced("compute training accuracy") {
          analyzeLogits(
            logits,
            predSpace,
            groundTruths,
            groundTruths.map(_.madeFromLibTypes),
          )
        }

        val loss = predictionLoss(logits, predSpace, groundTruths)

        (loss, accuracies)
      }

      private def analyzeLogits(
          logits: CompNode,
          predSpace: PredictionSpace,
          groundTruths: Vector[PType],
          isFromLibrary: Vector[Boolean],
      ): (LibAccuracy, ProjAccuracy) = {
        val predictions = numsca
          .argmax(logits.value, axis = 1)
          .data
          .map(_.toInt)
          .toVector
        val target = groundTruths.map(predSpace.indexOfType)
        val zipped = isFromLibrary.zip(predictions.zip(target))
        val libCorrect = zipped.collect {
          case (true, (x, y)) if x == y => ()
        }.length
        val projCorrect = zipped.collect {
          case (false, (x, y)) if x == y => ()
        }.length
        val numLib = isFromLibrary.count(identity)
        val numProj = isFromLibrary.count(!_)

        def safeDiv(a: Int, b: Int): Double =
          if (b == 0) 0.0 else a.toDouble / b

        (safeDiv(libCorrect, numLib), safeDiv(projCorrect, numProj))
      }

      private def predictionLoss(
          logits: CompNode,
          predSpace: PredictionSpace,
          groundTruths: Vector[PType],
      ): CompNode = {
        val targets = groundTruths.map(predSpace.indexOfType)
        val loss = mean(
          crossEntropyOnSoftmax(logits, oneHot(targets, predSpace.maxIndex)),
        )
        if (loss.value.squeeze() > 20) {
          printWarning(
            s"Abnormally large loss: ${loss.value}, logits: \n${logits.value}",
          )
        }
        loss
      }
    }

    val forkJoinPool = new ForkJoinPool(numOfThreads)
    val taskSupport: ForkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)
    val parallelCtx: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(forkJoinPool)

    private def loadDataSet(): DataSet = announced("loadDataSet") {
      import PrepareRepos._

      val ParsedRepos(libDefs, projects) =
        announced("parsePredGraphs")(parseRepos())
//        announced(s"read data set from file: $dataSetPath") {
//          SimpleMath.readObjectFromFile[ParsedRepos](dataSetPath.toIO)
//        }

      def libNodeType(n: LibNode) =
        libDefs
          .nodeMapping(n.n)
          .typeOpt
          .getOrElse(PredictionSpace.unknownType)

      val libraryTypes: Set[PType] =
        libDefs.nodeMapping.keySet.collect {
          case n if n.isType => PTyVar(n): PType
        }

      val random = new Random(1)

      val data = projects
        .pipe(x => random.shuffle(x))
        .map {
          case (path, g, annotations) =>
            val projectTypes =
              g.nodes.filter(n => !n.fromLib && n.isType).map(PTyVar)
            val pSpace = PredictionSpace(libraryTypes ++ projectTypes)
            val predictor =
              Predictor(g, libNodeType, pSpace, Some(taskSupport))

            Datum(path, annotations.toMap, predictor)
        }

      val totalNum = data.length
      val trainSetNum = totalNum - (totalNum * 0.2).toInt
      DataSet(data.take(trainSetNum), data.drop(trainSetNum), libraryTypes)
    }

  }

  /** A parsed typescript project */
  type TsProject = PredicateGraphWithCtx

  case class TrainingState(
      epoch: Int,
      dimMessage: Int,
      iterationNum: Int,
      optimizer: Optimizer,
      pc: ParamCollection,
  ) {
    def saveToFile(file: Path): Unit = {
      val toSave =
        List[(String, Any)](
          "epoch" -> epoch,
          "dimMessage" -> dimMessage,
          "iterationNum" -> iterationNum,
          "optimizer" -> optimizer,
          "pcData" -> pc.toSerializable,
        )
      SimpleMath.saveObjectToFile(file.toIO)(toSave)
    }

    override def toString: String = {
      s"""TrainingState:
         |  step: $epoch
         |  dimMessage: $dimMessage
         |  iterationNum: $iterationNum
         |  optimizer: $optimizer,
       """.stripMargin
    }
  }

  object TrainingState {
    def fromFile(file: Path): TrainingState = {
      val map = SimpleMath
        .readObjectFromFile[List[(String, Any)]](file.toIO)
        .toMap
      val step = map("step").asInstanceOf[Int]
      val dimMessage = map("dimMessage").asInstanceOf[Int]
      val optimizer = map("optimizer").asInstanceOf[Optimizer]
      val iterationNum = map.getOrElse("iterationNum", 10).asInstanceOf[Int]
      val pcData = map("pcData")
        .asInstanceOf[ParamCollection.SerializableFormat]
      val pc = ParamCollection.fromSerializable(pcData)
      TrainingState(step, dimMessage, iterationNum, optimizer, pc)
    }
  }

  case class Datum(
      projectName: ProjectPath,
      userAnnotations: Map[ProjNode, PType],
      predictor: Predictor,
  ) {
    override def toString: String = {
      s"{name: $projectName, annotations: ${userAnnotations.size}, " +
        s"predicates: ${predictor.graph.predicates.size}}"
    }
  }

  case class DataSet(
      trainSet: Vector[Datum],
      testSet: Vector[Datum],
      libTypes: Set[PType],
  )

  val defaultIterationNum = 9

  private def loadTrainingState(): TrainingState =
    announced("loadTrainingState") {
      val loadFromFile: Option[Path] =
        TrainingControl.restoreFromFile(consumeFile = true)

      loadFromFile
        .map { p =>
          announced("Loading training from file: " + p) {
            TrainingState.fromFile(p)
          }
        }
        .getOrElse(
          TrainingState(
            epoch = 0,
            dimMessage = 64,
            optimizer = Optimizers.Adam(learningRate = 1e-4),
            iterationNum = defaultIterationNum,
            pc = ParamCollection(),
          ),
        )
    }

  def mkEventLogger() = {
    import ammonite.ops._
    new EventLogger(
      pwd / "running-result" / "log.txt",
      printToConsole = true,
      overrideMode = true,
      configs = Seq(
//        "embedding-changes" -> PlotConfig("ImageSize->Medium"),
        //          "embedding-max-length" -> PlotConfig("ImageSize->Medium"),
        "loss" -> PlotConfig("ImageSize->Medium"),
        "libAcc" -> PlotConfig("ImageSize->Medium"),
        "projAcc" -> PlotConfig("ImageSize->Medium"),
        "iter-time" -> PlotConfig(
          "ImageSize->Medium",
          """AxesLabel->{"step","s"}""",
        ),
        "test-libAcc" -> PlotConfig("ImageSize->Medium"),
        "test-projAcc" -> PlotConfig("ImageSize->Medium"),
      ),
    )
  }

}
