package lambdanet.train

import lambdanet._
import java.util.concurrent.{ForkJoinPool, TimeoutException}

import ammonite.ops.Path
import botkop.numsca
import botkop.numsca.Tensor
import cats.Monoid
import funcdiff.{SimpleMath => SM}
import funcdiff._
import lambdanet.NewInference.Predictor
import lambdanet.TrainingCenter.Timeouts
import lambdanet.translation.PredicateGraph._
import lambdanet.utils.{EventLogger, ReportFinish}
import lambdanet.utils.EventLogger.PlotConfig
import lambdanet.{PredictionSpace, printWarning}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future, TimeoutException}
import scala.util.Random

object TrainingLoop {

  /** Remember to use these VM options to increase memory limits.
    * VM Options: -Xms2G -Xmx8G -Dorg.bytedeco.javacpp.maxbytes=18G -Dorg.bytedeco.javacpp.maxphysicalbytes=27G */
  def main(args: Array[String]): Unit = {

    run(
      maxTrainingEpochs = 1000,
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
      trainOnProjects(dataSet, trainingState).result()
    }

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
              optimizer = Optimizers.Adam(learningRate = 1e-3),
              iterationNum = 6,
              pc = ParamCollection(),
            ),
          )
          .tap(println)
      }

    //noinspection TypeAnnotation
    case class trainOnProjects(
        dataSet: DataSet,
        trainingState: TrainingState,
    ) {
      import TensorExtension.oneHot
      import dataSet._
      import trainingState._

      def result(): Unit = {
        (trainingState.epoch + 1 to maxTrainingEpochs)
          .foreach { epoch =>
            announced(s"epoch $epoch") {
              handleExceptions(epoch) {
                trainStep(epoch)
                testStep(epoch)
              }

              if (epoch % 20 == 0) {
                saveTraining(epoch, s"epoch$epoch")
              }
            }
          }

        saveTraining(maxTrainingEpochs, "finished")
        emailService.sendMail(emailService.userEmail)(
          s"TypingNet: Training finished on $machineName!",
          "Training finished!",
        )
      }

      val (machineName, emailService) = ReportFinish.readEmailInfo()
      private def handleExceptions(epoch: Int)(f: => Unit): Unit = {
        try f
        catch {
          case ex: Throwable =>
            val isTimeout = ex.isInstanceOf[TimeoutException]
            val errorName = if (isTimeout) "timeout" else "stopped"
            emailService.sendMail(emailService.userEmail)(
              s"TypingNet: $errorName on $machineName at epoch $epoch",
              s"Details:\n" + ex.getMessage,
            )
            if (isTimeout && Timeouts.restartOnTimeout) {
              println(
                "Timeout... training restarted (skip one training epoch)...",
              )
            } else {
              saveTraining(epoch, "error-save")
              throw ex
            }
        }
      }

      val logger: EventLogger = mkEventLogger()
      val layerFactory =
        new LayerFactory(SymbolPath.empty / Symbol("TrainingLoop"), pc)
      val stepsPerEpoch = trainSet.length max testSet.length

      def trainStep(epoch: Int): Unit = {
        val startTime = System.nanoTime()

        val stats = trainSet.map { datum =>
          announced(s"train on $datum") {
            val fwd = forward(datum)
              .tap(f => println(resultStr(f.toString)))

            checkShouldStop()
            def optimize() = optimizer.minimize(
              scaleLoss(fwd.loss, fwd.size),
              pc.allParams,
              backPropInParallel =
                Some(parallelCtx -> Timeouts.optimizationTimeout),
              //                  weightDecay = Some(5e-5),  todo: use dropout
            )

            limitTime(Timeouts.optimizationTimeout) {
              announced("optimization") {
                DebugTime.logTime("optimization") {
                  optimize()
                }
              }
            }
            fwd
          }
        }

        import cats.implicits._
        val ForwardResult(loss, _, libAcc, _, projAcc) = stats.combineAll
        logger.log("loss", epoch, loss.value)
        logger.log("libAcc", epoch, libAcc)
        logger.log("projAcc", epoch, projAcc)

        val timeInSec = (System.nanoTime() - startTime).toDouble / 1e9
        logger.log("iter-time", epoch, Tensor(timeInSec))

        println(DebugTime.show)
      }

      def testStep(epoch: Int): Unit =
        if (epoch % 5 == 0) announced("test on dev set") {
          import cats.implicits._

          val stat = testSet.map { datum =>
            announced(s"test on $datum") {
              forward(datum)
            }
          }.combineAll

          logger.log("test-libAcc", epoch, stat.libAccuracy)
          logger.log("test-projAcc", epoch, stat.projAccuracy)
        }

      type Logits = CompNode
      type Loss = CompNode
      type LibAccuracy = Double
      type ProjAccuracy = Double

      case class ForwardResult(
          loss: Loss,
          libNodes: Int,
          libAccuracy: LibAccuracy,
          projNodes: Int,
          projAccuracy: ProjAccuracy,
      ) {
        val size = libNodes + projNodes

        override def toString: String =
          s"forward result: {loss: ${loss.value}, lib acc: $libAccuracy ($libNodes nodes), " +
            s"proj acc: $projAccuracy ($projNodes nodes)}"
      }

      implicit val forwardResultMonoid: Monoid[ForwardResult] =
        new Monoid[ForwardResult] {
          def empty: ForwardResult = ForwardResult(0, 0, 1, 0, 1)

          def combine(x: ForwardResult, y: ForwardResult): ForwardResult = {
            val loss = (x.loss * x.size + y.loss * y.size) / nonZero(
              x.size + y.size,
            )
            val libNodes = x.libNodes + y.libNodes
            val libAcc = (x.libAccuracy * x.libNodes + y.libAccuracy * y.libNodes) /
              nonZero(x.libNodes + y.libNodes)
            val projNodes = x.projNodes + y.projNodes
            val projAcc = (x.projAccuracy * x.projNodes + y.projAccuracy * y.projNodes) /
              nonZero(x.projNodes + y.projNodes)
            ForwardResult(loss, libNodes, libAcc, projNodes, projAcc)
          }
        }

      private def nonZero(n: Int): Double = if (n == 0) 1.0 else n.toDouble

      private def forward(datum: Datum): ForwardResult =
        limitTime(Timeouts.forwardTimeout) {
          checkShouldStop()

          import datum._

          val nodesToPredict = userAnnotations.keys.toVector
          val predSpace = predictor.predictionSpace

          val logits = announced("run predictor") {
            predictor
              .run(dimMessage, layerFactory, nodesToPredict, iterationNum)
              .result
          }

          val groundTruths = nodesToPredict.map(userAnnotations)
          val targets = groundTruths.map(predSpace.indexOfType)
          val isFromLib = groundTruths.map(_.madeFromLibTypes)

          assert(logits.shape(1) == predSpace.size)
          val (libAcc, projAcc) = announced("compute training accuracy") {
            analyzeLogits(
              logits,
              targets,
              isFromLib,
            )
          }

          val loss = predictionLoss(logits, targets, predSpace.size)
            .pipe(scaleLoss(_, datum.userAnnotations.size))

          val libNodes = isFromLib.count(identity)
          val projNodes = isFromLib.count(!_)
          ForwardResult(loss, libNodes, libAcc, projNodes, projAcc)
        }

      @throws[TimeoutException]
      private def limitTime[A](timeLimit: Timeouts.Duration)(f: => A): A = {
        val exec = scala.concurrent.ExecutionContext.global
        Await.result(Future(f)(exec), timeLimit)
      }

      private def saveTraining(step: Int, dirName: String): Unit = {
        import ammonite.ops._

        announced(s"save training to $dirName") {
          val saveDir = pwd / "running-result" / "saved" / dirName
          if (!exists(saveDir)) {
            mkdir(saveDir)
          }
          val savePath = saveDir / "trainingState.serialized"
          TrainingState(step, dimMessage, iterationNum, optimizer, pc)
            .saveToFile(
              savePath,
            )
        }
      }

      @throws[Exception]
      private def checkShouldStop(): Unit = {
        if (TrainingControl.shouldStop(consumeFile = true)) {
          saveTraining(epoch, s"stopped-epoch$epoch")
          throw new Exception("Stopped by 'stop.txt'.")
        }
      }

      private def analyzeLogits(
          logits: CompNode,
          targets: Vector[Int],
          isFromLibrary: Vector[Boolean],
      ): (LibAccuracy, ProjAccuracy) = {
        val predictions = numsca
          .argmax(logits.value, axis = 1)
          .data
          .map(_.toInt)
          .toVector
        val zipped = isFromLibrary.zip(predictions.zip(targets))
        val libCorrect = zipped.collect {
          case (true, (x, y)) if x == y => ()
        }.length
        val projCorrect = zipped.collect {
          case (false, (x, y)) if x == y => ()
        }.length
        val numLib = isFromLibrary.count(identity)
        val numProj = isFromLibrary.count(!_)

        (libCorrect / nonZero(numLib), projCorrect / nonZero(numProj))
      }

      private val avgAnnotations =
        SM.mean(trainSet.map(_.userAnnotations.size.toDouble))
      private def scaleLoss(loss: Loss, annotations: Int) = {
        loss * (annotations.toDouble / avgAnnotations)
      }

      private def predictionLoss(
          logits: CompNode,
          targets: Vector[Int],
          predSpaceSize: Int,
      ): CompNode = {
        val loss = mean(
          crossEntropyOnSoftmax(logits, oneHot(targets, predSpaceSize)),
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
        announced(s"read data set from file: $parsedRepoPath") {
          SM.readObjectFromFile[ParsedRepos](parsedRepoPath.toIO)
        }

      def libNodeType(n: LibNode) =
        libDefs
          .nodeMapping(n.n)
          .typeOpt
          .getOrElse(PredictionSpace.unknownType)

      val libTypesToPredict: Set[LibTypeNode] = {
        import cats.implicits._
        val usages: Map[PNode, Int] = projects.par
          .map {
            case (_, _, annots) =>
              annots.collect { case (_, PTyVar(v)) => v -> 1 }.toMap
          }
          .fold(Map[PNode, Int]())(_ |+| _)

        /** sort lib tyeps by their usages */
        val sortedTypes = libDefs.nodeMapping.keys.toVector
          .collect {
            case n if n.isType =>
              (LibTypeNode(LibNode(n)), usages.getOrElse(n, 0))
          }
          .sortBy(-_._2)

        val totalUsages = sortedTypes.map(_._2).sum
        val coverageGoal = 0.8
        val (libTypes, achieved) =
          sortedTypes
            .zip(sortedTypes.scanLeft(0.0)(_ + _._2.toDouble / totalUsages))
            .takeWhile(_._2 < coverageGoal)
            .unzip

        printInfo(s"Coverages achieved: ${achieved.last}")
        printInfo(s"Lib types selected: $libTypes")

        libTypes.map(_._1).toSet
      }

      val projectsToUse = 6

      val data = projects
        .pipe(x => new Random(1).shuffle(x))
        .take(projectsToUse)
        .map {
          case (path, g, annotations) =>
            val predictor =
              Predictor(g, libTypesToPredict, libNodeType, Some(taskSupport))
            Datum(path, annotations.toMap, predictor)
              .tap(d => println(resultStr(d.showDetail)))
        }

      val totalNum = data.length
      val trainSetNum = totalNum - (totalNum * 0.2).toInt
      DataSet(data.take(trainSetNum), data.drop(trainSetNum))
        .tap(printResult)
    }
  }

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
      SM.saveObjectToFile(file.toIO)(toSave)
    }

    override def toString: String = {
      s"""TrainingState:
         |  step: $epoch
         |  dimMessage: $dimMessage
         |  iterationNum: $iterationNum
         |  optimizer: $optimizer
       """.stripMargin
    }
  }

  object TrainingState {
    def fromFile(file: Path): TrainingState = {
      val map = SM
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
    val inPSpaceRatio: Double =
      userAnnotations
        .count(
          _._2.pipe(predictor.predictionSpace.allTypes.contains),
        )
        .toDouble / userAnnotations.size

    def showInline: String =
      s"{name: $projectName, annotations: ${userAnnotations.size}, " +
        s"predicates: ${predictor.graph.predicates.size}, " +
        s"predictionSpace: ${predictor.predictionSpace.size}, " +
        s"inPSpaceRatio: $inPSpaceRatio}"

    override def toString: String = {
      showInline
    }

    def showDetail: String = {
      s"""$showInline
         |${predictor.predictionSpace}
         |nodes: ${predictor.graph.nodes}
       """.stripMargin
      showInline + s"\n${predictor.predictionSpace}"
    }
  }

  case class DataSet(
      trainSet: Vector[Datum],
      testSet: Vector[Datum],
  ) {
    override def toString: String =
      s"train set size: ${trainSet.size}, " +
        s"test set size: ${testSet.size}"
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
