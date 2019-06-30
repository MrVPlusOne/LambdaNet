package lambdanet.train

import lambdanet._
import java.util.concurrent.{ForkJoinPool}

import ammonite.ops.Path
import botkop.numsca
import cats.Monoid
import funcdiff.{SimpleMath => SM}
import funcdiff._
import lambdanet.TrainingCenter.Timeouts
import lambdanet.utils.{EventLogger, ReportFinish}
import lambdanet.{printWarning}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.{
  Await,
  ExecutionContext,
  ExecutionContextExecutorService,
  Future,
  TimeoutException,
}
import scala.language.reflectiveCalls

object TrainingLoop {

  /** Remember to use these VM options to increase memory limits.
    * VM Options: -Xms2G -Xmx8G -Dorg.bytedeco.javacpp.maxbytes=18G -Dorg.bytedeco.javacpp.maxphysicalbytes=27G */
  def main(args: Array[String]): Unit = {
    run(
      maxTrainingEpochs = 1000,
      numOfThreads = Runtime.getRuntime.availableProcessors() min 14,
    ).result()
  }

  case class run(
      maxTrainingEpochs: Int,
      numOfThreads: Int,
  ) {

    printInfo(s"maxTrainingEpochs = $maxTrainingEpochs, numberOfThreads: $numOfThreads")

    def result(): Unit = {
      val trainingState = loadTrainingState()
      val dataSet = DataSet.loadDataSet(taskSupport)
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
              epoch0 = 0,
              dimMessage = 32,
              optimizer = Optimizer.Adam(learningRate = 1e-3),
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
        (trainingState.epoch0 + 1 to maxTrainingEpochs)
          .foreach { epoch =>
            announced(s"epoch $epoch") {
              handleExceptions(epoch) {
                trainStep(epoch)
                testStep(epoch)
              }

              if (epoch % 10 == 0) {
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

        val stats = trainSet.zipWithIndex.map {
          case (datum, i) =>
            announced(s"(progress: ${i + 1}/${trainSet.size}) train on $datum") {
              checkShouldStop(epoch)
              for {
                (loss, fwd) <- forward(datum).tap(
                  _.foreach(r => printResult(r._2)),
                )
                _ = checkShouldStop(epoch)
              } yield {
                checkShouldStop(epoch)
                def optimize(loss: CompNode) = optimizer.minimize(
                  loss * fwd.loss.count / avgAnnotations,
                  pc.allParams,
                  backPropInParallel =
                    Some(parallelCtx -> Timeouts.optimizationTimeout),
                  // weightDecay = Some(5e-5),  todo: use dropout
                )

                val gradInfo = limitTimeOpt(
                  s"optimization: $datum",
                  Timeouts.optimizationTimeout,
                ) {
                  announced("optimization") {
                    val stats = DebugTime.logTime("optimization") {
                      optimize(loss)
                    }
                    calcGradInfo(stats)
                  }
                }.toVector

                DebugTime.logTime("GC") {
                  System.gc()
                }

                (fwd, gradInfo)
              }
            }
        }

        import cats.implicits._
        val (ForwardResult(loss, libAcc, projAcc, distanceAcc), gradInfo) =
          stats.flatMap(_.toVector).combineAll
        logger.logScalar("loss", epoch, toAccuracyD(loss))
        logger.logScalar("libAcc", epoch, toAccuracy(libAcc))
        logger.logScalar("projAcc", epoch, toAccuracy(projAcc))
        logger.logMap(
          "distanceAcc",
          epoch,
          distanceAcc.toVector
            .sortBy(_._1)
            .map {
              case (k, counts) =>
                val ks = if (k == Analysis.Inf) "Inf" else k.toString
                ks -> toAccuracy(counts)
            },
        )

        val (grads, deltas) = gradInfo.unzip
        logger.logScalar("gradientNorm", epoch, SM.mean(grads))
        logger.logScalar("paramDelta", epoch, SM.mean(deltas))

        val timeInSec = (System.nanoTime() - startTime).toDouble / 1e9
        logger.logScalar("iter-time", epoch, timeInSec)

        println(DebugTime.show)
      }

      def testStep(epoch: Int): Unit =
        if (epoch % 5 == 0) announced("test on dev set") {
          import cats.implicits._

          val stat = testSet.flatMap { datum =>
            checkShouldStop(epoch)
            announced(s"test on $datum") {
              forward(datum).map(_._2).toVector
            }
          }.combineAll

          import stat.{libCorrect, projCorrect}
          logger.logScalar("test-libAcc", epoch, toAccuracy(libCorrect))
          logger.logScalar("test-projAcc", epoch, toAccuracy(projCorrect))
        }

      private def logMemoryUsage(epoch: Double): Unit ={
        import java.lang.management.{ManagementFactory => F}
        val bean = F.getMemoryMXBean
        val GB = 1024*1024*1024

        val heap = bean.getHeapMemoryUsage.getUsed.toDouble / GB
        val nonHeap = bean.getNonHeapMemoryUsage.getUsed.toDouble / GB
        F.getMemoryManagerMXBeans
        logger.logScalar("memory-heap", epoch, heap)
        logger.logScalar("memory-nonHeap", epoch, nonHeap)
      }

      case class ForwardResult(
          loss: Counted[Double],
          libCorrect: Counted[LibCorrect],
          projCorrect: Counted[ProjCorrect],
          distCorrectMap: Map[Int, Counted[Correct]],
      ) {
        override def toString: String = {
          s"forward result: {loss: ${toAccuracyD(loss)}, " +
            s"lib acc: ${toAccuracy(libCorrect)} (${libCorrect.count} nodes), " +
            s"proj acc: ${toAccuracy(projCorrect)} (${projCorrect.count} nodes)}"
        }
      }

      def calcGradInfo(stats: Optimizer.OptimizeStats): (Real, Real) = {
        def meanSquaredNorm(gs: Iterable[Gradient]) = {
          import numsca._
          import cats.implicits._
          val combined = gs.toVector.map { g =>
            val t = g.toTensor()
            Counted(t.elements, sum(square(t)))
          }.combineAll
          math.sqrt(combined.value / nonZero(combined.count))
        }

        val grads = meanSquaredNorm(stats.gradients.values)
        val deltas = meanSquaredNorm(stats.deltas.values)
        grads -> deltas
      }

      implicit val forwardResultMonoid: Monoid[ForwardResult] =
        new Monoid[ForwardResult] {
          import Counted.zero
          import cats.implicits._

          def empty: ForwardResult =
            ForwardResult(zero(0), zero(0), zero(0), Map())

          def combine(x: ForwardResult, y: ForwardResult): ForwardResult = {
            val z = ForwardResult.unapply(x).get |+| ForwardResult
              .unapply(y)
              .get
            (ForwardResult.apply _).tupled(z)
          }
        }

      private def forward(datum: Datum): Option[(Loss, ForwardResult)] =
        limitTimeOpt(s"forward: $datum", Timeouts.forwardTimeout) {
          import datum._

          val nodesToPredict = annotations.keys.toVector
          val predSpace = predictor.predictionSpace

          val logits = announced("run predictor") {
            predictor
              .run(dimMessage, layerFactory, nodesToPredict, iterationNum)
              .result
          }

          val groundTruths = nodesToPredict.map(annotations)
          val targets = groundTruths.map(predSpace.indexOfType)
          val isFromLib = groundTruths.map(_.madeFromLibTypes)
          val nodeDistances = nodesToPredict.map(_.n.pipe(distanceToConsts))

          assert(logits.shape(1) == predSpace.size)
          val (libCounts, projCounts, distanceCounts) =
            announced("compute training accuracy") {
              analyzeLogits(
                logits,
                targets,
                isFromLib,
                nodeDistances,
              )
            }

          val loss = predictionLoss(logits, targets, predSpace.size)

          val totalCount = libCounts.count + projCounts.count
          loss -> ForwardResult(
            Counted(totalCount, loss.value.squeeze() * totalCount),
            libCounts,
            projCounts,
            distanceCounts,
          )
        }

      @throws[TimeoutException]
      private def limitTime[A](timeLimit: Timeouts.Duration)(f: => A): A = {
        val exec = scala.concurrent.ExecutionContext.global
        Await.result(Future(f)(exec), timeLimit)
      }

      private def limitTimeOpt[A](
          name: String,
          timeLimit: Timeouts.Duration,
      )(f: => A): Option[A] = {
        try {
          Some(limitTime(timeLimit)(f))
        } catch {
          case _: TimeoutException =>
            val msg= s"$name exceeded time limit $timeLimit."
            printWarning(msg)
            emailService.atFirstTime {
              emailService.sendMail(emailService.userEmail)(
                s"TypingNet: timeout on $machineName during $name",
                s"Details:\n" + msg,
              )
            }
            None
        }
      }

      private def saveTraining(epoch: Int, dirName: String): Unit = {
        import ammonite.ops._

        announced(s"save training to $dirName") {
          val saveDir = pwd / "running-result" / "saved" / dirName
          if (!exists(saveDir)) {
            mkdir(saveDir)
          }
          val savePath = saveDir / "trainingState.serialized"
          TrainingState(epoch, dimMessage, iterationNum, optimizer, pc)
            .saveToFile(
              savePath,
            )
        }
      }

      @throws[Exception]
      private def checkShouldStop(epoch: Int): Unit = {
        if (TrainingControl.shouldStop(consumeFile = true)) {
          saveTraining(epoch, s"stopped-epoch$epoch")
          throw new Exception("Stopped by 'stop.txt'.")
        }
      }

      private def analyzeLogits(
          logits: CompNode,
          targets: Vector[Int],
          targetFromLibrary: Vector[Boolean],
          nodeDistances: Vector[Int],
      ): (
          Counted[LibCorrect],
          Counted[ProjCorrect],
          Map[Int, Counted[Correct]],
      ) = {
        val predictions = numsca
          .argmax(logits.value, axis = 1)
          .data
          .map(_.toInt)
          .toVector
        val truthValues = predictions.zip(targets).map { case (x, y) => x == y }
        val zipped = targetFromLibrary.zip(truthValues)
        val libCorrect = zipped.collect {
          case (true, true) => ()
        }.length
        val projCorrect = zipped.collect {
          case (false, true) => ()
        }.length
        val libCounts = Counted(targetFromLibrary.count(identity), libCorrect)
        val projCounts = Counted(targetFromLibrary.count(!_), projCorrect)

        val distMap = nodeDistances
          .zip(truthValues)
          .groupBy(_._1)
          .mapValuesNow { vec =>
            Counted(vec.length, vec.count(_._2))
          }

        (libCounts, projCounts, distMap)
      }

      private val avgAnnotations =
        SM.mean(trainSet.map(_.annotations.size.toDouble))
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
  }

  case class TrainingState(
      epoch0: Int,
      dimMessage: Int,
      iterationNum: Int,
      optimizer: Optimizer,
      pc: ParamCollection,
  ) {
    def saveToFile(file: Path): Unit = {
      val toSave =
        List[(String, Any)](
          "epoch" -> epoch0,
          "dimMessage" -> dimMessage,
          "iterationNum" -> iterationNum,
          "optimizer" -> optimizer,
          "pcData" -> pc.toSerializable,
        )
      SM.saveObjectToFile(file.toIO)(toSave)
    }

    override def toString: String = {
      s"""TrainingState:
         |  step: $epoch0
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

  def mkEventLogger() = {
    import ammonite.ops._
    new EventLogger(
      pwd / "running-result" / "log.txt",
      printToConsole = true,
      overrideMode = true,
    )
  }

}
