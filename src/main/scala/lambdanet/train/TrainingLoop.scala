package lambdanet.train

import lambdanet._
import java.util.concurrent.ForkJoinPool

import ammonite.ops.Path
import botkop.numsca
import cats.Monoid
import funcdiff.{SimpleMath => SM}
import funcdiff._
import lambdanet.TrainingCenter.Timeouts
import lambdanet.architecture.{HybridArchitecture, NNArchitecture}
import lambdanet.utils.{EventLogger, ReportFinish}
import lambdanet.printWarning
import TrainingState._

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
    printResult("""Experiment description:
                  |Label: trainable random unit vec
                  |SingleLayer: 2 FC
                  |6 iterations """.stripMargin)

    run(
      maxTrainingEpochs = 1000,
      numOfThreads = Runtime.getRuntime.availableProcessors() min 14,
    ).result()
  }

  case class run(
      maxTrainingEpochs: Int,
      numOfThreads: Int,
  ) {

    printInfo(
      s"maxTrainingEpochs = $maxTrainingEpochs, numberOfThreads: $numOfThreads",
    )
    Timeouts.readFromFile()

    def result(): Unit = {
      val trainingState = loadTrainingState()
      val architecture =
        HybridArchitecture(trainingState.dimMessage, trainingState.pc)
      val dataSet = DataSet.loadDataSet(taskSupport, architecture)
      trainOnProjects(dataSet, trainingState, architecture).result()
    }

    //noinspection TypeAnnotation
    case class trainOnProjects(
        dataSet: DataSet,
        trainingState: TrainingState,
        architecture: NNArchitecture,
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
              if (!ex.isInstanceOf[StopException]) {
                saveTraining(epoch, "error-save")
              }
              throw ex
            }
        }
      }

      val logger: EventLogger = mkEventLogger()
      val stepsPerEpoch = trainSet.length max testSet.length
      val random = new util.Random(2)

      def trainStep(epoch: Int): Unit = {
        val startTime = System.nanoTime()

        val stats = random.shuffle(trainSet).zipWithIndex.map {
          case (datum, i) =>
            import Console.{GREEN, BLUE}
            announced(
              s"$GREEN(progress: ${i + 1}/${trainSet.size})$BLUE train on $datum",
            ) {
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

                (fwd, gradInfo, datum)
              }
            }
        }

        import cats.implicits._
        val (fws, gs, data) = stats.flatMap(_.toVector).unzip3
        val ForwardResult(loss, libAcc, projAcc, distanceAcc) = fws.combineAll
        val gradInfo = gs.combineAll
        logger.logScalar("loss", epoch, toAccuracyD(loss))
        logger.logScalar("libAcc", epoch, toAccuracy(libAcc))
        logger.logScalar("projAcc", epoch, toAccuracy(projAcc))
        logAccuracyDetails(data zip fws, epoch)
        val disMap = distanceAcc.toVector
          .sortBy(_._1)
          .map {
            case (k, counts) =>
              val ks = if (k == Analysis.Inf) "Inf" else k.toString
              ks -> toAccuracy(counts)
          }
        logger.logMap("distanceAcc", epoch, disMap)
        printResult("distance counts: " + distanceAcc.map {
          case (k, c) => k -> c.count
        })

        val (grads, deltas) = gradInfo.unzip
        logger.logScalar("gradientNorm", epoch, SM.mean(grads))
        logger.logScalar("paramDelta", epoch, SM.mean(deltas))

        val timeInSec = (System.nanoTime() - startTime).toDouble / 1e9
        logger.logScalar("iter-time", epoch, timeInSec)

        println(DebugTime.show)
      }

      private def logAccuracyDetails(
          stats: Vector[(Datum, ForwardResult)],
          epoch: Int,
      ) = {
        import cats.implicits._
        val str = stats
          .map {
            case (d, f) =>
              val size = d.predictor.graph.predicates.size
              val acc = toAccuracy(
                f.libCorrect.combine(f.projCorrect),
              )
              val name = d.projectName
              s"""{$size, $acc, "$name"}"""
          }
          .mkString("{", ",", "}")
        logger.logString("accuracy-distr", epoch, str)
      }

      def testStep(epoch: Int): Unit =
        if (epoch % 4 == 0) announced("test on dev set") {
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
              .run(architecture, nodesToPredict, iterationNum)
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
            val msg = s"$name exceeded time limit $timeLimit."
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

      @throws[StopException]
      private def checkShouldStop(epoch: Int): Unit = {
        if (TrainingControl.shouldStop(consumeFile = true)) {
          saveTraining(epoch, s"stopped-epoch$epoch")
          throw StopException("Stopped by 'stop.txt'.")
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

  def mkEventLogger() = {
    import ammonite.ops._
    new EventLogger(
      pwd / "running-result" / "log.txt",
      printToConsole = true,
      overrideMode = true,
    )
  }

}
