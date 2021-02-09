package lambdanet.train

import java.util.Calendar

import lambdanet._
import java.util.concurrent.ForkJoinPool

import botkop.numsca
import cats.Monoid
import funcdiff.{SimpleMath => SM}
import funcdiff._
import lambdanet.architecture._
import lambdanet.utils.{
  EmailService,
  EventLogger,
  FileLogger,
  QLangAccuracy,
  QLangDisplay,
  ReportFinish
}
import TrainingState._
import botkop.numsca.Tensor
import lambdanet.translation.PredicateGraph.{PAny, PNode, PType, ProjNode}
import org.nd4j.linalg.api.buffer.DataType

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.{
  Await,
  ExecutionContext,
  ExecutionContextExecutorService,
  Future,
  TimeoutException
}
import scala.language.reflectiveCalls
import scala.util.Random

object TrainingLoop {
  val toyMode: Boolean = false
  val useSeqModel = false
  val gnnIterations: Int = 6
  val useDropout: Boolean = true
  val useOracleForIsLib: Boolean = false
  val predictAny = true
  /* Assign more weights to project type to battle label imbalance */
  val maxLibRatio: Real = 2.0
  val projWeight: Real = maxLibRatio
  val gatHead = 1
  val weightDecay: Option[Real] = Some(1e-4)
  val onlyPredictLibType = false
  val lossAggMode: LossAggMode.Value = LossAggMode.Sum

  val debugTime: Boolean = false

  val taskName: String = {
    val flags = Seq(
      "fix" -> NeuralInference.fixBetweenIteration,
      "decay" -> weightDecay.nonEmpty,
      "with_any" -> predictAny,
      "lossAgg_sum" -> (lossAggMode == LossAggMode.Sum),
      "lib" -> onlyPredictLibType,
      "toy" -> toyMode
    ).map(flag(_)).mkString

    val ablationFlag = Seq(
      "noContextual" -> NeuralInference.noContextual,
      "noAttention" -> NeuralInference.noAttentional,
      "noLogical" -> NeuralInference.noLogical,
      "encodeSignature" -> NeuralInference.encodeLibSignature,
    ).map(flag(_, post = true)).mkString

    if (useSeqModel) "seqModel-theirName1-node"
    else
      s"${ablationFlag}NewData-GAT$gatHead-fc${NNArchitecture.messageLayers}" +
        s"$flags-${gnnIterations}"
  }

  def flag(nameValue: (String, Boolean), post: Boolean = false): String = {
    val (name, value) = nameValue
    if (value) (if (post) s"$name-" else s"-$name") else ""
  }

  def scaleLearningRate(epoch: Int): Double = {
    val min = 0.3
    val epochToSlowDown = if (toyMode) 100 else 30
    SimpleMath
      .linearInterpolate(1.0, min)(epoch.toDouble / epochToSlowDown)
      .max(min)
  }

  def gc(): Unit = DebugTime.logTime("GC") {
    System.gc()
  }

  def main(args: Array[String]): Unit = {
    NeuralInference.checkOMP()
    Tensor.floatingDataType = DataType.DOUBLE

    val threadNumber: Int = {
      import ammonite.ops._
      val f = pwd / "configs" / "threads.txt"
      if (exists(f)) {
        read(f).trim.toInt
      } else {
        Runtime.getRuntime.availableProcessors() / 2
      }
    }
    val resultsDir: ammonite.ops.Path = {
      import ammonite.ops._
      val pathText = read(pwd / "configs" / "resultsDir.txt").trim
      val path = util
        .Try {
          pwd / RelPath(pathText)
        }
        .getOrElse(Path(pathText))
      (path / taskName).tap { d =>
        lambdanet.printResult(s"save results to directory: $d")
      }
    }

    // the Gmail API no longer works.
//    val emailRelated = try {
//      val (mName, eService) = ReportFinish.readEmailInfo(taskName)
//      printInfo(s"Using email service at ${eService.user}")
//      Some(EmailRelated(mName, eService))
//    } catch {
//      case _: Exception =>
//        printWarning(
//          "No email service configuration founded. Will not " +
//            "report via email."
//        )
//        None
//    }

    config(
      threadNumber,
      resultsDir,
      emailRelated = None,
    ).result()
  }

  case class EmailRelated(machineName: String, emailService: EmailService)

  case class config(
      numOfThreads: Int,
      resultsDir: ammonite.ops.Path,
      emailRelated: Option[EmailRelated],
  ) {
    val fileLogger =
      new FileLogger(resultsDir / "console.txt", printToConsole = true)
    import fileLogger.{println, printInfo, printWarning, printResult, announced}

    printInfo(s"Task: $taskName")
    printInfo(s"threads = $numOfThreads")
    Timeouts.readFromFile()

    def makeModel(pc: ParamCollection, dataSet: DataSet) = {
      printResult(s"Message layer: ${NNArchitecture.messageLayers} FC")

      val dimMessage = if (TrainingLoop.useSeqModel) 64 else 32
      val architecture =
        if (useSeqModel)
          SequenceModel.SeqArchitecture(dimMessage, pc)
        else GATArchitecture(gatHead, dimMessage, pc)
      //        else SimpleArchitecture(state.dimMessage, pc)
      printResult(s"NN Architecture: ${architecture.arcName}")

      //      NamingBaseline.test(dataSet)
      //      MostFreqConstructorBaseline.test(dataSet, useByFreq = false)

      buildModel(
        dataSet,
        pc,
        architecture
      )
    }

    def result(): Unit = {
      val (state, pc, logger) = loadTrainingState(resultsDir, fileLogger)

      val repos = DataSet.loadRepos(toyMode, predictAny = predictAny)
      val dataSet = DataSet.makeDataSet(
        repos,
        taskSupport,
        useSeqModel,
        onlyPredictLibType,
        predictAny,
      )
      makeModel(pc, dataSet)
        .train(maxTrainingEpochs = if (toyMode) 90 else 100, state, logger)
        .run()
//      namingHelpfulness(run)
    }

    def namingHelpfulness(dataSet: DataSet, run: Model): Unit = {
      import cats.implicits._

      def showCount(c: Counted[Int]): String = {
        s"$c, percentage: ${toAccuracy(c)}"
      }

      val (helpSet, notHelpSet) =
        (for {
          datum <- dataSet.trainSet
          predictions = NamingBaseline
            .testOnDatum(datum, useOracle = true, identity)
            .predict(0)
          fwd = run
            .forwardStep(
              datum,
              shouldDownsample = false,
              shouldDropout = false,
              maxBatchSize = Some(600)
            )
            ._2
          Seq(s1, s2) = Seq(fwd.incorrectSet -> false, fwd.correctSet -> true)
            .map {
              case (set, correctness) =>
                set
                  .filterNot(_._2.madeFromLibTypes)
                  .toVector
                  .foldMap {
                    case (n, label, _) =>
                      val (truthPosition, label1) = predictions(ProjNode(n))
                      assert(label1 == label)
                      Counted.fromBool((truthPosition == 0) != correctness)
                  }
                  .tap { stat =>
                    val name = if (correctness) "unhelpful" else "help"
                    printResult(s"$name set: ${showCount(stat)}")
                  }
            }
        } yield (s1, s2)).combineAll

      printResult(s"Overall helpful set: ${showCount(helpSet)}")
      printResult(s"Overall unhelpful set: ${showCount(notHelpSet)}")
    }

    //noinspection TypeAnnotation
    case class buildModel(
        dataSet: DataSet,
        pc: ParamCollection,
        architecture: NNArchitecture,
    ) {
      import dataSet._
      val rand = new Random(1)
      val model =
        Model.fromData(dataSet, gnnIterations, architecture, lossAggMode, rand)

      val maxBatchSize = dataSet
        .signalSizeMedian(maxLibRatio)
        .tap(s => printResult(s"maxBatchSize: $s"))

      printResult(s"Label encoder: ${model.labelEncoder.name}")
      printResult(s"Name encoder: ${model.nameEncoder.name}")

      var shouldAnnounce: Boolean = true

      def forward(
          datum: ProcessedProject,
          shouldDownsample: Boolean,
          shouldDropout: Boolean,
          maxBatchSize: Option[Int]
      ) = {
        gc()
        limitTimeOpt("train-forward", Timeouts.forwardTimeout)(
          model.forwardStep(
            datum,
            shouldDownsample,
            shouldDownsample,
            maxBatchSize
          )
        )
      }

      case class train(
          maxTrainingEpochs: Int,
          trainingState: TrainingState,
          logger: EventLogger
      ) {
        import trainingState._

        def run(): Unit = {
          (trainingState.epoch0 + 1 to maxTrainingEpochs).foreach { epoch =>
            shouldAnnounce = epoch == 1 // only announce in the first epoch for debugging purpose
            announced(s"epoch $epoch") {
              TensorExtension.checkNaN = false // (epoch - 1) % 10 == 0
              handleExceptions(epoch) {
                trainStep(epoch)
                DebugTime.logTime("testSteps") {
                  testStep(epoch, isTestSet = false)
                  testStep(epoch, isTestSet = true)
                }
                if (epoch == 1 || epoch % saveInterval == 0)
                  DebugTime.logTime("saveTraining") {
                    saveTraining(epoch, s"epoch$epoch")
                  }
              }
            }
          }

          saveTraining(maxTrainingEpochs, "finished")
          emailRelated.foreach { params =>
            import params._
            emailService.sendMail(emailService.userEmail)(
              s"TypingNet: Training finished on $machineName!",
              "Training finished!"
            )
          }

        }

        val saveInterval = if (toyMode) 40 else 6

        def logAccuracyDetails(
            stats: Vector[(ProcessedProject, ForwardResult)],
            epoch: Int
        ) = {
          import cats.implicits._
          val str = stats
            .map {
              case (d, f) =>
                val size = d.predGraphOpt.map(_.predicates.size)
                val acc = toAccuracy(
                  f.libCorrect.combine(f.projCorrect)
                )
                val name = d.projectName
                s"""{$size, $acc, "$name"}"""
            }
            .mkString("{", ",", "}")
          logger.logString("accuracy-distr", epoch, str)
        }

        def trainStep(epoch: Int): Unit = {
          val startTime = System.nanoTime()
          val oldOrder = random.shuffle(trainSet)
          val (h, t) = oldOrder.splitAt(119)
          val stats = (t ++ h).zipWithIndex.map {
            case (datum, i) =>
              import Console.{GREEN, BLUE}
              announced(
                s"$GREEN[epoch $epoch]$BLUE train on $datum",
                shouldAnnounce
              ) {
                //              println(DebugTime.show)
                checkShouldStop(epoch)
                val (loss, fwd, _) = forward(
                  datum,
                  shouldDownsample = true,
                  shouldDropout = useDropout,
                  maxBatchSize = Some(maxBatchSize)
                ).get
                printResult(
                  s"[epoch $epoch] (progress: ${i + 1}/${trainSet.size}) " + fwd
                )
                checkShouldStop(epoch)

                def optimize(loss: CompNode) = {
                  gc()
                  val factor = fwd.loss.count.toDouble / avgAnnotations
                  optimizer.minimize(
                    loss * factor,
                    pc.allParams,
                    backPropInParallel = Some(parallelCtx -> Timeouts.optimizationTimeout),
                    gradientTransform = _.clipNorm(2 * factor),
                    scaleLearningRate = scaleLearningRate(epoch),
                    weightDecay = weightDecay
                  )
                }

                val gradInfo = limitTimeOpt(
                  s"optimization: $datum",
                  Timeouts.optimizationTimeout
                ) {
                  announced("optimization", shouldAnnounce = false) {
                    val stats = DebugTime.logTime("optimization") {
                      optimize(loss)
                    }
                    calcGradInfo(stats)
                  }
                }.toVector

                if (debugTime) {
                  println(DebugTime.show)
                }
                (fwd, gradInfo, datum)
              }
          }

          import cats.implicits._
          val (fws, gs, data) = stats.unzip3

          import logger._

          fws.combineAll.tap { fwd =>
            import fwd._
            logScalar("loss", epoch, toAccuracyD(loss))
            logScalar("libAcc", epoch, toAccuracy(libCorrect))
            logScalar("projAcc", epoch, toAccuracy(projCorrect))
            logConfusionMatrix("confusionMat", epoch, confusionMatrix.value, 2)
            logAccuracyDetails(data zip fws, epoch)
          }

          val gradInfo = gs.combineAll
          gradInfo.unzip3.tap {
            case (grads, transformed, deltas) =>
              logScalar("gradient", epoch, grads.sum)
              logScalar("clippedGradient", epoch, transformed.sum)
              logScalar("paramDelta", epoch, deltas.sum)
          }

          logScalar("nameSharpness", epoch, {
            architecture.layerFactory
              .getVar('decodingSharpness)(
                Tensor(0.1).reshape(1, 1)
              )
              .value
              .squeeze()
          })

          val timeInSec = (System.nanoTime() - startTime).toDouble / 1e9
          logScalar("iter-time", epoch, timeInSec)

          println(DebugTime.show)
        }

        def testStep(epoch: Int, isTestSet: Boolean): Unit = {
          val dataSetName = if (isTestSet) "test" else "dev"
          val dataSet = if (isTestSet) testSet else devSet
          announced(s"test on $dataSetName set") {
            import cats.implicits._

            val (stat, fse1Acc, libTop5Acc, projTop5Acc) =
              dataSet.zipWithIndex.flatMap {
                case (datum, i) =>
                  checkShouldStop(epoch)
                  announced(s"test on $datum", shouldAnnounce) {
                    forward(
                      datum,
                      shouldDownsample = !isTestSet,
                      shouldDropout = false,
                      maxBatchSize = Some(maxBatchSize)
                    ).map {
                      case (_, fwd, pred) =>
                        val (fse1, _, _) = datum.fseAcc
                          .countTopNCorrect(
                            1,
                            pred.mapValuesNow(_.distr.map(_._2)),
                            onlyCountInSpaceTypes = true
                          )
                        val Seq(libTop5, projTop5) = Seq(true, false).map { fromLib =>
                          val predictions = pred.map {
                            case (n, distr) =>
                              n -> distr.distr.take(5).map(_._2)
                          }
                          val nodesMap = datum.nodesToPredict.collect {
                            case (n, ty)
                                if predictions.contains(n.n) && ty.madeFromLibTypes == fromLib =>
                              n.n -> ty
                          }
                          QLangAccuracy
                            .countTopNCorrect(
                              5,
                              nodesMap,
                              predictions,
                              _ => 1
                            )
                            ._1
                        }
                        (fwd, fse1, libTop5, projTop5).tap { _ =>
                          printResult(s"(progress: ${i + 1}/${dataSet.size})")
                        }
                    }.toVector
                  }
              }.combineAll

            import stat.{libCorrect, projCorrect, confusionMatrix, categoricalAcc}
            import logger._
            logScalar(s"$dataSetName-loss", epoch, toAccuracyD(stat.loss))
            logScalar(s"$dataSetName-libAcc", epoch, toAccuracy(libCorrect))
            logScalar(s"$dataSetName-libTop5Acc", epoch, toAccuracy(libTop5Acc))
            logScalar(s"$dataSetName-projAcc", epoch, toAccuracy(projCorrect))
            logScalar(
              s"$dataSetName-projTop5Acc",
              epoch,
              toAccuracy(projTop5Acc)
            )
            printResult(
              s"lib targets: ${libCorrect.count}, proj targets: ${projCorrect.count}"
            )
            logConfusionMatrix(
              s"$dataSetName-confusionMat",
              epoch,
              confusionMatrix.value,
              2
            )
            logScalar(s"$dataSetName-fse-top1", epoch, toAccuracy(fse1Acc))
            val libTypeAcc = categoricalAcc.filter(_._1.madeFromLibTypes)
            logString(
              s"$dataSetName-typeAcc",
              epoch,
              typeAccString(libTypeAcc)
            )
            val projTypeAcc = categoricalAcc.filterNot(_._1.madeFromLibTypes)
            logString(
              s"$dataSetName-proj-typeAcc",
              epoch,
              typeAccString(projTypeAcc)
            )
          }
        }

        import ammonite.ops._

        private def saveTraining(
            epoch: Int,
            dirName: String,
            skipTest: Boolean = false
        ): Unit = {
          announced(s"save training to '$dirName'") {
            val saveDir = resultsDir / "saved" / dirName
            if (!exists(saveDir)) {
              mkdir(saveDir)
            }
            // do the following tasks in parallel
            val tasks = Vector(
              () => {
                val savePath = saveDir / "state.serialized"
                announced("save training state") {
                  TrainingState(epoch, optimizer)
                    .saveToFile(savePath)
                }
              },
              () => {
                announced("save model") {
                  SimpleMath.saveObjectToFile((saveDir / "model.serialized").toIO)(model)
                }
              },
              () => {
                val currentLogFile = resultsDir / "log.txt"
                if (exists(currentLogFile)) {
                  cp.over(currentLogFile, saveDir / "log.txt")
                }
              },
              () =>
                if (testSet.nonEmpty && !skipTest) {
                  import cats.implicits._

                  val (right, wrong) = testSet.zipWithIndex.flatMap {
                    case (datum, i) =>
                      checkShouldStop(epoch)
                      forward(
                        datum,
                        shouldDownsample = false,
                        shouldDropout = false,
                        maxBatchSize = None
                      ).map {
                        case (_, fwd, pred) =>
                          printResult(
                            s"(progress: ${i + 1}/${testSet.size})"
                          )
                          DebugTime.logTime("printQSource") {
                            QLangDisplay.renderProjectToDirectory(
                              datum.projectName.toString,
                              datum.qModules,
                              pred,
                              datum.predictionSpace.allTypes
                            )(saveDir / "predictions")
                          }
                          (fwd.correctSet, fwd.incorrectSet)
                      }.toVector
                  }.combineAll

                  QLangDisplay.renderPredictionIndexToDir(
                    right,
                    wrong,
                    saveDir,
                    sourcePath = "predictions"
                  )
                }
            )

            tasks.par.foreach(_.apply())

            val dateTime = Calendar.getInstance().getTime
            write.over(saveDir / "time.txt", dateTime.toString)
          }
        }

        @throws[StopException]
        private def checkShouldStop(epoch: Int): Unit = {
          if (TrainingControl(resultsDir).shouldStop(consumeFile = true)) {
            saveTraining(epoch, s"stopped-epoch$epoch")
            throw StopException("Stopped by 'stop.txt'.")
          }
        }

        private def handleExceptions(epoch: Int)(f: => Unit): Unit = {
          try f
          catch {
            case ex: Throwable =>
              val isTimeout = ex.isInstanceOf[TimeoutException]
              val errorName = if (isTimeout) "timeout" else "stopped"
              emailRelated.foreach { p =>
                import p._
                emailService.sendMail(emailService.userEmail)(
                  s"TypingNet: $errorName on $machineName at epoch $epoch",
                  s"Details:\n" + ex.getMessage
                )
              }

              if (isTimeout && Timeouts.restartOnTimeout) {
                printWarning(
                  "Timeout... training restarted (skip one training epoch)..."
                )
                gc()
              } else {
                if (!ex.isInstanceOf[StopException]) {
                  ex.printStackTrace()
                  gc()
                  saveTraining(epoch, "error-save", skipTest = true)
                }
                throw ex
              }
          }
        }
      }

      val random = new util.Random(2)

      private def typeAccString(accs: Map[PType, Counted[Correct]]): String = {
        val (tys, counts) = accs.toVector.sortBy { c =>
          -c._2.count
        }.unzip
        val typeStr = tys
          .map(t => SM.wrapInQuotes(t.showSimple))
          .mkString("{", ",", "}")
        val countStr = counts
          .map(c => s"{${c.count}, ${c.value}}")
          .mkString("{", ",", "}")
        s"{$typeStr,$countStr}"
      }

      def calcGradInfo(stats: Optimizer.OptimizeStats) = {
        def meanSquaredNorm(gs: Iterable[Gradient]) = {
          import numsca._
          import cats.implicits._
          val combined = gs.toVector.map { g =>
            val t = g.toTensor()
            Counted(t.elements.toInt, sum(square(t)))
          }.combineAll
          math.sqrt(combined.value / nonZero(combined.count))
        }

        val grads = meanSquaredNorm(stats.gradients.values)
        val transformed = meanSquaredNorm(stats.transformedGrads.values)
        val deltas = meanSquaredNorm(stats.deltas.values)
        (grads, transformed, deltas)
      }

      printResult(s"loss agg mode: ${lossAggMode}")

      private def limitTimeOpt[A](
          name: String,
          timeLimit: Timeouts.Duration
      )(f: => A): Option[A] = {
        try {
          Some(limitTime(timeLimit)(f))
        } catch {
          case e: TimeoutException =>
            val msg = s"$name exceeded time limit $timeLimit."
            printWarning(msg)
            throw e
//            emailRelated.foreach { e =>
//              import e._
//              emailService.atFirstTime {
//                emailService.sendMail(emailService.userEmail)(
//                  s"TypingNet: timeout on $machineName during $name",
//                  s"Details:\n" + msg
//                )
//              }
//            }
//
//            None
        }
      }

      @throws[TimeoutException]
      private def limitTime[A](timeLimit: Timeouts.Duration)(f: => A): A = {
        val exec = scala.concurrent.ExecutionContext.global
        Await.result(Future(f)(exec), timeLimit)
      }

      private val avgAnnotations =
        SM.mean(
          trainSet.map(_.downsampleLibAnnots(maxLibRatio, random).size.toDouble)
        )
    }

    private val pool = new ForkJoinPool(numOfThreads)
    val taskSupport: Option[ForkJoinTaskSupport] =
      if (numOfThreads == 1) None
      else Some(new ForkJoinTaskSupport(pool))
    val parallelCtx: ExecutionContextExecutorService = {
      import ExecutionContext.fromExecutorService
      fromExecutorService(pool)
    }
  }

  case class ForwardResult(
      loss: Counted[Double],
      correctSet: Set[(PNode, PType, ProjectPath)],
      incorrectSet: Set[(PNode, PType, ProjectPath)],
      confusionMatrix: Counted[ConfusionMatrix],
      categoricalAcc: Map[PType, Counted[Correct]]
  ) {
    override def toString: String = {
      s"forward result: {loss: ${toAccuracyD(loss)}, " +
        s"lib acc: ${toAccuracy(libCorrect)} (${libCorrect.count} nodes), " +
        s"proj acc: ${toAccuracy(projCorrect)} (${projCorrect.count} nodes)}"
    }

    private def countCorrect(isLibType: Boolean) = {
      filterCount(_._2.madeFromLibTypes == isLibType)
    }

    private def filterCount(
        filter: ((PNode, PType, ProjectPath)) => Boolean
    ) = {
      val correct = correctSet.count(filter)
      val incorrect = incorrectSet.count(filter)
      Counted(correct + incorrect, correct)
    }

    def libCorrect: Counted[LibCorrect] = countCorrect(true)
    def projCorrect: Counted[ProjCorrect] = countCorrect(false)

    def isConsistent: Boolean = {
      categoricalAcc.keySet == (correctSet ++ incorrectSet).map(_._2)
    }
  }

  private implicit val forwardResultMonoid: Monoid[ForwardResult] =
    new Monoid[ForwardResult] {
      import Counted.zero
      import cats.implicits._

      def empty: ForwardResult =
        ForwardResult(zero(0), Set(), Set(), zero(Map()), Map())

      def combine(x: ForwardResult, y: ForwardResult): ForwardResult = {
        val z = ForwardResult.unapply(x).get |+| ForwardResult
          .unapply(y)
          .get
        (ForwardResult.apply _).tupled(z)
      }
    }

}
