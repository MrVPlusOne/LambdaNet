package lambdanet.train

import java.util.Calendar
import lambdanet._

import java.util.concurrent.ForkJoinPool
import botkop.numsca
import cats.Monoid
import funcdiff.{SimpleMath => SM}
import funcdiff._
import lambdanet.architecture._
import lambdanet.utils.{EventLogger, FileLogger, QLangAccuracy, QLangDisplay}
import TrainingState._
import ammonite.{ops => amm}
import botkop.numsca.Tensor
import lambdanet.translation.PredicateGraph.{PNode, PType, ProjNode}
import me.tongfei.progressbar.ProgressBar
import upickle.{default => pickle}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}
import scala.language.reflectiveCalls
import scala.util.Random

object Training {

  val debugTime: Boolean = false

  def main(args: Array[String]): Unit = {
    checkMemoryConfigs()

    val dropAllAnnots = false
    val modelConfig = ModelConfig(
      predictAny = false,
      annotsSampling =
        if (dropAllAnnots) AnnotsSampling(0, 0) else AnnotsSampling(0.0, 0.81),
      maxLibRatio = 100.0,
      gatHeads = 1,
    )

    val configs = Configs()
    val resultsDir = configs.resultsDir() / modelConfig.taskName
    lambdanet.printResult(s"save results to directory: $resultsDir")
    val trainConfig = SystemConfig(configs.numOfThreads(), resultsDir)

    val resultsDirEmpty = !amm.exists(resultsDir) || amm.ls(resultsDir).isEmpty
    if (!resultsDirEmpty) {
      println(warnStr(s"directory $resultsDir is not empty. Remove it first? (y/n): "))
      if (scala.io.StdIn.readLine().trim().toLowerCase() == "y") {
        amm.rm(resultsDir)
      } else {
        printInfo("Training aborted.")
        System.exit(0)
      }
    }
    amm.mkdir(resultsDir / "control")

    Trainer(
      modelConfig,
      trainConfig,
    ).trainModel()
  }

  def scaleLearningRate(toyMode: Boolean, epoch: Int): Double = {
    val min = 0.3
    val epochToSlowDown = if (toyMode) 100 else 30
    SimpleMath
      .linearInterpolate(1.0, min)(epoch.toDouble / epochToSlowDown)
      .max(min)
  }

  def gc(): Unit = DebugTime.logTime("GC") {
    System.gc()
  }

  case class ForwardResult(
      loss: Counted[Double],
      kept: Set[(ProjNode, ProjectPath)],
      dropped: Set[(ProjNode, ProjectPath)],
      correctSet: Set[(PNode, PType, ProjectPath)],
      incorrectSet: Set[(PNode, PType, ProjectPath)],
      confusionMatrix: Counted[ConfusionMatrix],
      categoricalAcc: Map[PType, Counted[Correct]]
  ) {
    assert(correctSet.size + incorrectSet.size <= dropped.size)

    override def toString: String = {
      val allNodes = correctSet ++ incorrectSet
      val nLib = allNodes.count(_._2.madeFromLibTypes)
      val nProj = allNodes.size - nLib
      val nTotal = nLib + nProj
      s"forward result: {loss: ${toAccuracyD(loss)}, " +
        s"total_acc: %.4f ($nTotal nodes), ".format(totalAccuracy) +
        s"lib_acc: %.4f ($nLib nodes), ".format(toAccuracy(libCorrect)) +
        s"proj_acc: %.4f ($nProj nodes), ".format(toAccuracy(projCorrect)) +
        s"user_annotations: ${kept.size}, " +
        s"dropped_annotations: ${dropped.size}}"
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
    def totalCorrect: Counted[Correct] =
      Counted(correctSet.size + incorrectSet.size, correctSet.size)
    def totalAccuracy: Double = toAccuracy(totalCorrect)

    def isConsistent: Boolean = {
      categoricalAcc.keySet == (correctSet ++ incorrectSet).map(_._2)
    }
  }

  private implicit val forwardResultMonoid: Monoid[ForwardResult] =
    new Monoid[ForwardResult] {
      import Counted.zero
      import cats.implicits._

      def empty: ForwardResult =
        ForwardResult(zero(0), Set(), Set(), Set(), Set(), zero(Map()), Map())

      def combine(x: ForwardResult, y: ForwardResult): ForwardResult = {
        val z = ForwardResult.unapply(x).get |+| ForwardResult.unapply(y).get
        (ForwardResult.apply _).tupled(z)
      }
    }

  case class Trainer(
      modelConfig: ModelConfig,
      trainConfig: SystemConfig,
  ) {
    import modelConfig._
    import trainConfig._

    val fileLogger =
      new FileLogger(resultsDir / "console.txt", printToConsole = true)
    import fileLogger.{println, printInfo, printWarning, printResult, announced}
    Timeouts.readFromFile()

    private val pool = new ForkJoinPool(numOfThreads)
    val taskSupport: Option[ForkJoinTaskSupport] =
      if (numOfThreads == 1) None
      else Some(new ForkJoinTaskSupport(pool))
    val parallelCtx: concurrent.ExecutionContextExecutorService = {
      import ExecutionContext.fromExecutorService
      fromExecutorService(pool)
    }

    def trainModel(): Unit = {
      printInfo(s"Task: $taskName")
      printInfo(s"threads = $numOfThreads")

      val (state, pc, logger) = loadTrainingState(resultsDir, fileLogger)

      val configStr = modelConfig.toJsonString
      printInfo(s"Config: $configStr")
      ammonite.ops.write(resultsDir / "modelConfig.json", configStr)

      val repos = DataSet.loadRepos(toyMode, predictAny = predictAny)
      val dataSet = DataSet.makeDataSet(
        repos,
        onlyPredictLibType,
        predictAny,
      )

      val model = makeModel(pc, dataSet)
      val maxTrainingEpochs = if (toyMode) 90 else 100
      val earlyStopEpochs = if (toyMode) 30 else 10
      TrainingLoop(dataSet, model, state, logger).run(maxTrainingEpochs, earlyStopEpochs)
    }

    def makeModel(pc: ParamCollection, dataSet: DataSet) = {
      printResult(s"Message layer: ${NNArchitecture.messageLayers} FC")

      val architecture =
        if (useSeqModel)
          SequenceModel.SeqArchitecture(dimMessage, pc)
        else GATArchitecture(gatHeads, dimMessage, pc)
      //        else SimpleArchitecture(state.dimMessage, pc)
      printResult(s"NN Architecture: ${architecture.arcName}")

      //      NamingBaseline.test(dataSet)
      //      MostFreqConstructorBaseline.test(dataSet, useByFreq = false)

      val rand = new Random(1)
      val model = announced("Building model") {
        Model.fromData(modelConfig, dataSet, architecture, rand)
      }

      ammonite.ops.write(resultsDir / "model_config.serial", model.description)
      ModelWrapper(model)
    }

    def namingHelpfulness(dataSet: DataSet, model: Model): Unit = {
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
          fwd = model
            .forwardStep(
              datum,
              annotsSampling = annotsSampling,
              shouldDropout = false,
              maxLibRatio = None,
              maxBatchSize = Some(600),
              projWeight = projWeight,
              taskSupport = taskSupport,
            )(ModeEval)
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
    case class ModelWrapper(
        model: Model,
    ) {

      printResult(model.description)
      ammonite.ops.write(resultsDir / "model_params.txt", model.description)

      var shouldAnnounce: Boolean = true

      def forward(
          datum: ProcessedProject,
          shouldDownsample: Boolean,
          shouldDropout: Boolean,
          maxBatchSize: Option[Int]
      )(implicit mode: GraphMode) = {
        gc()

        limitTimeOpt("train-forward", Timeouts.forwardTimeout)(
          model.forwardStep(
            datum,
            annotsSampling,
            shouldDropout,
            maxLibRatio = if (shouldDropout) Some(maxLibRatio) else None,
            maxBatchSize,
            projWeight,
            taskSupport = taskSupport,
          )
        )
      }

      val random = new util.Random(2)

      printResult(s"loss agg mode: ${lossAggMode}")
    }

    case class TrainingLoop(
        dataSet: DataSet,
        modelWrapper: ModelWrapper,
        trainingState: TrainingState,
        logger: EventLogger,
    ) {
      import dataSet._
      import modelWrapper._
      import trainingState._
      val architecture: NNArchitecture = modelWrapper.model.architecture
      val pc: ParamCollection = architecture.pc
      type Epoch = Int

      val (maxBatchSize, avgAnnotations) = {
        val sizes = dataSetLabelNums(dataSet.trainSet, random)
        (SM.median(sizes), SM.mean(sizes.map(_.toDouble)))
      }
      printResult(s"maxBatchSize: $maxBatchSize, avgAnnotations: $avgAnnotations")

      def run(maxTrainingEpochs: Int, earlyStopEpochs: Int): Unit = {
        TensorExtension.checkNaN = false // (epoch - 1) % 10 == 0

        val epochCost = trainSet.size * 3 + devSet.size + testSet.size
        val epochs = trainingState.epoch0 + 1 to maxTrainingEpochs
        var bestModel: Option[(Epoch, ForwardResult)] = None
        var shouldStop = false
        val pb = new ProgressBar("Training", epochs.size * epochCost)

        def loopStep(epoch: Epoch): Unit = {
          trainStep(epoch, pb)
          val fr = DebugTime.logTime("testSteps") {
            testStep(epoch, pb, isTestSet = true) // test set
            testStep(epoch, pb, isTestSet = false) // dev set, used for early stopping
          }
          if (bestModel.isEmpty || fr.totalAccuracy > bestModel.get._2.totalAccuracy) {
            bestModel = Some((epoch, fr))
            DebugTime.logTime("saveTraining") {
              saveTraining(epoch, "bestModel", skipTest = false)
            }
          }
          if (epoch == 1 || epoch % saveInterval == 0)
            DebugTime.logTime("saveTraining") {
              saveTraining(epoch, s"epoch$epoch", skipTest = true)
            }

          printInfo(s"[[epoch $epoch]] dev set performance: $fr")
          val lastImprove = bestModel.get._1
          if (epoch - lastImprove >= earlyStopEpochs) {
            shouldStop = true
            printInfo(
              s"Early stopping triggered since the last improvement was " +
                s"at epoch $lastImprove."
            )
          }
        }

        epochs.foreach { epoch =>
          shouldAnnounce = epoch == 1 // only announce in the first epoch for debugging purpose
          if (!shouldStop)
            announced(s"epoch $epoch") {
              handleExceptions(epoch) { loopStep(epoch) }
            }
        }

        printInfo("Training finished.")
        val (bestEpoch, performance) = bestModel.get
        printInfo(s"{Best_epoch: $bestEpoch, test_performance: $performance}")
      }

      val saveInterval = if (toyMode) 40 else 5

      def logAccuracyDetails(
          stats: Vector[(ProcessedProject, ForwardResult)],
          epoch: Int
      ) = {
        import cats.implicits._
        val str = stats
          .map {
            case (d, f) =>
              val size = d.graph.predicates.size
              val acc = toAccuracy(
                f.libCorrect.combine(f.projCorrect)
              )
              val name = d.projectName
              s"""{$size, $acc, "$name"}"""
          }
          .mkString("{", ",", "}")
        logger.logString("accuracy-distr", epoch, str)
      }

      def trainStep(epoch: Int, pb: ProgressBar): Unit = {
        import Console.{GREEN, BLUE}
        implicit val m: GraphMode = ModeTraining

        val startTime = System.nanoTime()
        val stats = random.shuffle(trainSet).zipWithIndex.map {
          case (datum, i) =>
            announced(
              s"$GREEN[epoch $epoch]$BLUE train on $datum",
              shouldAnnounce
            ) {
              checkShouldStop(epoch)
              pb.setExtraMessage("forward ")
              val (loss, fwd, _) = forward(
                datum,
                shouldDownsample = true,
                shouldDropout = useDropout,
                maxBatchSize = Some(maxBatchSize)
              ).get
              printResult(
                s"[epoch $epoch] (progress: ${i + 1}/${trainSet.size}) " + fwd
              )
              pb.step()
              checkShouldStop(epoch)

              def optimize(loss: CompNode) = {
                gc()
                val factor = fwd.loss.count.toDouble / avgAnnotations
                optimizer.minimize(
                  loss * factor,
                  pc.allParams,
                  backPropInParallel = Some(parallelCtx -> Timeouts.optimizationTimeout),
                  gradientTransform = _.clipNorm(2 * factor),
                  scaleLearningRate = scaleLearningRate(toyMode, epoch),
                  weightDecay = weightDecay
                )
              }

              val gradInfo = limitTimeOpt(
                s"optimization: $datum",
                Timeouts.optimizationTimeout
              ) {
                pb.setExtraMessage("optimize")
                announced("optimization", shouldAnnounce = false) {
                  val stats = DebugTime.logTime("optimization") {
                    optimize(loss)
                  }
                  calcGradInfo(stats)
                }
              }.toVector
              pb.stepBy(2)

              amm.write.over(resultsDir / "time_stats.txt", DebugTime.show)
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

      def testStep(epoch: Int, pb: ProgressBar, isTestSet: Boolean): ForwardResult = {
        val dataSetName = if (isTestSet) "test" else "dev"
        val dataSet = if (isTestSet) testSet else devSet
        announced(s"test on $dataSetName set") {
          import cats.implicits._

          val (fr, fse1Acc, libTop5Acc, projTop5Acc) =
            dataSet.zipWithIndex.flatMap {
              case (datum, i) =>
                checkShouldStop(epoch)
                pb.setExtraMessage("testing ")
                announced(s"test on $datum", shouldAnnounce) {
                  forward(
                    datum,
                    shouldDownsample = !isTestSet,
                    shouldDropout = false,
                    maxBatchSize = Some(maxBatchSize)
                  )(ModeEval).map {
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
                      pb.step()
                      (fwd, fse1, libTop5, projTop5).tap { _ =>
                        printResult(s"(progress: ${i + 1}/${dataSet.size})")
                      }
                  }.toVector
                }
            }.combineAll

          import fr.{libCorrect, projCorrect, confusionMatrix, categoricalAcc}
          import logger._
          logScalar(s"$dataSetName-loss", epoch, toAccuracyD(fr.loss))
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
          fr
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
                    )(ModeEval).map {
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
          write.over(saveDir / "currentTime.txt", dateTime.toString)
          write.over(saveDir / "currentEpoch.txt", epoch.toString)
          write.over(saveDir / "timeStats.txt", DebugTime.show)
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

    /** Randomly samples the number of prediction labels for each given project */
    private def dataSetLabelNums(
        projects: Seq[ProcessedProject],
        random: Random
    ): Seq[Int] = {
      projects.map { d =>
        random.synchronized {
          val (kept, dropped) = annotsSampling.randomSplit(d.nodesToPredict, random)
          ProjectLabelStats(d.nodesToPredict).downsampleLibAnnots(maxLibRatio, random).size
        }
      }
    }

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
  }

  case class SystemConfig(
      numOfThreads: Int,
      resultsDir: ammonite.ops.Path,
  ) {
    override def toString: String =
      s"SystemConfiguration: {numOfThreads: $numOfThreads, resultsDir: $resultsDir}"
  }

  /** A simple sampling strategy that first samples `p` uniformly from the range
    * `[minKeepProb, maxKeepProb]`, then randomly keeps each user annotation
    * with probability `p`.
    * */
  case class AnnotsSampling(minKeepProb: Real, maxKeepProb: Real) {
    require(minKeepProb >= 0)
    require(maxKeepProb <= 1)
    require(maxKeepProb >= minKeepProb)

    /**
      * Returns `(toKeep, toDrop)`. `toKeep` should be used to add more constraints on
      * the predicate graph, while `toDrop` should be used as prediction targets and not
      * directly visible to the GNN.
      *
      * @see [[lambdanet.translation.PredicateGraph.addUserAnnotations]].
      */
    def randomSplit[K,V](
        allAnnotations: Map[K, V],
        random: Random
    ): (Map[K, V], Map[K, V]) = {
      val pKeep = random.nextDouble() * (maxKeepProb - minKeepProb) + minKeepProb
      val nKeep = (pKeep * allAnnotations.size).toInt
      val (keep, drop) = random
        .shuffle(allAnnotations.toVector)
        .splitAt(nKeep)
      (keep.toMap, drop.toMap)
    }
  }
  object AnnotsSampling {
    import pickle._
    implicit val rw: ReadWriter[AnnotsSampling] = macroRW
  }

  /**
    * Records the training-time model configurations.
    * @param toyMode if true, will run the model on a toy dataset for fast debugging purpose.
    * @param useSeqModel if true, will use RNN architecture instead of GNN.
    * @param gnnIterations how many GNN iteration layers to use.
    * @param useDropout whether to use dropout in some layers.
    * @param predictAny whether to include the special type `any` in the prediction space.
    * @param maxLibRatio the maximal ratio of (library type labels)/(project type labels).
    *                    Ectra library labels will be randomly down-sampled.
    * @param projWeight the relative weight ratio between a project type label and a
    *                   library type label. Setting this > 1 during training will encourage
    *                   the model to predict project types more frequently.
    * @param gatHeads the number of attention heads in the GAT message aggregation kernel.
    * @param weightDecay i.e., the factor for L2 regularization.
    * @param onlyPredictLibType if true, the model will only predict library types.
    * @param lossAggMode how to compute the loss. See [[LossAggMode]].
    * @param encodeLibSignature whether the GNN take the type signature of library nodes as
    *                           additional inputs.
    * @param annotsSampling During training, this defines how user annotations will be
    *                       randomly sampled as inputs and labels.
    *
    */
  @SerialVersionUID(3L)
  case class ModelConfig(
      toyMode: Boolean = false,
      useSeqModel: Boolean = false,
      gnnIterations: Int = 6,
      useDropout: Boolean = true,
      predictAny: Boolean = true,
      maxLibRatio: Real = 9.0,
      projWeight: Real = 1.0,
      gatHeads: Int = 1,
      weightDecay: Option[Real] = Some(1e-4),
      onlyPredictLibType: Boolean = false,
      lossAggMode: LossAggMode.Value = LossAggMode.Sum,
      encodeLibSignature: Boolean = true,
      annotsSampling: AnnotsSampling = AnnotsSampling(0.0, 0.0)
  ) {
    def toJsonString: String = pickle.write(this, indent = 4)

    val taskName: String = {
      val flags = Seq(
        "toy" -> toyMode,
        "fix" -> NeuralInference.fixBetweenIteration,
        "decay" -> weightDecay.nonEmpty,
        "with_any" -> predictAny,
        "lossAgg_sum" -> (lossAggMode == LossAggMode.Sum),
        "encodeSignature" -> encodeLibSignature,
        "lib" -> onlyPredictLibType,
      ).map(flag(_)).mkString

      val ablationFlag = Seq(
        "noContextual" -> NeuralInference.noContextual,
        "noAttention" -> NeuralInference.noAttentional,
        "noLogical" -> NeuralInference.noLogical,
      ).map(flag(_, post = true)).mkString

      if (useSeqModel) "seqModel-theirName1-node"
      else
        s"${ablationFlag}UserAnnot-v1.3-GAT$gatHeads-fc${NNArchitecture.messageLayers}" +
          s"$annotsSampling-$flags-${gnnIterations}"
    }

    val dimMessage: Int = if (useSeqModel) 64 else 32

    def flag(nameValue: (String, Boolean), post: Boolean = false): String = {
      val (name, value) = nameValue
      if (value) (if (post) s"$name-" else s"-$name") else ""
    }

    def limitTimeOpt[A](
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
      }
    }

    @throws[TimeoutException]
    def limitTime[A](timeLimit: Timeouts.Duration)(f: => A): A = {
      val exec = scala.concurrent.ExecutionContext.global
      Await.result(Future(f)(exec), timeLimit)
    }
  }

  object ModelConfig {
    import pickle._
    implicit val rw: ReadWriter[ModelConfig] = macroRW
    implicit val rwEnum: ReadWriter[LossAggMode.Value] =
      pickle.readwriter[Int].bimap(_.id, LossAggMode.apply)
  }

  def checkMemoryConfigs(): Unit = {
    import ammonite.ops._
    val f = pwd / "configs" / "memory.txt"
    if (!exists(f))
      printWarning(
        s"$f not exits. Using default memory limits, which is likely " +
          s"to be too small. If you see timeouts during training, try to create this file " +
          s"with two integers in it, one on each line, which specify the heap and " +
          s"off-heap memory limit in Gigabytes."
      )
  }
}
