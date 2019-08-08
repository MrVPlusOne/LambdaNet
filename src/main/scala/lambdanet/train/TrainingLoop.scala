package lambdanet.train

import java.util.Calendar

import lambdanet.{printResult, _}
import java.util.concurrent.ForkJoinPool

import botkop.numsca
import cats.Monoid
import funcdiff.{SimpleMath => SM}
import funcdiff._
import lambdanet.architecture._
import lambdanet.utils.{EventLogger, QLangDisplay, ReportFinish}
import TrainingState._
import botkop.numsca.Tensor
import lambdanet.architecture.LabelEncoder.{ConstantLabelEncoder, SegmentedLabelEncoder, TrainableLabelEncoder}
import lambdanet.architecture.Embedding
import lambdanet.translation.PredicateGraph.{PNode, PType, ProjNode}
import lambdanet.translation.QLang.QModule
import org.nd4j.linalg.api.buffer.DataType

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future, TimeoutException}
import scala.language.reflectiveCalls

object TrainingLoop extends TrainingLoopTrait {
  val toyMod: Boolean = false
  val taskName = s"combined-init-${TrainingState.iterationNum}"

  import fileLogger.{println, printInfo, printWarning, printResult, announced}

  def scaleLearningRate(epoch: Int): Double = {
    val min = 0.3
    val epochToSlowDown = if (toyMod) 300 else 150
    SimpleMath
      .linearInterpolate(1.0, min)(epoch.toDouble / epochToSlowDown)
      .max(min)
  }

  def main(args: Array[String]): Unit = {
    Tensor.floatingDataType = DataType.DOUBLE
    run(
      maxTrainingEpochs = 5000,
      numOfThreads = readThreadNumber(),
    ).result()
  }

  case class run(
      maxTrainingEpochs: Int,
      numOfThreads: Int,
  ) {

//    Nd4j.setNumThreads(numOfThreads)
    printInfo(s"Task: $taskName")
    printInfo(s"maxEpochs = $maxTrainingEpochs, threads: $numOfThreads")
    Timeouts.readFromFile()

    def result(): Unit = {
      val (state, logger) = loadTrainingState(resultsDir, fileLogger)
      val architecture = GruArchitecture(state.dimMessage, state.pc)
      val seqArchitecture =
        SequenceModel.SeqArchitecture(state.dimMessage, state.pc)
      val dataSet = DataSet.loadDataSet(taskSupport, architecture)
      trainOnProjects(dataSet, state, logger, architecture, seqArchitecture)
        .result()
    }

    //noinspection TypeAnnotation
    case class trainOnProjects(
        dataSet: DataSet,
        trainingState: TrainingState,
        logger: EventLogger,
        architecture: NNArchitecture,
        seqArchitecture: SequenceModel.SeqArchitecture,
    ) {
      import dataSet._
      import trainingState._

      val labelCoverage =
        //        SegmentedLabelEncoder(repos, coverageGoal = 0.90, architecture)
        TrainableLabelEncoder(trainSet, coverageGoal = 0.90, architecture)

      val labelEncoder =
        SegmentedLabelEncoder(trainSet, coverageGoal = 0.90, architecture)

      //      val randomLabelEncoder = RandomLabelEncoder(architecture)
      val nameEncoder = {
        SegmentedLabelEncoder(trainSet, coverageGoal = 0.90, architecture)
//        ConstantLabelEncoder(architecture)
      }

      printResult(s"Label encoder: ${labelEncoder.name}")
      printResult(s"Name encoder: ${nameEncoder.name}")

      printResult(s"NN Architecture: ${architecture.arcName}")
      printResult(s"Single layer consists of: ${architecture.singleLayerModel}")

      def result(): Unit = {
        val saveInterval = if (toyMod) 100 else 10

        (trainingState.epoch0 + 1 to maxTrainingEpochs).foreach { epoch =>
          announced(s"epoch $epoch") {
            handleExceptions(epoch) {
              DebugTime.logTime("test-step") {
                testStep(epoch)
              }
              trainStep(epoch)
              if (epoch % saveInterval == 0) {
                saveTraining(epoch, s"epoch$epoch")
              }
            }
          }
        }

        saveTraining(maxTrainingEpochs, "finished")
        emailService.sendMail(emailService.userEmail)(
          s"TypingNet: Training finished on $machineName!",
          "Training finished!",
        )
      }

      val (machineName, emailService) = ReportFinish.readEmailInfo(taskName)
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
              printWarning(
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

      val random = new util.Random(2)

      def trainStep(epoch: Int): Unit = {
        DebugTime.logTime("GC") {
          System.gc()
        }
        val startTime = System.nanoTime()
        val stats = random.shuffle(trainSet).zipWithIndex.map {
          case (datum, i) =>
            import Console.{GREEN, BLUE}
            announced(
              s"$GREEN[epoch $epoch](progress: ${i + 1}/${trainSet.size})$BLUE train on $datum",
            ) {
//              println(DebugTime.show)
              checkShouldStop(epoch)
              architecture.dropoutStorage = Some(new ParamCollection())
              for {
                (loss, fwd, _) <- selectForward(datum).tap(
                  _.foreach(r => printResult(r._2)),
                )
                _ = checkShouldStop(epoch)
              } yield {
                checkShouldStop(epoch)
                def optimize(loss: CompNode) = {
                  val factor = fwd.loss.count.toDouble / avgAnnotations
                  optimizer.minimize(
                    loss * factor,
                    pc.allParams,
                    backPropInParallel =
                      Some(parallelCtx -> Timeouts.optimizationTimeout),
                    gradientTransform = _.clipNorm(2 * factor),
                    scaleLearningRate = scaleLearningRate(epoch),
                  )
                }

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

                (fwd, gradInfo, datum)
              }
            }
        }

        import cats.implicits._
        val (fws, gs, data) = stats.flatMap(_.toVector).unzip3

        fws.combineAll.tap {
          case ForwardResult(
              loss,
              libAcc,
              projAcc,
              confMat,
              categoricalAcc
              ) =>
            logger.logScalar("loss", epoch, toAccuracyD(loss))
            logger.logScalar("libAcc", epoch, toAccuracy(libAcc))
            logger.logScalar("projAcc", epoch, toAccuracy(projAcc))
            logger.logConfusionMatrix("confusionMat", epoch, confMat.value, 2)
            logAccuracyDetails(data zip fws, epoch)

            logger.logString("typeAcc", epoch, typeAccString(categoricalAcc))
        }

        val gradInfo = gs.combineAll
        gradInfo.unzip3.tap {
          case (grads, transformed, deltas) =>
            logger.logScalar("gradient", epoch, grads.sum)
            logger.logScalar("clippedGradient", epoch, transformed.sum)
            logger.logScalar("paramDelta", epoch, deltas.sum)
        }

        val timeInSec = (System.nanoTime() - startTime).toDouble / 1e9
        logger.logScalar("iter-time", epoch, timeInSec)

        println(DebugTime.show)
      }

      private def typeAccString(accs: Map[PType, Counted[Correct]]): String ={
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
        if ((epoch - 1) % 5 == 0) announced("test on dev set") {
          import cats.implicits._
          architecture.dropoutStorage = None

          val (stat, fse1Acc, fse5Acc) = testSet.flatMap { datum =>
            checkShouldStop(epoch)
            announced(s"test on $datum") {
              selectForward(datum).map {
                case (_, fwd, pred) =>
                  val pred1 = pred.mapValuesNow { _.head }
                  printQSource(
                    datum.qModules,
                    pred1,
                    datum.predictor.predictionSpace,
                    resultsDir / "predictions" / datum.projectName
                  )

                  val (fse1, _) = datum.fseAcc.countTopNCorrect(1, pred)
                  val (fse5, _) = datum.fseAcc.countTopNCorrect(5, pred)

                  (fwd, fse1, fse5)
              }.toVector
            }
          }.combineAll

          import stat.{libCorrect, projCorrect, confusionMatrix, categoricalAcc}
          logger.logScalar("test-libAcc", epoch, toAccuracy(libCorrect))
          logger.logScalar("test-projAcc", epoch, toAccuracy(projCorrect))
          logger.logConfusionMatrix(
            "test-confusionMat",
            epoch,
            confusionMatrix.value,
            2
          )
          logger.logScalar("test-fse-top1", epoch, toAccuracy(fse1Acc))
          logger.logScalar("test-fse-top5", epoch, toAccuracy(fse5Acc))
          logger.logString("test-typeAcc", epoch, typeAccString(categoricalAcc))
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

      val lossModel: LossModel = LossModel.EchoLoss
        .tap(m => printResult(s"loss model: ${m.name}"))

      private def selectForward(data: Datum) = {
        val useSeqModel = false
        if (useSeqModel) seqForward(data)
        else forward(data)
      }

      /** Forward propagation for the sequential model */
      private def seqForward(
          datum: Datum,
      ): Option[(Loss, ForwardResult, Map[PNode, Vector[PType]])] = {
        def result = {
          val predictor = datum.seqPredictor
          val predSpace = predictor.predSpace
          // the logits for very iterations
          val nodes = datum.nodesToPredict.map { _.n }
          val logits = announced("run predictor") {
            predictor.run(seqArchitecture, nameEncoder, nodes)
          }
//          val diff = nodes.toSet.diff(datum.annotations.keySet.map(_.n))
//          assert(diff.isEmpty, s"diff is not empty: $diff")

          val nonGenerifyIt = DataSet.nonGenerify(predictor.libDefs)

          val groundTruths = nodes.map {
            case n if n.fromLib =>
              nonGenerifyIt(predictor.libDefs.nodeMapping(n).get)
            case n if n.fromProject =>
              datum.annotations(ProjNode(n))
          }

          val targets = groundTruths.map(predSpace.indexOfType)
          val nodeDistances = nodes.map(_.pipe(datum.distanceToConsts))

          val (libCounts, projCounts, confMat, typeAccs) =
            announced("compute training accuracy") {
              analyzeLogits(
                logits,
                groundTruths,
                predSpace,
                nodeDistances,
              )
            }

          val loss =
            lossModel.crossEntropyWithLogitsLoss(
              logits,
              targets,
              predSpace.size,
            )

          val totalCount = libCounts.count + projCounts.count
          val fwd = ForwardResult(
            Counted(totalCount, loss.value.squeeze() * totalCount),
            libCounts,
            projCounts,
            confMat,
            typeAccs,
          )

          val predictions: Map[PNode, Vector[PType]] = {
            import numsca._
            val Shape(Vector(row, _)) = logits.value.shape
            val predVec = (0 until row.toInt).map { r =>
              logits
                .value(r, :>)
                .data
                .zipWithIndex
                .sortBy(_._1)
                .map { case (_, i) => predSpace.typeVector(i) }
                .reverse
                .toVector
            }

            nodes.zip(predVec).toMap
          }

          (loss, fwd, predictions)
        }

        limitTimeOpt(s"forward: $datum", Timeouts.forwardTimeout) {
          DebugTime.logTime("seqForward") { result }
        }
      }
      private def forward(
          datum: Datum,
      ): Option[(Loss, ForwardResult, Map[PNode, Vector[PType]])] =
        limitTimeOpt(s"forward: $datum", Timeouts.forwardTimeout) {
          import datum._

          val predSpace = predictor.predictionSpace
//          val seqLogits = announced("run seq predictor") {
//            seqPredictor.run(
//              seqArchitecture,
//              nameEncoder,
//              nodesToPredict.map(_.n)
//            )
//          }
          val seqEmbedding = announced("run seq encoder"){
            seqPredictor.encode(seqArchitecture, nameEncoder)
          }

          // the probability for very iterations
          val probsVec = announced("run predictor") {
            predictor
              .run(
                architecture,
                nodesToPredict,
                nodeSet => Embedding(nodeSet.map{n => n -> seqEmbedding(n.n)}.toMap),
                iterationNum,
                nodeForAny,
                labelEncoder,
                labelCoverage.isLibLabel,
                nameEncoder
              )
              .result
//              .pipe { logitsVec =>
//                logitsVec.map { _ + seqLogits }
//              }
          }
          val probs = probsVec.last

          val groundTruths = nodesToPredict.map(annotations)
          val targets = groundTruths.map(predSpace.indexOfType)
          val nodeDistances = nodesToPredict.map(_.n.pipe(distanceToConsts))

          val (libCounts, projCounts, confMat, typeAccs) =
            announced("compute training accuracy") {
              analyzeLogits(
                probs,
                groundTruths,
                predSpace,
                nodeDistances,
              )
            }

          val loss =
            lossModel.predictionLoss(
              predictor.parallelize(probsVec),
              targets,
              predSpace.size,
            )

          val totalCount = libCounts.count + projCounts.count
          val fwd = ForwardResult(
            Counted(totalCount, loss.value.squeeze() * totalCount),
            libCounts,
            projCounts,
            confMat,
            typeAccs,
          )

          val predictions: Map[PNode, Vector[PType]] = {
            import numsca._
            val Shape(Vector(row, _)) = probs.value.shape
            val predVec = (0 until row.toInt).map { r =>
              probs
                .value(r, :>)
                .data
                .zipWithIndex
                .sortBy(_._1)
                .map { case (_, i) => predSpace.typeVector(i) }
                .reverse
                .toVector
            }

            nodesToPredict.map(_.n).zip(predVec).toMap
          }

          (loss, fwd, predictions)
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

      import ammonite.ops._

      private def printQSource(
          qModules: Vector[QModule],
          predictions: Map[PNode, PType],
          predictionSpace: PredictionSpace,
          predictionDir: Path
      ) = DebugTime.logTime("printQSource") {
        rm(predictionDir)
        qModules.par.foreach { m =>
          QLangDisplay.renderModuleToDirectory(
            m,
            predictions,
            predictionSpace.allTypes
          )(predictionDir)
        }
      }

      private def saveTraining(epoch: Int, dirName: String): Unit = {
        architecture.dropoutStorage = None

        announced(s"save training to $dirName") {
          val saveDir = resultsDir / "saved" / dirName
          if (!exists(saveDir)) {
            mkdir(saveDir)
          }
          val savePath = saveDir / "trainingState.serialized"
          TrainingState(epoch, dimMessage, iterationNum, optimizer, pc)
            .saveToFile(savePath)
          val currentLogFile = resultsDir / "log.txt"
          if (exists(currentLogFile)) {
            cp.over(currentLogFile, saveDir / "log.txt")
          }

          testSet.foreach { datum =>
            checkShouldStop(epoch)
            announced(s"test on $datum") {
              selectForward(datum).map {
                case (_, fwd, pred) =>
                  printQSource(
                    datum.qModules,
                    pred.mapValuesNow { _.head },
                    datum.predictor.predictionSpace,
                    saveDir / "predictions" / datum.projectName
                  )
              }.toVector
            }
          }

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

      private def analyzeLogits(
          logits: CompNode,
          groundTruths: Vector[PType],
          predictionSpace: PredictionSpace,
          nodeDistances: Vector[Int],
      ): (
          Counted[LibCorrect],
          Counted[ProjCorrect],
          Counted[ConfusionMatrix],
          Map[PType, Counted[Correct]],
      ) = {
        val predictions = numsca
          .argmaxAxis(logits.value, axis = 1)
          .data
          .map(_.toInt)
          .toVector
        val targets = groundTruths.map(predictionSpace.indexOfType)
        val truthValues = predictions.zip(targets).map { case (x, y) => x == y }
        val targetFromLibrary = groundTruths.map { _.madeFromLibTypes }
        val zipped = targetFromLibrary.zip(truthValues)
        val libCorrect = zipped.collect {
          case (true, true) => ()
        }.length
        val projCorrect = zipped.collect {
          case (false, true) => ()
        }.length
        val libCounts = Counted(targetFromLibrary.count(identity), libCorrect)
        val projCounts = Counted(targetFromLibrary.count(!_), projCorrect)

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
          groundTruths.zip(truthValues).groupBy(_._1).mapValuesNow { bools =>
            Counted(bools.length, bools.count(_._2))
          }

        (libCounts, projCounts, confMat, typeAccs)
      }

      private val avgAnnotations =
        SM.mean(trainSet.map(_.annotations.size.toDouble))
    }

    val taskSupport: Option[ForkJoinTaskSupport] =
      if (numOfThreads == 1) None
      else Some(new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads)))
    val parallelCtx: ExecutionContextExecutorService = {
      import ExecutionContext.fromExecutorService
      fromExecutorService(new ForkJoinPool(numOfThreads))
    }
  }

  private case class ForwardResult(
      loss: Counted[Double],
      libCorrect: Counted[LibCorrect],
      projCorrect: Counted[ProjCorrect],
      confusionMatrix: Counted[ConfusionMatrix],
      categoricalAcc: Map[PType, Counted[Correct]]
  ) {
    override def toString: String = {
      s"forward result: {loss: ${toAccuracyD(loss)}, " +
        s"lib acc: ${toAccuracy(libCorrect)} (${libCorrect.count} nodes), " +
        s"proj acc: ${toAccuracy(projCorrect)} (${projCorrect.count} nodes)}"
    }
  }

  private implicit val forwardResultMonoid: Monoid[ForwardResult] =
    new Monoid[ForwardResult] {
      import Counted.zero
      import cats.implicits._

      def empty: ForwardResult =
        ForwardResult(zero(0), zero(0), zero(0), zero(Map()), Map())

      def combine(x: ForwardResult, y: ForwardResult): ForwardResult = {
        val z = ForwardResult.unapply(x).get |+| ForwardResult
          .unapply(y)
          .get
        (ForwardResult.apply _).tupled(z)
      }
    }

}
