package lambdanet.train

import lambdanet._
import java.util.concurrent.{ForkJoinPool}

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
import scala.concurrent.{
  Await,
  ExecutionContext,
  ExecutionContextExecutorService,
  Future,
  TimeoutException,
}
import scala.language.reflectiveCalls
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
              epoch0 = 0,
              dimMessage = 32,
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
        (trainingState.epoch0 + 1 to maxTrainingEpochs)
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
            checkShouldStop(epoch)
            for {
              (loss, fwd) <- forward(datum).tap(printResult)
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

              limitTimeOpt(
                s"optimization: $datum",
                Timeouts.optimizationTimeout,
              ) {
                announced("optimization") {
                  DebugTime.logTime("optimization") {
                    optimize(loss)
                  }
                }
              }
              fwd
            }
          }
        }

        import cats.implicits._
        val ForwardResult(loss, libAcc, projAcc, distanceAcc) =
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

      def toAccuracy(counts: Counted[Int]): Double = {
        def nonZero(n: Int): Double = if (n == 0) 1.0 else n.toDouble
        counts.value.toDouble / nonZero(counts.count)
      }

      def toAccuracyD(counts: Counted[Double]): Double = {
        def nonZero(n: Double): Double = if (n == 0) 1.0 else n.toDouble
        counts.value / nonZero(counts.count)
      }

      case class Counted[V](count: Int, value: V)

      object Counted {
        def zero[V](v: V) = Counted(0, v)
      }

      type Logits = CompNode
      type Loss = CompNode
      type Correct = Int
      type LibCorrect = Correct
      type ProjCorrect = Correct

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

      implicit def weightedMonoid[V](
          implicit m: Monoid[V],
      ): Monoid[Counted[V]] = new Monoid[Counted[V]] {
        def empty: Counted[V] = Counted(0, m.empty)

        def combine(x: Counted[V], y: Counted[V]): Counted[V] = {
          Counted(x.count + y.count, m.combine(x.value, y.value))
        }
      }

      implicit object LossMonoid extends Monoid[Loss] {
        def empty: Loss = 0.0

        def combine(x: Loss, y: Loss): Loss = x + y
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
            printWarning(s"$name exceeded time limit $timeLimit.")
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

        /** sort lib types by their usages */
        val sortedTypes = libDefs.nodeMapping.keys.toVector
          .collect {
            case n if n.isType =>
              (LibTypeNode(LibNode(n)), usages.getOrElse(n, 0))
          }
          .sortBy(-_._2)

        val totalUsages = sortedTypes.map(_._2).sum
        val coverageGoal = 0.85
        val (libTypes, achieved) =
          sortedTypes
            .zip(sortedTypes.scanLeft(0.0)(_ + _._2.toDouble / totalUsages))
            .takeWhile(_._2 < coverageGoal)
            .unzip

        printResult(s"Coverages achieved: ${achieved.last}")
        printResult(s"Lib types selected (${libTypes.length}): $libTypes")

        libTypes.map(_._1).toSet
      }

      val ALL = 100
      val projectsToUse = ALL

      val data = projects
        .pipe(x => new Random(1).shuffle(x))
        .take(projectsToUse)
        .map {
          case (path, g, annotations) =>
            val predictor =
              Predictor(g, libTypesToPredict, libNodeType, Some(taskSupport))
            Datum(path, annotations.toMap, predictor)
              .tap(d => printResult(d.showDetail))
        }

      val libAnnots = data.map(_.libAnnots).sum
      val projAnnots = data.map(_.projAnnots).sum
      printResult(s"$libAnnots library targets, $projAnnots project targets.")

      val totalNum = data.length
      val trainSetNum = totalNum - (totalNum * 0.2).toInt
      DataSet(data.take(trainSetNum), data.drop(trainSetNum))
        .tap(printResult)
    }
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

  case class Datum(
      projectName: ProjectPath,
      annotations: Map[ProjNode, PType],
      predictor: Predictor,
  ) {
    val inPSpaceRatio: Double =
      annotations
        .count(
          _._2.pipe(predictor.predictionSpace.allTypes.contains),
        )
        .toDouble / annotations.size

    val distanceToConsts: PNode => Int = {
      Analysis.analyzeGraph(predictor.graph).distanceToConstNode
    }

    def libAnnots: Int = annotations.count(_._2.madeFromLibTypes)
    def projAnnots: Int = annotations.count(!_._2.madeFromLibTypes)

    def showInline: String = {
      s"{name: $projectName, " +
        s"annotations: ${annotations.size}(L:$libAnnots/P:$projAnnots), " +
        s"predicates: ${predictor.graph.predicates.size}, " +
        s"predictionSpace: ${predictor.predictionSpace.size}, " +
        s"inPSpaceRatio: $inPSpaceRatio}"
    }

    override def toString: String = {
      showInline
    }

    def showDetail: String = {
      s"""$showInline
         |${predictor.predictionSpace}
         |""".stripMargin
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
    )
  }

}
