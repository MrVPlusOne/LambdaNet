package lambdanet

import java.util.concurrent.ForkJoinPool

import botkop.numsca
import cats.effect.IO
import cats.implicits._
import funcdiff._
import lambdanet.TrainingCenter.Timeouts
import lambdanet.translation.PredicateGraph.{PTyVar, PType}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object TrainingLoop {

  val maxTrainingSteps: Int = 1000
  val numOfThreads: Int = Runtime.getRuntime.availableProcessors()
  val forkJoinPool = new ForkJoinPool(numOfThreads)
  val taskSupport: ForkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)
  val parallelCtx: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(forkJoinPool)

  /** Remember to use these VM options to increase memory limits.
    * VM Options: -Xms2G -Xmx8G -Dorg.bytedeco.javacpp.maxbytes=18G -Dorg.bytedeco.javacpp.maxphysicalbytes=27G */
  def main(args: Array[String]): Unit = {
    trainingLoop.unsafeRunSync()
  }

  /** A parsed typescript project */
  type TsProject = PredicateGraphWithCtx

  case class TrainingState(
      step: Int,
      dimMessage: Int,
      iterationNum: Int,
      optimizer: Optimizer,
      pc: ParamCollection
  )

  case class DataSet(
      trainSet: Vector[TsProject],
      testSet: Vector[TsProject],
      libTypes: Set[PType]
  )

  def trainingLoop: IO[Unit] = {
    for {
      trainingState <- loadTrainingState()
      dataSet <- loadDataSet()
      _ <- trainOnProjects(dataSet, trainingState)
    } yield ()
  }

  def trainOnProjects(
      dataSet: DataSet,
      trainingState: TrainingState
  ): IO[Unit] = {
    import TensorExtension.oneHot

    import trainingState._
    import dataSet._

    val inference = new NeuralInference(pc, dimMessage, Some(taskSupport))

    def predictionLoss(
        logits: CompNode,
        predSpace: PredictionSpace,
        groundTruths: Vector[PType]
    ): CompNode = {
      val targets = groundTruths.map(predSpace.indexOfType)
      val loss = mean(
        crossEntropyOnSoftmax(logits, oneHot(targets, predSpace.maxIndex))
      )
      if (loss.value.squeeze() > 20) {
        warn(s"Abnormally large loss: ${loss.value}, logits: \n${logits.value}")
      }
      loss
    }

    def trainStep(step: Int): IO[Unit] = {
      def analyzeLogits(
          logits: CompNode,
          predSpace: PredictionSpace,
          groundTruths: Vector[PType]
      ): Double = {
        val predictions = numsca
          .argmax(logits.value, axis = 1)
          .data
          .map(_.toInt)
          .toVector
        val target = groundTruths.map(predSpace.indexOfType)
        val numCorrect = predictions.zip(target).count { case (x, y) => x == y }
        numCorrect.toDouble / target.length
      }

      trainSet.foldMap { graph =>
        IO[Unit] {
          val nodesToPredict = graph.userAnnotations.keysIterator.toVector
          val predSpace = PredictionSpace(
            libTypes ++ graph.projectNodes.collect {
              case n if n.isType => PTyVar(n)
            }
          )
          val logits = inference
            .predictTypes(graph, nodesToPredict, predSpace, iterationNum)
          val groundTruths = nodesToPredict.map(graph.userAnnotations(_).get)
          val accuracy = analyzeLogits(logits, predSpace, groundTruths)
          val loss = predictionLoss(
            logits,
            predSpace,
            groundTruths
          )
          optimizer.minimize(
            loss,
            pc.allParams, //todo: consider only including the involved
            backPropInParallel =
              Some(parallelCtx -> Timeouts.optimizationTimeout)
          )
        }
      }
    }

    def testStep(step: Int): IO[Unit] =
      if (step % 10 == 0) announce("test") {
        putStrLn("Test not implemented yet...")
      } else IO {}

    (trainingState.step + 1 to maxTrainingSteps).toVector
      .foldMap(step => trainStep(step) *> testStep(step))
  }

  private def loadDataSet(): IO[DataSet] = {
    ???
  }

  private def loadTrainingState(): IO[TrainingState] = ???

  def putStrLn(text: String): IO[Unit] =
    IO { println(text) }

  def announce[A](actionName: String)(action: IO[A]): IO[A] =
    for {
      _ <- putStrLn(s"$actionName started...")
      r <- action
      _ <- putStrLn(s"$actionName finished...")
    } yield r
}
