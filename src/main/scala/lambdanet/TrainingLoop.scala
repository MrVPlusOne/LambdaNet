package lambdanet

import java.util.concurrent.ForkJoinPool

import botkop.numsca
import cats.implicits._
import funcdiff._
import lambdanet.NewInference.Predictor
import lambdanet.TrainingCenter.Timeouts
import lambdanet.translation.PAnnot
import lambdanet.translation.PredicateGraph.{PNode, PTyVar, PType}

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
    val trainingState = loadTrainingState()
    val dataSet = loadDataSet()
    trainOnProjects(dataSet, trainingState)
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

  case class Datum(userAnnotations: Map[PNode, PAnnot], predictor: Predictor)

  case class DataSet(
      trainSet: Vector[Datum],
      testSet: Vector[Datum],
      libTypes: Set[PType]
  )

  case class trainOnProjects(
      dataSet: DataSet,
      trainingState: TrainingState
  ) {
    import TensorExtension.oneHot
    import trainingState._
    import dataSet._

    val layerFactory =
      new LayerFactory(SymbolPath.empty / Symbol("TrainingLoop"), pc)

    (trainingState.step + 1 to maxTrainingSteps)
      .foreach { step =>
        trainStep(step)
        testStep(step)
      }

    def trainStep(step: Int): Unit = {
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

      trainSet.foreach { datum =>
        import datum._

        val nodesToPredict = userAnnotations.keys.toVector
        val predSpace = PredictionSpace(
          libTypes ++ predictor.projectNodes.collect {
            case n if n.isType => PTyVar(n)
          }
        )

        val logits = predictor
          .run(layerFactory, dimMessage)
          .run(nodesToPredict, iterationNum)
          .result

        val groundTruths = nodesToPredict.map(userAnnotations(_).get)
        val accuracy = analyzeLogits(logits, predSpace, groundTruths)
        val loss = predictionLoss(
          logits,
          predSpace,
          groundTruths
        )
        optimizer.minimize(
          loss,
          pc.allParams, //todo: consider only including the involved
          backPropInParallel = Some(parallelCtx -> Timeouts.optimizationTimeout)
        )
      }
    }

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

    def testStep(step: Int): Unit =
      if (step % 10 == 0) announce("test") {
        putStrLn("Test not implemented yet...")
      }
  }

  private def loadDataSet(): DataSet = {
    ???
  }

  private def loadTrainingState(): TrainingState = ???

  def putStrLn(text: String): Unit =
    println(text)

  def announce[A](actionName: String)(action: => A): A = {
    putStrLn(s"$actionName started...")
    action.tap(_ => putStrLn(s"$actionName finished..."))
  }
}
