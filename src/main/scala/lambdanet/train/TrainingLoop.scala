package lambdanet.train

import lambdanet._
import java.util.concurrent.ForkJoinPool

import ammonite.ops.Path
import botkop.numsca
import funcdiff.{
  CompNode,
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
import lambdanet.translation.PredicateGraph.{PNode, PTyVar, PType}
import lambdanet.{PredicateGraphWithCtx, PredictionSpace, printWarning}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.util.Random
import Console.{GREEN, RESET}

object TrainingLoop {

  /** Remember to use these VM options to increase memory limits.
    * VM Options: -Xms2G -Xmx8G -Dorg.bytedeco.javacpp.maxbytes=18G -Dorg.bytedeco.javacpp.maxphysicalbytes=27G */
  def main(args: Array[String]): Unit = {

    run(
      maxTrainingSteps = 1000,
      numOfThreads = Runtime.getRuntime.availableProcessors(),
    ).result()
  }

  case class run(
      maxTrainingSteps: Int,
      numOfThreads: Int,
  ) {
    def result(): Unit = {
      val trainingState = loadTrainingState()
      val dataSet = loadDataSet()
      trainOnProjects(dataSet, trainingState)
    }

    case class trainOnProjects(
        dataSet: DataSet,
        trainingState: TrainingState,
    ) {
      import TensorExtension.oneHot
      import dataSet._
      import trainingState._

      val layerFactory =
        new LayerFactory(SymbolPath.empty / Symbol("TrainingLoop"), pc)

      (trainingState.step + 1 to maxTrainingSteps)
        .foreach { step =>
          trainStep(step)
          testStep(step)
        }

      def trainStep(step: Int): Unit = announced(s"trainStep $step") {
        def analyzeLogits(
            logits: CompNode,
            predSpace: PredictionSpace,
            groundTruths: Vector[PType],
        ): Double = {
          val predictions = numsca
            .argmax(logits.value, axis = 1)
            .data
            .map(_.toInt)
            .toVector
          val target = groundTruths.map(predSpace.indexOfType)
          val numCorrect =
            predictions.zip(target).count { case (x, y) => x == y }
          numCorrect.toDouble / target.length
        }

        trainSet.foreach { datum =>
          import datum._

          val nodesToPredict = userAnnotations.keys.toVector
          val predSpace = PredictionSpace(
            libTypes ++ predictor.projectNodes.collect {
              case n if n.isType => PTyVar(n)
            },
          )

          val logits = announced("run predictor") {
            predictor
              .run(dimMessage, layerFactory, nodesToPredict, iterationNum)
              .result
          }

          val groundTruths = nodesToPredict.map(userAnnotations)
          val accuracy = announced("compute training accuracy") {
            analyzeLogits(logits, predSpace, groundTruths)
          }

          println(s"${GREEN}accuracy for $projectName: $accuracy$RESET")

          val loss = predictionLoss(logits, predSpace, groundTruths)

          announced("optimization") {
            optimizer.minimize(
              loss,
              pc.allParams, //todo: consider only including the involved
              backPropInParallel =
                Some(parallelCtx -> Timeouts.optimizationTimeout),
            )
          }
        }
      }

      def predictionLoss(
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

      def testStep(step: Int): Unit =
        if (step % 10 == 0) announced("test") {
          println("Test not implemented yet...")
        }
    }

    val forkJoinPool = new ForkJoinPool(numOfThreads)
    val taskSupport: ForkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)
    val parallelCtx: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(forkJoinPool)

    private def loadDataSet(): DataSet = announced("loadDataSet") {
      import PrepareRepos._

      val ParsedRepos(libDefs, projects) =
        announced("parsePredGraphs")(parsePredGraphs())
//        announced(s"read data set from file: $dataSetPath") {
//          SimpleMath.readObjectFromFile[ParsedRepos](dataSetPath.toIO)
//        }

      def libNodeType(n: PNode) =
        libDefs
          .nodeMapping(n)
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
      val trainSetNum = (totalNum * 0.8).toInt
      DataSet(data.take(trainSetNum), data.drop(trainSetNum), libraryTypes)
    }

  }

  /** A parsed typescript project */
  type TsProject = PredicateGraphWithCtx

  case class TrainingState(
      step: Int,
      dimMessage: Int,
      iterationNum: Int,
      optimizer: Optimizer,
      pc: ParamCollection,
  ) {
    def saveToFile(file: Path): Unit = {
      val toSave =
        List[(String, Any)](
          "step" -> step,
          "dimMessage" -> dimMessage,
          "iterationNum" -> iterationNum,
          "optimizer" -> optimizer,
          "pcData" -> pc.toSerializable,
        )
      SimpleMath.saveObjectToFile(file.toIO)(toSave)
    }

    override def toString: String = {
      s"""TrainingState:
         |  step: $step
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
      userAnnotations: Map[PNode, PType],
      predictor: Predictor,
  )

  case class DataSet(
      trainSet: Vector[Datum],
      testSet: Vector[Datum],
      libTypes: Set[PType],
  )

  val defaultIterationNum = 10

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
            step = 0,
            dimMessage = 64,
            optimizer = Optimizers.Adam(learningRate = 4e-4),
            iterationNum = defaultIterationNum,
            pc = ParamCollection(),
          ),
        )
    }

}
