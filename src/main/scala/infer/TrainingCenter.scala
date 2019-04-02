package infer

import java.util.concurrent.ForkJoinPool

import botkop.numsca
import botkop.numsca.Tensor
import funcdiff.API._
import funcdiff.SimpleMath.BufferedTotalMap
import funcdiff.SimpleMath.Extensions._
import funcdiff._
import gtype.EventLogger.PlotConfig
import gtype._
import infer.GraphEmbedding._
import infer.IRTranslation.TranslationEnv

import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport
import ammonite.ops._
import infer.IR.{IRModule, IRTypeId}
import infer.PredicateGraph.{PredicateModule, TypeLabel}

import scala.concurrent.ExecutionContextExecutorService

object TrainingCenter {

  val numOfThreads: Int = Runtime.getRuntime.availableProcessors()
  val forkJoinPool = new ForkJoinPool(numOfThreads)
  val taskSupport: ForkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)
  val parallelCtx: ExecutionContextExecutorService =
    concurrent.ExecutionContext.fromExecutorService(forkJoinPool)

  TensorExtension.checkNaN = false // uncomment to train faster

  case class TrainingState(
    step: Int,
    dimMessage: Int,
    layerFactory: LayerFactory,
    optimizer: Optimizer
  ) {
    def saveToFile(file: Path): Unit = {
      val toSave =
        (step, dimMessage, layerFactory.paramCollection.toSerializable, optimizer)
      SimpleMath.saveObjectToFile(file.toIO)(toSave)
    }
  }

  object TrainingState {
    def fromFile(file: Path): TrainingState = {
      val (step, dimMessage, data, optimizer) = SimpleMath
        .readObjectFromFile[(Int, Int, ParamCollection.SerializableFormat, Optimizer)](
          file.toIO
        )
      val factory = LayerFactory(
        SymbolPath.empty / 'TypingNet,
        ParamCollection.fromSerializable(data)
      )
      TrainingState(step, dimMessage, factory, optimizer)
    }
  }

  def main(args: Array[String]): Unit = {

    val libraryTypes = JSExamples.libraryTypes

//    val trainRoot = pwd / RelPath("data/toy")
//    val testRoot = trainRoot

    val trainRoot = pwd / RelPath("data/train/algorithms-train")
    val trainParsed = infer.PredicateGraphConstruction
      .fromSourceFiles(
        trainRoot,
        libraryTypes = libraryTypes.map(TyVar)
      )

    val testRoot = pwd / RelPath("data/test/algorithms-test")
    val testParsed = infer.PredicateGraphConstruction
      .fromSourceFiles(
        testRoot,
        libraryTypes = libraryTypes.map(TyVar)
      )

    println(s"=== Training on $trainRoot ===")

    val loadFromFile
      : Option[Path] = None // Some(pwd / RelPath("running-result/saved/step20/trainingState.serialized"))
    val trainingState = loadFromFile
      .map { p =>
        println("Loading training from file: " + p)
        TrainingState.fromFile(p)
      }
      .getOrElse(
        TrainingState(
          step = 0,
          layerFactory = LayerFactory(SymbolPath.empty / 'TypingNet, ParamCollection()),
          dimMessage = 64,
          optimizer = Optimizers.Adam(learningRate = 4e-4)
        )
      )

    trainOnModules(
      trainParsed.predModules,
      testParsed.predModules,
      trainParsed.irModules,
      trainParsed.irEnv,
      trainingState
    )
  }

  //noinspection TypeAnnotation
  case class GraphNetBuilder(
    predModules: IS[PredicateModule],
    transEnv: TranslationEnv,
    libraryFields: Vector[Symbol],
    libraryTypes: Vector[Symbol],
    factory: LayerFactory,
    dimMessage: Int = 64
  ) {
    val typeLabels = predModules.flatMap(m => m.typeLabels)

    val predicates = predModules.flatMap(m => m.predicates) ++ PredicateGraphConstruction
      .encodeUnaryPredicates(transEnv.idTypeMap.values)
    val newTypes = predModules.flatMap(m => m.newTypes.keys).toSet

    def predicateCategoryNumbers: Map[Symbol, IRTypeId] = {
      predicates.groupBy(PredicateGraph.predicateCategory).mapValuesNow { _.length }
    }

    val decodingCtx = DecodingCtx(
      Vector(AnyType, TyVar(unknownTypeSymbol)) ++ libraryTypes.map(TyVar),
      newTypes.toVector
    )

    import factory._

    def randomVar(path: SymbolPath): CompNode =
      getVar(path)(numsca.randn(1, dimMessage) * 0.01)

    import GraphEmbedding._

    def encodeDecode(): (CompNode, IS[Embedding]) = {
      val libraryTypeMap: Map[Symbol, CompNode] = {
        libraryTypes.map { k =>
          k -> randomVar('TyVar / k)
        }
      }.toMap

      val fieldKnowledge =
        libraryFields.map { k =>
          k -> (randomVar('fieldKey / k), randomVar('fieldValue / k))
        }.toMap

      val unknownTypeVec = randomVar('TyVar / GraphEmbedding.unknownTypeSymbol)

      val extendedTypeMap = BufferedTotalMap(libraryTypeMap.get) { _ =>
        unknownTypeVec
      }

      val labelEncoding = BufferedTotalMap((_: Symbol) => None) { _ =>
        const(TensorExtension.randomUnitVec(dimMessage)) ~>
          linear('UnknownLabel, dimMessage) ~> relu //todo: see if this is useful
      }

      val embedCtx = EmbeddingCtx(
        transEnv.idTypeMap.toMap,
        extendedTypeMap,
        predicates,
        labelEncoding,
        fieldKnowledge
      )

      GraphEmbedding(embedCtx, factory, dimMessage, Some(taskSupport))
        .encodeAndDecode(
          iterations = 10,
          decodingCtx,
          typeLabels.map(_._1)
        )
    }

  }

  def trainOnModules(
    trainingModules: IS[PredicateModule],
    testingModules: IS[PredicateModule],
    trainingIRModules: IS[IRModule],
    transEnv: TranslationEnv,
    trainingState: TrainingState
  ): Unit = {

    val TrainingState(initStep, dimMessage, factory, optimizer) = trainingState

    /** any symbols that are not defined within the project */
    val libraryFields: Vector[Symbol] = {
      var allDefined, allUsed = Set[Symbol]()
      trainingIRModules.foreach(m => {
        val stats = m.moduleStats
        allDefined ++= stats.fieldsDefined
        allUsed ++= stats.fieldsUsed
      })
      (allUsed -- allDefined).toVector
    }
    println("libraryFields: " + libraryFields)

    val libraryTypes = JSExamples.libraryTypes.toVector

    val trainBuilder =
      GraphNetBuilder(
        trainingModules,
        transEnv,
        libraryFields,
        libraryTypes,
        factory,
        dimMessage
      )

    val testBuilder =
      GraphNetBuilder(
        testingModules,
        transEnv,
        libraryFields,
        libraryTypes,
        factory,
        dimMessage
      )

    val typeLabels = trainBuilder.typeLabels
    val decodingCtx = trainBuilder.decodingCtx
    println("max Idx: " + decodingCtx.maxIndex)

    println("Predicate numbers:")
    println {
      trainBuilder.predicates.groupBy(PredicateGraph.predicateCategory).mapValuesNow {
        _.length
      }
    }

    val eventLogger = {
      import ammonite.ops._
      new EventLogger(
        pwd / "running-result" / "log.txt",
        printToConsole = true,
        overrideMode = true,
        configs = Seq(
          //          "embedding-magnitudes" -> PlotConfig("ImageSize->Medium"),
          "embedding-changes" -> PlotConfig("ImageSize->Medium"),
          "embedding-max-length" -> PlotConfig("ImageSize->Medium"),
          "iteration-time" -> PlotConfig(
            "ImageSize->Medium",
            """AxesLabel->{"step","ms"}"""
          ),
          "accuracy" -> PlotConfig("ImageSize->Medium"),
          "test-accuracy" -> PlotConfig("ImageSize->Medium"),
          "loss" -> PlotConfig("ImageSize->Large")
        )
      )
    }

    import TensorExtension.oneHot

    // training loop
    for (step <- initStep until initStep + 1000) try {
      val startTime = System.currentTimeMillis()

      val (logits, embeddings) = {
        note("encodeDecode")
        trainBuilder.encodeDecode()
      }

      DebugTime.logTime('loggingTime) {
        note("loggingTime")
        val diffs = embeddings
          .zip(embeddings.tail)
          .par
          .map {
            case (e1, e0) =>
              val diffMap = e1.nodeMap.elementwiseCombine(e0.nodeMap) { (x, y) =>
                math.sqrt(numsca.sum(numsca.square(x.value - y.value)))
              }
              SimpleMath.mean(diffMap.values.toSeq)
          }
          .seq
        eventLogger.log("embedding-changes", step, Tensor(diffs: _*))

        val maxEmbeddingLength = embeddings.map { _.stat.trueEmbeddingLengths.max }.max
        eventLogger.log("embedding-max-length", step, Tensor(maxEmbeddingLength))
      }

      note("analyzeResults")
      val accuracy = analyzeResults(
        typeLabels,
        logits.value,
        transEnv,
        decodingCtx,
        printResults = false
      )
      eventLogger.log("accuracy", step, Tensor(accuracy))

      println("annotated places number: " + typeLabels.length)
      val targets = typeLabels.map(p => decodingCtx.indexOfType(p._2))
      val loss = mean(
        crossEntropyOnSoftmax(logits, oneHot(targets, decodingCtx.maxIndex))
      )

      if (loss.value.squeeze() > 20) {
        println(s"Abnormally large loss: ${loss.value}")
        println("logits: ")
        println { logits.value }
      }
      eventLogger.log("loss", step, loss.value)

      note("optimization")
      DebugTime.logTime('optimization) {
        optimizer.minimize(
          loss,
          trainBuilder.factory.paramCollection.allParams,
          backPropInParallel = Some(parallelCtx)
        )
      }

      println(DebugTime.show)

      eventLogger.log(
        "iteration-time",
        step,
        Tensor(System.currentTimeMillis() - startTime)
      )

      if (step % 10 == 0) {
        println("start testing...")
        SimpleMath.measureTimeAsSeconds {
          val (testLogits, _) = testBuilder.encodeDecode()
          val testAcc = analyzeResults(
            testBuilder.typeLabels,
            testLogits.value,
            transEnv,
            testBuilder.decodingCtx,
            printResults = true
          )
          eventLogger.log("test-accuracy", step, Tensor(testAcc))
        }
      }
      if (step % 20 == 0) {
        saveTraining(step+1, s"step$step")
      }
    } catch {
      case ex: Throwable =>
        saveTraining(step, "error-save")
        throw ex
    }

    def saveTraining(step: Int, dirName: String): Unit = {
      println("save training...")
      val saveDir = pwd / "running-result" / "saved" / dirName
      if (!exists(saveDir)) {
        mkdir(saveDir)
      }
      val savePath = saveDir / "trainingState.serialized"
      TrainingState(
        step,
        dimMessage,
        factory,
        optimizer).saveToFile(savePath)
      println("Training state saved into: " + saveDir)
    }
  }

  def analyzeResults(
    annotatedPlaces: IS[(IRTypeId, TypeLabel)],
    logits: Tensor,
    transEnv: TranslationEnv,
    ctx: DecodingCtx,
    printResults: Boolean = true
  ): Double = {
    type Prediction = Int
    val predictions = numsca.argmax(logits, axis = 1)
    val correct = mutable.ListBuffer[(IRTypeId, Prediction)]()
    val incorrect = mutable.ListBuffer[(IRTypeId, Prediction)]()
    for (row <- annotatedPlaces.indices) {
      val (nodeId, t) = annotatedPlaces(row)
      val expected = ctx.indexOfType(t)
      val actual = predictions(row, 0).squeeze().toInt
      if (expected == actual)
        correct += (nodeId -> actual)
      else {
        incorrect += (nodeId -> actual)
      }
    }

    correct.foreach {
      case (id, pred) =>
        val t = ctx.typeFromIndex(pred)
        val tv = transEnv.idTypeMap(id)
        if (printResults)
          println(s"[correct] \t$tv: $t")
    }

//    val holeTypeMap = annotatedPlaces.toMap
    val labelMap = annotatedPlaces.toMap
    incorrect.foreach {
      case (id, pred) =>
        val tv = transEnv.idTypeMap(id)
        val actualType = ctx.typeFromIndex(pred)
        val expected = labelMap(id)
        if (printResults)
          println(s"[incorrect] \t$tv: $actualType not match $expected")
    }

    val accuracy = correct.length.toDouble / (correct.length + incorrect.length)
    accuracy
  }

  def note(msg: String): Unit = {
    val printNote = false
    if (printNote) println("note: " + msg)
  }
}
