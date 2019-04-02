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
  val taskSupport: ForkJoinTaskSupport = new ForkJoinTaskSupport(
    new ForkJoinPool(1)
  )
  val parallelCtx: ExecutionContextExecutorService =
    concurrent.ExecutionContext.fromExecutorService(new ForkJoinPool(numOfThreads))

  val printCorrectWrongs = true
  val debugFreeze = true

  def note(msg: String): Unit ={
    if(debugFreeze) println("note: " + msg)
  }

  def main(args: Array[String]): Unit = {
    TensorExtension.checkNaN = false // uncomment to train faster

    val libraryTypes = JSExamples.libraryTypes

    val projectRoot = pwd / RelPath("data/toy")
//    val projectRoot = pwd / RelPath("data/ts-algorithms")
    val parsed = infer.PredicateGraphConstruction
      .fromSourceFiles(
        projectRoot,
        libraryTypes = libraryTypes.map(TyVar)
      )

    println(s"=== Training on $projectRoot ===")
    parsed.irModules.foreach(m => m.stmts.foreach(s => println(s.prettyPrint())))

    trainOnModules(parsed.predModules, parsed.predModules, parsed.irModules, parsed.irEnv)
  }

  //noinspection TypeAnnotation
  case class GraphNetBuilder(
    predModules: IS[PredicateModule],
    transEnv: TranslationEnv,
    libraryFields: Vector[Symbol],
    libraryTypes: Vector[Symbol],
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

    val factory = LayerFactory('GraphEmbedding, ParamCollection())
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
    transEnv: TranslationEnv
  ): Unit = {

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
      GraphNetBuilder(trainingModules, transEnv, libraryFields, libraryTypes)

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
          "certainty" -> PlotConfig("ImageSize->Medium"),
          "iteration-time" -> PlotConfig(
            "ImageSize->Medium",
            """AxesLabel->{"step","ms"}"""
          ),
          "loss" -> PlotConfig("ImageSize->Large"),
          "accuracy" -> PlotConfig("ImageSize->Large")
        )
      )
    }

    import TensorExtension.oneHot

    val optimizer = Optimizers.Adam(learningRate = 4e-4)

    // training loop
    for (step <- 0 until 1000) {
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

      eventLogger.log(
        "certainty",
        step,
        trainBuilder.factory.getVar(Symbol("decode:certainty"))(throw new Error()).value
      )

      note("analyzeResults")
      val accuracy = analyzeResults(
        typeLabels,
        logits.value,
        transEnv,
        decodingCtx,
        printResults = printCorrectWrongs
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
}
