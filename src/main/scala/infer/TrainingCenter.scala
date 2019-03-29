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
import infer.PredicateGraphConstruction._

import scala.collection.{immutable, mutable}
import scala.collection.parallel.ForkJoinTaskSupport
import ammonite.ops._

object TrainingCenter {

  def main(args: Array[String]): Unit = {
    TensorExtension.checkNaN = false // uncomment to train faster
    trainOnModules()
  }

  def trainOnModules() = {
    val root = pwd / RelPath("data/ts-algorithms")
    val parsed = infer.PredicateGraphConstruction
      .fromSourceFiles(root, marksToHoles = true)
    val modules = parsed.predModules

    val transEnv = parsed.irEnv
    val annotatedPlaces = parsed.typeHoleContext.holeTypeMap.map {
      case (h, t) => transEnv.holeTyVarMap(h).id -> t
    }.toIndexedSeq

    val predicates = parsed.predModules.flatMap(m => m.predicates) ++ PredicateGraphConstruction
      .encodeUnaryPredicates(transEnv.idTypeMap.values)
    val newTypes = modules.flatMap(m => m.newTypes)

    println("Predicate numbers:")
    println {
      predicates.groupBy(PredicateGraph.predicateCategory).mapValuesNow { _.length }
    }
    println {
      val wrongNodes = Seq(GTHole(1), GTHole(2)).map(transEnv.holeTyVarMap)
      val graphString = PredicateGraph
        .displayPredicateGraph(
          (transEnv.idTypeMap.values.toSet -- wrongNodes.toSet).toSeq,
          wrongNodes,
          predicates,
          transEnv.tyVarHoleMap.toMap
        )
        .toMamFormat("Automatic", directed = false)
      import ammonite.ops._
      write.over(pwd / "running-result" / "graph.txt", graphString)
      graphString
    }

    /** any symbols that are not defined within the project */
    val libraryFields: Vector[Symbol] = {
      var allDefined, allUsed = Set[Symbol]()
      parsed.irModules.foreach(m => {
        val stats = m.moduleStats
        allDefined ++= stats.fieldsDefined
        allUsed ++= stats.fieldsUsed
      })
      (allUsed -- allDefined).toVector
    }
    println("libraryFields: " + libraryFields)

    /** any types that are not defined within the project */
    val libraryTypes: Vector[Symbol] = JSExamples.typeContext.typeUnfold.keys.toVector //todo: improve

    val dimMessage = 64

    val factory = LayerFactory('GraphEmbedding, ParamCollection())
    import factory._

    def randomVar(path: SymbolPath): CompNode =
      getVar(path)(numsca.randn(1, dimMessage) * 0.01)

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

    import GraphEmbedding._
    import TensorExtension.oneHot

    val optimizer = Optimizers.Adam(learningRate = 4e-4)

    val numOfThreads = Runtime.getRuntime.availableProcessors()
    val taskSupport = new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads))
    val parallelCtx =
      concurrent.ExecutionContext.fromExecutorService(new ForkJoinPool(numOfThreads))

    // training loop
    for (step <- 0 until 1000) {
      val startTime = System.currentTimeMillis()

      val libraryTypeMap: Map[Symbol, CompNode] = {
        libraryTypes.toList.map { k =>
          k -> randomVar('TyVar / k)
        }
      }.toMap

      val fieldKnowledge =
        libraryFields.map { k =>
          k -> (randomVar('fieldKey / k), randomVar('fieldValue / k))
        }.toMap

      val extendedTypeMap = BufferedTotalMap(libraryTypeMap.get) { _ =>
        randomVar('TyVar / GraphEmbedding.unknownTypeSymbol)
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
      val decodingCtx = DecodingCtx(
        Vector(AnyType, TyVar(unknownTypeSymbol)) ++ libraryTypeMap.keys.toIndexedSeq
          .map(TyVar),
        newTypes.map(_._1)
      )

      def logEmbeddingMagnitudeAndChanges(
        embeddings: IS[GraphEmbedding.Embedding]
      ): Unit = {

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
      val logits =
        GraphEmbedding(embedCtx, factory, dimMessage, Some(taskSupport))
          .encodeAndDecode(
            iterations = 10,
            decodingCtx,
            annotatedPlaces.map(_._1),
            logEmbeddingMagnitudeAndChanges
          )
      //      println("Predictions: ====")
      //      println(logits)
      eventLogger.log("certainty", step, getVar(Symbol("decode:certainty"))(throw new Error()).value)

      val accuracy = analyzeResults(annotatedPlaces, logits.value, transEnv, decodingCtx)
      eventLogger.log("accuracy", step, Tensor(accuracy))

      val targets = annotatedPlaces.map(p => decodingCtx.indexOfType(p._2))
      val loss = mean(
        crossEntropyOnSoftmax(logits, oneHot(targets, decodingCtx.maxIndex))
      )

      if (loss.value.squeeze() > 20) {
        println(s"Abnormally large loss: ${loss.value}")
        println("logits: ")
        println { logits.value }
      }
      eventLogger.log("loss", step, loss.value)

      DebugTime.logTime('optimization) {
        optimizer.minimize(loss, params.allParams, backPropInParallel = Some(parallelCtx))
      }

      println(DebugTime.show)

      eventLogger.log(
        "iteration-time",
        step,
        Tensor(System.currentTimeMillis() - startTime)
      )
    }
  }

  def trainOnSingleExample() = {
    TensorExtension.checkNaN = false // uncomment to train faster

    val example = JSExamples.Collection.doublyLinkedList
    val transEnv = new TranslationEnv()

    println("Program statements: === ")
    println { example.program }

    val stmts = IRTranslation.translateStmt(example.program)(Set(), transEnv)

//    println("IR statements: === ")
//    stmts.foreach(println)

    val annotatedPlaces = example.holeTypeMap.map {
      case (h, t) => transEnv.holeTyVarMap(h).id -> t
    }.toIndexedSeq

    val (predicates, newTypes) = {
      val (ps, ctx) =
        PredicateGraphConstruction.encodeIR(stmts, PredicateContext.jsCtx(transEnv))
      val ups =
        PredicateGraphConstruction.encodeUnaryPredicates(transEnv.idTypeMap.values)
      (ps ++ ups, ctx.newTypeMap.toIndexedSeq)
    }
    println("Predicate numbers:")
    println {
      predicates.groupBy(PredicateGraph.predicateCategory).mapValuesNow { _.length }
    }
    println {
      val wrongNodes = Seq(GTHole(1), GTHole(2)).map(transEnv.holeTyVarMap)
      val graphString = PredicateGraph
        .displayPredicateGraph(
          (transEnv.idTypeMap.values.toSet -- wrongNodes.toSet).toSeq,
          wrongNodes,
          predicates,
          transEnv.tyVarHoleMap.toMap
        )
        .toMamFormat("Automatic", directed = false)
      import ammonite.ops._
      write.over(pwd / "running-result" / "graph.txt", graphString)
      graphString
    }

    val knownFields = (JSExamples.typeContext.typeUnfold.values.flatMap {
      case ObjectType(fields) => fields.keySet
      case _                  => Set[Symbol]()
    } ++ JSExamples.exprContext.varAssign.keySet).toIndexedSeq
    println("knownFields: " + knownFields)

    val dimMessage = 64

    val factory = LayerFactory('GraphEmbedding, ParamCollection())
    import factory._

    def randomVar(path: SymbolPath): CompNode =
      getVar(path)(numsca.randn(1, dimMessage) * 0.01)

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

    import GraphEmbedding._
    import TensorExtension.oneHot

    val optimizer = Optimizers.Adam(learningRate = 4e-4)

    val numOfThreads = Runtime.getRuntime.availableProcessors()
    val taskSupport = new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads))
    val parallelCtx =
      concurrent.ExecutionContext.fromExecutorService(new ForkJoinPool(numOfThreads))

    // training loop
    for (step <- 0 until 1000) {
      val startTime = System.currentTimeMillis()

      val libraryTypeMap: Map[Symbol, CompNode] = {
        val keys = JSExamples.typeContext.typeUnfold.keySet
        keys.toList.map { k =>
          k -> randomVar('TyVar / k)
        }
      }.toMap

      val fieldKnowledge =
        knownFields.map { k =>
          k -> (randomVar('fieldKey / k), randomVar('fieldValue / k))
        }.toMap

      val extendedTypeMap = BufferedTotalMap(libraryTypeMap.get) { _ =>
        randomVar('TyVar / GraphEmbedding.unknownTypeSymbol)
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
      val decodingCtx = DecodingCtx(
        Vector(AnyType, TyVar(unknownTypeSymbol)) ++ libraryTypeMap.keys.toIndexedSeq
          .map(TyVar),
        newTypes.map(_._2)
      )

      def logEmbeddingMagnitudeAndChanges(
        embeddings: IS[GraphEmbedding.Embedding]
      ): Unit = {
//        val magnitudes = Tensor(embeddings.map { e =>
//          SimpleMath.mean(e.nodeMap.values.map { n =>
//            math.sqrt(numsca.sum(numsca.square(n.value)))
//          }.toVector)
//        }: _*)
//        eventLogger.log("embedding-magnitudes", step, magnitudes)

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
      val logits =
        GraphEmbedding(embedCtx, factory, dimMessage, Some(taskSupport))
          .encodeAndDecode(
            iterations = 10,
            decodingCtx,
            annotatedPlaces.map(_._1),
            logEmbeddingMagnitudeAndChanges
          )
//      println("Predictions: ====")
//      println(logits)
      eventLogger.log("certainty", step, getVar(Symbol("decode:certainty"))(???).value)

      val accuracy = analyzeResults(annotatedPlaces, logits.value, transEnv, decodingCtx)
      eventLogger.log("accuracy", step, Tensor(accuracy))

      val targets = annotatedPlaces.map(p => decodingCtx.indexOfType(p._2))
      val loss = mean(
        crossEntropyOnSoftmax(logits, oneHot(targets, decodingCtx.maxIndex))
      )

      if (loss.value.squeeze() > 20) {
        println(s"Abnormally large loss: ${loss.value}")
        println("logits: ")
        println { logits.value }
      }
      eventLogger.log("loss", step, loss.value)

      DebugTime.logTime('optimization) {
        optimizer.minimize(loss, params.allParams, backPropInParallel = Some(parallelCtx))
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
    annotatedPlaces: IS[(Int, GType)],
    logits: Tensor,
    transEnv: TranslationEnv,
    ctx: DecodingCtx,
    printResults: Boolean = true
  ): Double = {
    type TypeIdx = Int
    val predictions = numsca.argmax(logits, axis = 1)
    val correct = mutable.ListBuffer[(GTHole, TypeIdx)]()
    val incorrect = mutable.ListBuffer[(GTHole, TypeIdx)]()
    for (row <- annotatedPlaces.indices) {
      val (nodeId, t) = annotatedPlaces(row)
      val hole = transEnv.tyVarHoleMap(nodeId)
      val expected = ctx.indexOfType(t)
      val actual = predictions(row, 0).squeeze().toInt

      if (expected == actual)
        correct += (hole -> actual)
      else {
        incorrect += (hole -> actual)
      }
    }

    correct.foreach {
      case (hole, tId) =>
        val t = ctx.typeOfIndex(tId)
        val tv = transEnv.holeTyVarMap(hole)
        if(printResults)
          println(s"[correct] \t$tv $hole: $t")
    }

    val holeTypeMap = annotatedPlaces.toMap
    incorrect.foreach {
      case (hole, tId) =>
        val tv = transEnv.holeTyVarMap(hole)
        val actualType = ctx.typeOfIndex(tId)
        val expected = holeTypeMap(transEnv.holeTyVarMap(hole).id)
        if(printResults)
          println(s"[incorrect] \t$tv $hole: $actualType not match $expected")
    }

    val accuracy = correct.length.toDouble / (correct.length + incorrect.length)
    accuracy
  }
}
