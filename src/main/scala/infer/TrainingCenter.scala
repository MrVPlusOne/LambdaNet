package infer

import botkop.numsca
import botkop.numsca.Tensor
import funcdiff.SimpleMath.BufferedTotalMap
import funcdiff._
import gtype._
import infer.IRTranslation.TranslationEnv
import infer.PredicateGraph._
import funcdiff.API._
import infer.GraphEmbedding.DecodingCtx
import collection.mutable

object TrainingCenter {

  def main(args: Array[String]): Unit = {
    TensorExtension.checkNaN = false  // uncomment to train faster

    val example = JSExamples.Collection.doublyLinkedList
    val transEnv = new TranslationEnv()

    println("Program statements: === ")
    println { example.program }

    val stmts = IRTranslation.translateStmt(example.program)(transEnv)

//    println("IR statements: === ")
//    stmts.foreach(println)

    val annotatedPlaces = example.holeTypeMap
      .map {
        case (h, t) => transEnv.holeTyVarMap(h).id -> t
      }
      .toIndexedSeq

    println("values: " + transEnv.idTypeMap)

    val (predicates, newTypes) = {
      val (ps, ctx) = PredicateGraph.encodeIR(stmts, PredicateContext.jsCtx(transEnv))
      val ups = PredicateGraph.encodeUnaryPredicates(transEnv.idTypeMap.values)
      (ps ++ ups, ctx.newTypeMap.toIndexedSeq)
    }
    println("Predicates: =====")
    predicates.foreach(println)
//    println("newTypes: " + newTypes)

    val labels = (JSExamples.typeContext.typeUnfold.values.flatMap {
      case ObjectType(fields) => fields.keySet
      case _                  => Set[Symbol]()
    } ++ JSExamples.exprContext.varAssign.keySet).toIndexedSeq
    println("labels: " + labels)

    val dimMessage = 64

    val factory = LayerFactory('GraphEmbedding, ParamCollection())
    import factory._

    val eventLogger = {
      import ammonite.ops._
      new EventLogger(
        pwd / "running-result" / "log.txt",
        printToConsole = true,
        overrideMode = true
      )
    }

    import GraphEmbedding._
    import funcdiff.API._
    import TensorExtension.oneHot

    val optimizer = Optimizers.Adam(learningRate = 1e-4)
    // training loop
    for (step <- 0 until 1000) {

      val libraryTypeMap: Map[Symbol, CompNode] = {
        val keys = JSExamples.typeContext.typeUnfold.keySet
        keys.toList.map { k =>
          k -> (getVar('TyVar / k)(numsca.randn(1, dimMessage) * 0.01): CompNode)
        }
      }.toMap

      val labelMap =
        labels.map { k =>
          k -> (getVar('labelVec / k)(numsca.randn(1, dimMessage) * 0.01): CompNode)
        }.toMap

      val extendedTypeMap = BufferedTotalMap(libraryTypeMap.get) { _ =>
        getVar('TyVar / GraphEmbedding.unknownTypeSymbol)(
          numsca.randn(1, dimMessage) * 0.01
        )
      }

      val extendedLabelMap = BufferedTotalMap(labelMap.get) { k =>
        const(TensorExtension.randomUnitVec(dimMessage)) //todo: add encoding layer
//        pc.getVar('extraLabelVec / k)(numsca.randn(1, dimMessage) * 0.01): CompNode
      }

      val embedCtx = EmbeddingCtx(
        transEnv.idTypeMap.toMap,
        extendedTypeMap,
        predicates,
        extendedLabelMap
      )
      val decodingCtx = DecodingCtx(
        Vector(AnyType, TyVar(unknownTypeSymbol)) ++ libraryTypeMap.keys.toIndexedSeq
          .map(TyVar),
        newTypes.map(_._2)
      )

      val logits =
        GraphEmbedding(embedCtx, factory, dimMessage)
          .encodeAndDecode(
            iterations = 12,
            decodingCtx,
            annotatedPlaces.map(_._1)
          )
//      println("Predictions: ====")
//      println(logits)
      val accuracy = analyzeResults(annotatedPlaces, logits.value, transEnv, decodingCtx)
      eventLogger.log("accuracy", step, Tensor(accuracy))

      val targets = annotatedPlaces.map(p => decodingCtx.indexOfType(p._2))
      val loss = mean(
        crossEntropyOnSoftmax(logits, oneHot(targets, decodingCtx.maxIndex))
      )

      if(loss.value.squeeze() > 20){
        println(s"Abnormally large loss: ${loss.value}")
        println("logits: ")
        println{logits.value}
      }
      eventLogger.log("loss", step, loss.value)
      // minimize the loss
//      println("all params: " + pc.allParams)
      val parallelCtx = concurrent.ExecutionContext.global
      optimizer.minimize(loss, params.allParams, backPropInParallel = Some(parallelCtx))
    }
  }

  def analyzeResults(
    annotatedPlaces: IS[(Int, GType)],
    logits: Tensor,
    transEnv: TranslationEnv,
    ctx: DecodingCtx
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
        println(s"Hole $hole [correct]: $t")
    }

    val holeTypeMap = annotatedPlaces.toMap
    incorrect.foreach {
      case (hole, tId) =>
        val actualType = ctx.typeOfIndex(tId)
        val expected = holeTypeMap(transEnv.holeTyVarMap(hole).id)
        println(s"Hole $hole [incorrect]: $actualType not match $expected")
    }

    val accuracy = correct.length.toDouble / (correct.length + incorrect.length)
    accuracy
  }
}
