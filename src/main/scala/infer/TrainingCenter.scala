package infer

import botkop.numsca
import funcdiff.SimpleMath.BufferedTotalMap
import funcdiff.{CompNode, ParamCollection, Optimizers, TensorExtension}
import gtype._
import infer.IRTranslation.TranslationEnv
import infer.PredicateGraph._

object TrainingCenter {

  def main(args: Array[String]): Unit = {
    val transEnv = new TranslationEnv()
    val example = JSExamples.Collection.doublyLinkedList

    val stmts = IRTranslation.translateStmt(example.program)(transEnv)
    //    println("holeTypeMap: " + example.holeTypeMap)
    val annotatedPlaces = example.holeTypeMap.map {
      case (h, t) => transEnv.holeTyVarMap(h).id -> t
    }.toIndexedSeq

    val predicateCtx = PredicateContext.jsCtx(transEnv)

    val predicates = {
      PredicateGraph.encodeIR(stmts, predicateCtx) ++ PredicateGraph
        .encodeUnaryPredicates(
          transEnv.idTypeMap.values
        )
    }
    val newTypes = predicateCtx.newTypeMap.toIndexedSeq

    val pc = ParamCollection()
    val dimMessage = 64

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
    for (step <- 0 until 100) {

      val concreteTypeMap = {
        //todo: add more structural information
        val keys = JSExamples.typeContext.typeUnfold.keySet ++ Set(
          AnyType.id,
          unknownTypeSymbol
        )
        keys.toList.map { k =>
          (TyVar(k): GType) -> (pc.getVar('TyVar / k)(numsca.randn(1, dimMessage) * 0.01): CompNode)
        }
      }.toMap

      val labelMap = {
        val labels =
          List('OP_Plus, 'OP_Minus, 'OP_Times, 'OP_Divide, 'OP_LessThan, 'charAt)
        labels.map { k =>
          k -> (pc.getVar('labelVec / k)(numsca.randn(1, dimMessage) * 0.01): CompNode)
        }.toMap
      }

      val extendedTypeMap = BufferedTotalMap(concreteTypeMap.get) { _ =>
        const(TensorExtension.randomUnitVec(dimMessage)) //todo: add encoding layer
      }

      val extendedLabelMap = BufferedTotalMap(labelMap.get) { _ =>
        const(TensorExtension.randomUnitVec(dimMessage)) //todo: add encoding layer
      }

      val embedCtx = EmbeddingCtx(
        transEnv.idTypeMap.toMap,
        extendedTypeMap,
        predicates,
        extendedLabelMap
      )
      val decodingCtx = DecodingCtx(
        concreteTypeMap.keys.toIndexedSeq,
        newTypes.map(_._2)
      )

      val targets = annotatedPlaces.map(p => decodingCtx.indexOfType(p._2))

      val logits =
        GraphEmbedding(embedCtx, pc, dimMessage)
          .encodeAndDecode(
            iterations = 10,
            decodingCtx,
            annotatedPlaces.map(_._1)
          )
      println("Predictions: ====")
      println(logits)
      println("Ground truths: ===")
      println(annotatedPlaces.map(_._2))

      val loss = mean(
        crossEntropyOnSoftmax(logits, oneHot(targets, decodingCtx.maxIndex))
      )
      eventLogger.log("loss", step, loss.value)
      // minimize the loss
      optimizer.minimize(loss, pc.allParams, weightDecay = Some(1e-4))
    }
  }

}
