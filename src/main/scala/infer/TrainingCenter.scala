package infer

import botkop.numsca
import funcdiff.{CompNode, ParamCollection, SimpleMath, TensorExtension}
import gtype._
import infer.IRTranslation.TranslationEnv

object TrainingCenter {

  def main(args: Array[String]): Unit = {
    val transEnv = new TranslationEnv()
    val example = JSExamples.Collection.doublyLinkedList

    GStmt.assertAllTypesStripped(example.program)

    val stmts = IRTranslation.translateStmt(example.program)(transEnv)

    println("IR Programs: ===")
    stmts.foreach(println)

    println("holeTypeMap: " + example.holeTypeMap)
    val annotatedPlaces = example.holeTypeMap.map {
      case (h, t) =>
        transEnv.holeTyVarMap(h).id -> t
    }.toIndexedSeq

    import infer.PredicateGraph._
    val predicateCtx = EncodingCtx.jsCtx(transEnv)

    val predicates = {
      PredicateGraph.encodeIR(stmts, predicateCtx) ++ PredicateGraph
        .encodeUnaryPredicates(
          transEnv.idTypeMap.values
        )
    }

    val newTypes = predicateCtx.newTypeMap.toIndexedSeq

    predicates.foreach(println)

    import GraphEmbedding._
    import funcdiff.API._

    val pc = ParamCollection()
    val dimMessage = 64

    val concreteTypeMap = {
      val keys = JSExamples.typeContext.typeUnfold.keySet
      keys.toList.map { k =>
        (TyVar(k): GType) -> (pc.getVar('TyVar / k)(numsca.randn(1, dimMessage) * 0.01): CompNode)
      }
    }.toMap

    val labelMap = {
      val labels = List('OP_Plus, 'OP_Minus, 'OP_Times, 'OP_Divide, 'OP_LessThan, 'charAt)
      labels.map { k =>
        k -> (pc.getVar('labelVec / k)(numsca.randn(1, dimMessage) * 0.01): CompNode)
      }
    }.toMap

    for (step <- 0 until 2) {
      val extendedTypeMap = SimpleMath.BufferedTotalMap(concreteTypeMap.get) { _ =>
        const(TensorExtension.randomUnitVec(dimMessage)) //fixme: add encoding layer
      }

      val extendedLabelMap = SimpleMath.BufferedTotalMap(labelMap.get) { _ =>
        const(TensorExtension.randomUnitVec(dimMessage)) //fixme: add encoding layer
      }

      val embedCtx = EmbeddingCtx(
        transEnv.idTypeMap.toMap,
        extendedTypeMap,
        predicates,
        extendedLabelMap
      )
      val decodingCtx = DecodingCtx(
        concreteTypeMap.keys.toIndexedSeq,
        newTypes.map(_._2.id)
      )

      val ge = GraphEmbedding(embedCtx, pc, dimMessage)
      val predictions = ge.encodeAndDecode(
        iterations = 10,
        decodingCtx,
        annotatedPlaces.map(_._1)
      )
      println("Predictions: ====")
      println(predictions)
      println("Ground truths: ===")
      println(annotatedPlaces.map(_._2))
    }
  }

}
