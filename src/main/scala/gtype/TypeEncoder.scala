package gtype

import funcdiff.API._
import funcdiff._
import botkop.numsca
import botkop.numsca.Tensor
import funcdiff.Optimizers.Adam
import gtype.TypeConstraintGraph._

import scala.util.Random

case class TypeEncoding(labelMap: Map[Symbol, CompNode], typeMap: Map[Symbol, CompNode])

//noinspection TypeAnnotation
object FieldCombineMethod extends Enumeration {

  /** Sum all field encodings together */
  val Sum = Value

  /** Weighted average field encodings using an attention mechanism */
  val Attention = Value
}

//noinspection TypeAnnotation
object ArgsEncodingMethod extends Enumeration {

  /** Encode arg types and return type separately and combine them using a linear layer */
  val Separate = Value

  /** Use the return type as the initial state of an RNN, then feed all arg types to it */
  val Unified = Value
}

case class EncoderParams(
    labelDim: Int,
    typeDim: Int,
    fieldDim: Int,
    fieldCombineMethod: FieldCombineMethod.Value = FieldCombineMethod.Attention,
    argsEncodingMethod: ArgsEncodingMethod.Value = ArgsEncodingMethod.Unified,
    updateWithRNN: Boolean = false,
    activation: CompNode => CompNode)

object EncoderParams {
  val small = EncoderParams(
    labelDim = 40,
    typeDim = 40,
    fieldDim = 40,
    activation = x => funcdiff.API.leakyRelu(x)
  )

  val large = EncoderParams(
    labelDim = 40,
    typeDim = 80,
    fieldDim = 120,
    activation = x => funcdiff.API.leakyRelu(x)
  )
}

class TypeEncoder(encoderParams: EncoderParams) {

  import encoderParams._

  val pc = ParamCollection()
  val typePath: SymbolPath = SymbolPath.empty / 'type
  val subtypePath: SymbolPath = SymbolPath.empty / 'subtype

  def encode(symbolDefMap: Map[Symbol, TypeRewrite], iterations: Int): TypeEncoding = {
    import collection.mutable
    val labelMap = mutable.HashMap[Symbol, CompNode]()

    def getFieldLabel(label: Symbol): CompNode = {
      labelMap.getOrElseUpdate(
        label,
        const(TensorExtension.randomUnitVec(labelDim))
      )
    }

    val anyInit = pc.getVar(typePath / 'anyInit)(numsca.randn(1, typeDim) * 0.01)
    val fieldZeroInit = const(numsca.zeros(1, fieldDim))
    val typeInit = pc.getVar(typePath / 'typeInit)(numsca.randn(1, typeDim) * 0.01)
    val attentionTypeKernel = pc.getVar(typePath / 'attentionTypeKernel)(
      numsca.rand(typeDim, typeDim) * 0.001
    )
    val attentionFieldKernel = pc.getVar(typePath / 'attentionFieldKernel)(
      numsca.rand(fieldDim, typeDim) * 0.001
    )
    val attentionVec = pc
      .getVar(typePath / 'attentionVec)(numsca.rand(1, 2 * fieldDim) * 0.01)
      .t

    var typeMap: Map[Symbol, CompNode] = symbolDefMap.keys
      .map { tyName =>
        tyName -> typeInit
      }
      .toMap
      .updated(AnyType.id, anyInit)

    for (_ <- 0 until iterations) {
      val newTypeMap = symbolDefMap.map {
        case (tyName, typeDef) =>
          val typeFactory = LayerFactory(typePath, pc)
          import typeFactory._

          val aggregated = typeDef match {
            case FuncRewrite(argTypes, returnType) =>
              argsEncodingMethod match {
                case ArgsEncodingMethod.Separate =>
                  val funcArgInit: CompNode =
                    pc.getVar(typePath / 'funcArgInit)(numsca.rand(1, typeDim) * 0.01)
                  val argsEncoding = argTypes
                    .map(typeMap)
                    .foldLeft(funcArgInit)(gru('FuncArgsGru))
                  val x = argsEncoding.concat(typeMap(returnType), axis = 1)
                  activation(linear('FuncDefLinear, nOut = fieldDim)(x))
                case ArgsEncodingMethod.Unified =>
                  argTypes
                    .map(typeMap)
                    .foldRight(typeMap(returnType))(gru('FuncArgsGru))
              }

            case ObjectRewrite(fields) =>
              val fieldEncodings = fields.map {
                case (fieldName, fieldType) =>
                  val x = getFieldLabel(fieldName)
                    .concat(typeMap(fieldType), axis = 1)
                  activation(linear('ContainsFieldLinear, nOut = fieldDim)(x))
              }.toVector
              fieldCombineMethod match {
                case FieldCombineMethod.Sum =>
                  if (fieldEncodings.nonEmpty) total(fieldEncodings)
                  else fieldZeroInit
                case FieldCombineMethod.Attention =>
                  // see the paper 'Graph Attention Networks'
                  val transformedThis = typeMap(tyName) dot attentionTypeKernel
                  assert(transformedThis.shape(1) == typeDim)
                  val transformedFields = fieldEncodings.map { _ dot attentionFieldKernel }
                  val allTransformed = transformedFields :+ transformedThis
                  val attentionLogits = allTransformed.map { ft =>
                    val w = transformedThis
                      .concat(ft, axis = 1)
                      .dot(attentionVec)
                    assert(w.shape.product == 1)
                    leakyRelu(w, 0.2)
                  }
                  val aWeights = softmax(concatN(attentionLogits, axis = 1))
                  total(allTransformed.indices.map { i =>
                    aWeights.slice(0, i :> (i + 1)) * allTransformed(i)
                  })
              }
          }

          val newTypeEnc = if (updateWithRNN) {
            gru('UpdateGRU)(
              state = typeMap(tyName),
              input = aggregated
            )
          } else aggregated
          tyName -> newTypeEnc
      }

      typeMap = newTypeMap.updated(AnyType.id, anyInit)
    }
    TypeEncoding(labelMap.toMap, typeMap)
  }

  def subtypePredict(encoding: TypeEncoding, typePairs: Seq[(Symbol, Symbol)]): CompNode = {
    val (t1s, t2s) = typePairs.unzip
    val t1 = concatN(t1s.map(encoding.typeMap).toVector, axis = 0)
    val t2 = concatN(t2s.map(encoding.typeMap).toVector, axis = 0)

    val modelFactory = LayerFactory(subtypePath, pc)
    import modelFactory._

    //      val layer1 = activation(
    //        linear('linear1, nOut = typeDim)(t1.concat(t2, axis = 1))
    //      )
    //      val layer2 = activation(linear('linear2, nOut = typeDim / 2)(layer1))
    //      activation(linear('linear3, nOut = 2)(layer2))

    linear('simpleLinear, nOut = 2)((t1 * t2).concat(t1 + t2, axis = 1).concat(t2 - t1, axis = 1))
  }

}

object TypeEncoder {
  def main(args: Array[String]): Unit = {

    implicit val random: Random = new Random()

    val encoder = new TypeEncoder(EncoderParams.small)

    val posVec = Tensor(1, 0).reshape(1, -1)
    val negVec = Tensor(0, 1).reshape(1, -1)

    def forwardPredict(symbolDefMap: Map[Symbol, TypeRewrite],
                       posRelations: Seq[(Symbol, Symbol)],
                       negRelations: Seq[(Symbol, Symbol)]) = {
      val relationNum = math.min(posRelations.length, negRelations.length)
      val posExamples = random.shuffle(posRelations).take(relationNum)
      val negExamples = random.shuffle(negRelations).take(relationNum)

      val target = numsca.concatenate(posExamples.map(_ => posVec)
                                        ++ negExamples.map(_ => negVec),
                                      axis = 0)

      val typeEncoding = encoder.encode(symbolDefMap, iterations = 5)
      val examples = posExamples ++ negExamples
      val predictions =
        encoder.subtypePredict(typeEncoding, examples)
      (target, predictions, examples)
    }

    import TrainingTypeGeneration.augmentWithRandomTypes

    val trainingSet = {
      List(
        "JSTraining100" -> augmentWithRandomTypes(JSExamples.trainingTypeContext, 100),
        "JSTraining90" -> augmentWithRandomTypes(JSExamples.trainingTypeContext, 90),
        "JSCore100" -> augmentWithRandomTypes(JSExamples.typeContext, 100),
        "JSCore110" -> augmentWithRandomTypes(JSExamples.typeContext, 110),
      )
    }

    val devSet = {
      val context = augmentWithRandomTypes(Examples.pointExample, 50)
      val typeRewrites = typeContextToRewrites(context)
      val typeContext = typeRewritesToContext(typeRewrites)

      val rels = Vector(
        'number -> 'number,
        'point -> 'point2D,
        'point2D -> 'point,
        'point -> 'point,
        'A -> 'A,
        'point2D -> 'point2D,
        'A -> 'point,
        'A -> 'point2D,
        'A -> 'B,
        'B -> 'A,
        'A -> 'C,
        'C -> 'B,
        'C -> 'C,
        'stringPoint -> 'point,
        'stringPoint -> 'point2D
      )
      typeRewrites -> rels.map {
        case (l, r) =>
          (l, r) ->
            typeContext.isSubtype(
              GroundType.symbolToType(l),
              GroundType.symbolToType(r)
            )
      }
    }

    val preComputes = trainingSet.map {
      case (name, context) =>
        val symbolDefMap = typeContextToRewrites(context)
        println("Symbol rewrites map:")
        symbolDefMap.foreach(println)
        assert(symbolDefMap.contains(JSExamples.boolean))
        val (reflexivity, posRelations, negRelations) = typeRewritesToGroundTruths(symbolDefMap)

        println(
          s"pos relations: ${posRelations.length}, neg relations: ${negRelations.length}, " +
            s"reflexivity: ${reflexivity.length}"
        )
        (name, symbolDefMap, (posRelations ++ reflexivity, negRelations))
    }

    val optimizer = Adam(learningRate = 0.003)

    for (step <- 0 until 1000) {
      for ((exampleName, symbolDefMap, (posRelations, negRelations)) <- preComputes) {

        val (target, predictions, _) =
          forwardPredict(symbolDefMap, posRelations, negRelations)
        val loss = crossEntropyOnSoftmax(predictions, target)

        println(s"[$exampleName][$step] average loss: ${mean(loss).value}")
        val (correct, wrong) =
          correctWrongSets(predictions.value, target(:>, 1).data.map(_.toInt))
        println(
          s"[$exampleName] accuracy = ${correct.size.toDouble / (correct.size + wrong.size)}"
        )

        optimizer.minimize(loss, encoder.pc.allParams, weightDecay = Some(1e-4))
      }

      if (step % 10 == 0) {
        val posDev = devSet._2.collect { case (r, b) if b  => r }
        val negDev = devSet._2.collect { case (r, b) if !b => r }
        val (target, predictions, examples) = forwardPredict(devSet._1, posDev, negDev)
        val (correct, wrong) =
          correctWrongSets(predictions.value, target(:>, 1).data.map(_.toInt))
        val accuracy = correct.size.toDouble / (correct.size + wrong.size)
        println("dev set accuracy = " + accuracy)
        println("=== correct ===")
        correct.foreach(i => println(examples(i)))
        println("=== wrong ===")
        wrong.foreach(i => println(examples(i)))
      }
    }
  }
}
