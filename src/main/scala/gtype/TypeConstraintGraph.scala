package gtype

import botkop.numsca.Tensor
import funcdiff.Optimizers.Adam
import funcdiff.{CompNode, LayerFactory, ParamNode}

import scala.util.Random

/**
  * Turn type definitions into a constraint graph over literals
  */
object TypeConstraintGraph {

  sealed trait TypeDef

  case class FuncDef(argTypes: List[Symbol], returnType: Symbol) extends TypeDef

  case class ObjectDef(fields: Map[Symbol, Symbol]) extends TypeDef

  def typeDefsToContext(typeDefs: Map[Symbol, TypeDef]): TypeContext = {
    import GroundType.symbolToType

    val typeUnfold = typeDefs.mapValues {
      case ObjectDef(fields) => ObjectType(fields.mapValues(symbolToType))
      case FuncDef(argTypes, returnType) =>
        FuncType(argTypes.map(symbolToType), symbolToType(returnType))
    }

    TypeContext(baseTypes = Set(), typeUnfold = typeUnfold, subRel = Set())
  }

  def typeContextToConstraints(context: TypeContext): Map[Symbol, TypeDef] = {

    import collection.mutable
    val tConstraints = mutable.HashMap[Symbol, TypeDef]()
    val typeNameMap = mutable.HashMap[CompoundType, Symbol]()

    def getTypeName(ty: GType, useName: Option[Symbol]): Symbol = {
      ty match {
        case g: GroundType => g.id
        case ty: CompoundType =>
          typeNameMap.getOrElseUpdate(
            ty, {
              val tDef = ty match {
                case ObjectType(fields) =>
                  ObjectDef(fields.mapValues(f => getTypeName(f, None)))
                case FuncType(from, to) =>
                  FuncDef(from.map { x =>
                    getTypeName(x, None)
                  }, getTypeName(to, None))
              }

              val newSymbol = useName.getOrElse(Symbol("$" + tDef.toString))
              tConstraints(newSymbol) = tDef
              println(s"update: $newSymbol: $tDef")
              newSymbol
            }
          )
      }
    }

    context.typeUnfold.foreach {
      case (tyName, ty) => getTypeName(ty, Some(tyName))
    }
    tConstraints.toMap
  }

  //noinspection TypeAnnotation
  object FieldAggregation extends Enumeration {
    val Sum = Value
    val Attention = Value
  }

  case class EncoderParams(labelDim: Int,
                           typeDim: Int,
                           fieldDim: Int,
                           fieldAggregation: FieldAggregation.Value,
                           activation: CompNode => CompNode)

  object EncoderParams {
    val small = EncoderParams(
      labelDim = 30,
      typeDim = 40,
      fieldDim = 40,
      fieldAggregation = FieldAggregation.Attention,
      activation = x => funcdiff.API.leakyRelu(x)
    )
  }

  import funcdiff.API._
  import funcdiff.{ParamCollection, SymbolPath, TensorExtension}
  import botkop.numsca

  case class TypeEncoding(labelMap: Map[Symbol, CompNode], typeMap: Map[Symbol, CompNode])

  class TypeEncoder(encoderParams: EncoderParams) {

    import encoderParams._

    val pc = ParamCollection()
    val typePath: SymbolPath = SymbolPath.empty / 'type
    val subtypePath: SymbolPath = SymbolPath.empty / 'subtype

    def encode(symbolDefMap: Map[Symbol, TypeDef], iterations: Int): TypeEncoding = {
      import collection.mutable
      val labelMap = mutable.HashMap[Symbol, CompNode]()

      def getFieldLabel(label: Symbol): CompNode = {
        labelMap.getOrElseUpdate(
          label,
          const(TensorExtension.randomUnitVec(labelDim))
        )
      }

      val anyInit =
        pc.getVar(typePath / 'anyInit)(numsca.randn(1, typeDim) * 0.01)
      val fieldZeroInit = const(numsca.zeros(1, fieldDim))
      val funcArgInit =
        pc.getVar(typePath / 'funcArgInit)(numsca.rand(1, typeDim) * 0.01)
      val attentionKernel = pc.getVar(typePath / 'attentionKernel)(
        numsca.rand(fieldDim, fieldDim) * 0.001
      )
      val attentionVec = pc
        .getVar(typePath / 'attentionVec)(numsca.rand(1, 2 * fieldDim) * 0.01)
        .t

      var typeMap: Map[Symbol, CompNode] = symbolDefMap.keys
        .map { tyName =>
          tyName -> pc.getVar(typePath / 'typeInit)(
            numsca.randn(1, typeDim) * 0.01
          )
        }
        .toMap
        .updated(AnyType.id, anyInit)

      for (_ <- 0 until iterations) {
        val newTypeMap = symbolDefMap.map {
          case (tyName, typeDef) =>
            val typeFactory = LayerFactory(typePath, pc)
            import typeFactory._

            val aggregated = typeDef match {
              case FuncDef(argTypes, returnType) =>
                val argsEncoding = argTypes
                  .map(typeMap)
                  .foldLeft(funcArgInit: CompNode)(gru('FuncArgsGru))
                val x = argsEncoding.concat(typeMap(returnType), axis = 1)
                activation(linear('FuncDefLinear, nOut = fieldDim)(x))
              case ObjectDef(fields) =>
                val fieldEncodings = fields.map {
                  case (fieldName, fieldType) =>
                    val x = getFieldLabel(fieldName)
                      .concat(typeMap(fieldType), axis = 1)
                    activation(linear('ContainsFieldLinear, nOut = fieldDim)(x))
                }.toVector
                fieldAggregation match {
                  case FieldAggregation.Sum =>
                    if (fieldEncodings.nonEmpty) total(fieldEncodings)
                    else fieldZeroInit
                  case FieldAggregation.Attention =>
                    // see the paper 'Graph Attention Networks'
                    if (fieldEncodings.nonEmpty) {
                      val transformedThis = typeMap(tyName) dot attentionKernel
                      val transformedFields = fieldEncodings.map { _ dot attentionKernel }
                      val attentionLogits = transformedFields.map { ft =>
                        val w = transformedThis
                          .concat(ft, axis = 1)
                          .dot(attentionVec)
                        assert(w.shape.product == 1)
                        leakyRelu(w, 0.2)
                      }
                      val aWeights = softmax(concatN(attentionLogits, axis = 1))
                      total((0 until aWeights.shape(1)).map { i =>
                        aWeights.slice(0, i :> (i + 1)) * transformedFields(i)
                      })
                    } else fieldZeroInit
                }
            }

            tyName -> gru('UpdateGRU)(
              state = typeMap(tyName),
              input = aggregated
            )
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

      val layer1 = activation(
        linear('linear1, nOut = typeDim)(t1.concat(t2, axis = 1))
      )
      val layer2 = activation(linear('linear2, nOut = typeDim / 2)(layer1))
      activation(linear('linear3, nOut = 2)(layer2))

      //      linear('linear1, nOut = 2)((t1 * t2).concat(t1, axis = 1))
    }

  }

  def exampleToGroundTruths(typeDefs: Map[Symbol, TypeDef]) = {
    val typeContext = typeDefsToContext(typeDefs)
    val types = typeContext.typeUnfold.keys.toList
    import GroundType.symbolToType

    val groundTruths = for (t1 <- types; t2 <- types if t1 != t2)
      yield {
        (t1, t2) -> typeContext.isSubType(symbolToType(t1), symbolToType(t2))
      }

    val posExamples = groundTruths.filter(_._2).map(_._1)
    val negExamples = groundTruths.filterNot(_._2).map(_._1)
    posExamples -> negExamples
  }

  object Examples {

    import GType.API._

    import JSExamples._

    val basicTypes: Map[Symbol, CompoundType] = Map(
      boolean -> obj('BoolPlaceholder -> boolean),
      void -> obj('VoidPlaceholder -> void),
      number -> obj(
        'OP_Plus -> (List(number) -: number),
        'OP_Minus -> (List(number) -: number),
        'OP_Times -> (List(number) -: number),
        'OP_Divide -> (List(number) -: number),
        'OP_LessThan -> (List(number) -: boolean),
      ),
      string -> obj(
        'OP_Plus -> (List(any) -: string),
        'charAt -> (List(number) -: string),
        'length -> number
      ),
      'Comparator -> obj('equal -> (List(any, any) -: boolean))
    ) ++ Seq[GroundType](boolean, number, string, any).map(mkArrayType).toMap

    val pointExample = {
      val typeUnfold = basicTypes ++
        Map[Symbol, CompoundType](
          'point -> obj('x -> 'number, 'moveX -> (List('number) -: 'point)),
          'point2D -> obj(
            'x -> 'number,
            'y -> 'number,
            'moveX -> (List('number) -: 'point2D),
            'moveY -> (List('number) -: 'point2D)
          ),
          'stringPoint -> obj(
            'x -> 'string,
            'moveX -> (List('number) -: 'stringPoint)
          ),
          'A -> obj('a -> 'B),
          'B -> obj('a -> 'A),
          'C -> obj('c -> 'A)
        )
      TypeContext(Set(), typeUnfold, Set())
    }
  }

  def main(args: Array[String]): Unit = {

    implicit val random: Random = new Random()

    val encoder = new TypeEncoder(EncoderParams.small)

    val posVec = Tensor(1, 0).reshape(1, -1)
    val negVec = Tensor(0, 1).reshape(1, -1)

    def forwardPredict(symbolDefMap: Map[Symbol, TypeDef],
                       posRelations: Seq[(Symbol, Symbol)],
                       negRelations: Seq[(Symbol, Symbol)]) = {
      val relationNum = math.min(posRelations.length, negRelations.length)
      val posExamples = random.shuffle(posRelations).take(relationNum)
      val negExamples = random.shuffle(negRelations).take(relationNum)

      val target = numsca.concatenate(posExamples.map { _ =>
        posVec
      }
        ++ negExamples.map { _ =>
          negVec
        }, axis = 0)

      val typeEncoding = encoder.encode(symbolDefMap, iterations = 5)
      val predictions =
        encoder.subtypePredict(typeEncoding, posExamples ++ negExamples)
      (target, predictions)
    }

    val trainingSet = List("JSExamples" -> JSExamples.typeContext)

    val devSet = {
      val typeDefs = typeContextToConstraints(Examples.pointExample)
      val typeContext = typeDefsToContext(typeDefs)
      val rels = Vector(
        'point -> 'point2D,
        'point2D -> 'point,
        'point -> 'point,
        'A -> 'point,
        'A -> 'B,
        'B -> 'A,
        'A -> 'C,
        'C -> 'B,
        'C -> 'C,
        'stringPoint -> 'point,
        'stringPoint -> 'point2D
      )
      typeDefs -> rels.map {
        case (l, r) =>
          (l, r) ->
            typeContext.isSubType(
              GroundType.symbolToType(l),
              GroundType.symbolToType(r)
            )
      }
    }

    val preComputes = trainingSet.map {
      case (name, context) =>
        val symbolDefMap = typeContextToConstraints(context)
        symbolDefMap.foreach(println)
        val (posRelations, negRelations) = exampleToGroundTruths(symbolDefMap)
        println(
          s"pos relations: ${posRelations.length}, neg relations: ${negRelations.length}"
        )
        (name, symbolDefMap, (posRelations, negRelations))
    }

    val optimizer = Adam(learningRate = 0.005)

    for (step <- 0 until 1000;
         (exampleName, symbolDefMap, (posRelations, negRelations)) <- preComputes) {

      val (target, predictions) =
        forwardPredict(symbolDefMap, posRelations, negRelations)
      val loss = crossEntropyOnSoftmax(predictions, target)

      println(s"[$exampleName][$step] average loss: ${mean(loss).value}")
      val (correct, wrong) =
        correctWrongSets(predictions.value, target(:>, 1).data.map(_.toInt))
      println(
        s"[$exampleName] accuracy = ${correct.size.toDouble / (correct.size + wrong.size)}"
      )

      if (step % 10 == 0) {
        val posDev = devSet._2.collect { case (r, b) if b  => r }
        val negDev = devSet._2.collect { case (r, b) if !b => r }
        val (target, pred) = forwardPredict(devSet._1, posDev, negDev)
        val (correct, wrong) =
          correctWrongSets(pred.value, target(:>, 1).data.map(_.toInt))
        val accuracy = correct.size.toDouble / (correct.size + wrong.size)
        println("dev set accuracy = " + accuracy)
        println("=== correct ===")
        correct.foreach(i => println(devSet._2(i)))
        println("=== wrong ===")
        wrong.foreach(i => println(devSet._2(i)))
      }

      optimizer.minimize(loss, encoder.pc.allParams, weightDecay = Some(1e-4))
    }
  }
}
