package gtype

import botkop.numsca.Tensor
import funcdiff.Optimizers.Adam
import funcdiff.{CompNode, LayerFactory, ParamNode}

import scala.util.Random

/**
  * Turn type definitions into a constraint graph over literals
  */
object TypeConstraintGraph {

  val anySymbol = 'ANY

  sealed trait TypeDef

  case class FuncDef(argType: Symbol, returnType: Symbol) extends TypeDef with TypeConstraint

  case class ObjectDef(fields: Map[Symbol, Symbol]) extends TypeDef


  sealed trait TypeConstraint

  case class ContainsField(fieldName: Symbol, fieldType: Symbol) extends TypeConstraint

  case object IsObject extends TypeConstraint

  //  case class IsSubType(tyName: Symbol) extends TypeConstraint

  //  case class IsParentType(tyName: Symbol) extends TypeConstraint

  sealed trait FieldConstraint

  case class AppearAsField(objTypeName: Symbol, fieldType: Symbol) extends FieldConstraint

  def typeDefsToContext(typeDefs: Map[Symbol, TypeDef]): TypeContext = {
    import GType.API._
    val typeUnfold = typeDefs.mapValues {
      case ObjectDef(fields) => ObjectType(fields.mapValues(symbol2TyVar))
      case FuncDef(argType, returnType) => List(argType) -: returnType
    }

    TypeContext(baseTypes = Set(), typeUnfold = typeUnfold, subRel = Set())
  }

  def typeDefsToConstraints(typeDefs: Map[Symbol, TypeDef]):
  (Map[Symbol, List[TypeConstraint]], Map[Symbol, List[FieldConstraint]]) = {

    import collection.mutable
    val tConstraints = mutable.HashMap[Symbol, List[TypeConstraint]]()
    val fConstraints = mutable.HashMap[Symbol, List[FieldConstraint]]()

    def addConstraint[T](map: mutable.HashMap[Symbol, List[T]], symbol: Symbol)(x: T) = {
      map(symbol) = x :: map.getOrElseUpdate(symbol, List())
    }

    typeDefs.foreach { case (tyName, typeDef) =>
      typeDef match {
        case fd: FuncDef =>
          addConstraint(tConstraints, tyName) {
            fd
          }
        case ObjectDef(fields) =>
          addConstraint(tConstraints, tyName) { IsObject }
          fields.foreach { case (field, ty) =>
            addConstraint(tConstraints, tyName) {
              ContainsField(field, ty)
            }
            addConstraint(fConstraints, field) {
              AppearAsField(tyName, ty)
            }
          }
      }
    }

    tConstraints.toMap -> fConstraints.toMap
  }

  def typeContextToConstraints(context: TypeContext) = {

  }

  case class EncoderParams(labelDim: Int, fieldDim: Int, typeDim: Int, constraintDim: Int)

  object EncoderParams {
    val small = EncoderParams(labelDim = 20, fieldDim = 30, typeDim = 40,
      constraintDim = 40)
  }

  import funcdiff.API._
  import funcdiff.{ParamCollection, SymbolPath, TensorExtension}
  import botkop.numsca

  case class TypeEncoding(labelMap: Map[Symbol, CompNode],
                          fieldMap: Map[Symbol, CompNode],
                          typeMap: Map[Symbol, CompNode])

  class TypeEncoder(encoderParams: EncoderParams
                   ) {

    import encoderParams._

    val pc = ParamCollection()
    val labelPath: SymbolPath = SymbolPath.empty / 'label
    val fieldPath: SymbolPath = SymbolPath.empty / 'field
    val typePath: SymbolPath = SymbolPath.empty / 'type
    val subtypePath: SymbolPath = SymbolPath.empty / 'subtype


    def encode(typeConstraints: Map[Symbol, List[TypeConstraint]],
               fieldConstraints: Map[Symbol, List[FieldConstraint]],
               iterations: Int): TypeEncoding = {
      val labelMap = fieldConstraints.keys.map { field =>
        field -> const(TensorExtension.randomUnitVec(labelDim)).withShape(1, labelDim)
      }.toMap

      var fieldMap: Map[Symbol, CompNode] = labelMap.mapValues { _ =>
        pc.getVar(fieldPath / 'fieldInit)(
          numsca.randn(1, fieldDim) * 0.01
        )
      }

      var typeMap: Map[Symbol, CompNode] = typeConstraints.keys.map { tyName =>
        tyName -> pc.getVar(typePath / 'typeInit)(
          numsca.randn(1, typeDim) * 0.01
        )
      }.toMap

      for (_ <- 0 until iterations) {
        val newTypeMap = typeConstraints.map { case (tyName, constraints) =>
          val typeFactory = LayerFactory(typePath, pc)
          import typeFactory._

          val constraintInit = pc.getVar(typePath / 'constraintInit)(
            numsca.rand(1, constraintDim) * 0.01
          )
          val aggregated = total(constraints.filterNot(_.isInstanceOf[IsObject.type]).map {
            case ContainsField(fieldName, fieldType) =>
              val x = labelMap(fieldName).concat(fieldMap(fieldName), axis = 1).concat(typeMap(fieldType), axis = 1)
              relu(linear('ContainsFieldLinear, nOut = constraintDim)(x))
            case FuncDef(argType, returnType) =>
              val x = typeMap(argType).concat(typeMap(returnType), axis = 1)
              relu(linear('FuncDefLinear, nOut = constraintDim)(x))
          }.toIndexedSeq :+ constraintInit)

          tyName -> gru('UpdateGRU)(state = typeMap(tyName), input = aggregated)
        }

        val newFieldMap = fieldConstraints.map { case (fieldName, constraints) =>
          val fieldFactory = LayerFactory(fieldPath, pc)
          import fieldFactory._

          val aggregated = total(constraints.map {
            case AppearAsField(objTypeName, fieldType) =>
              val x = typeMap(objTypeName).concat(typeMap(fieldType), axis = 1)
              relu(linear('AppearsInLinear, nOut = constraintDim)(x))
          }.toIndexedSeq)

          fieldName -> gru('UpdateGRU)(state = fieldMap(fieldName), input = aggregated)
        }

        typeMap = newTypeMap
        fieldMap = newFieldMap
      }
      TypeEncoding(labelMap, fieldMap, typeMap)
    }

    def subtypePredict(encoding: TypeEncoding, typePairs: Seq[(Symbol, Symbol)]): CompNode = {
      val (t1s, t2s) = typePairs.unzip
      val t1 = concatN(t1s.map(encoding.typeMap).toVector, axis = 0)
      val t2 = concatN(t2s.map(encoding.typeMap).toVector, axis = 0)


      val modelFactory = LayerFactory(subtypePath, pc)
      import modelFactory._

      //      val layer1 = relu(linear('linear1, nOut = typeDim / 2)(t1.concat(t2, axis = 1)))
      //      relu(linear('linear2, nOut = 2)(layer1))

      linear('linear1, nOut = 2)((t1 * t2).concat(t1, axis=1))
    }

  }

  def exampleToGroundTruths(typeDefs: Map[Symbol, TypeDef]) = {
    val typeContext = typeDefsToContext(typeDefs)
    val types = typeContext.typeUnfold.keys.toList
    val groundTruths = for (t1 <- types; t2 <- types if t1 != t2)
      yield {
        (t1, t2) -> typeContext.isSubType(TyVar(t1), TyVar(t2))
      }

    val posExamples = groundTruths.filter(_._2).map(_._1)
    val negExamples = groundTruths.filterNot(_._2).map(_._1)
    posExamples -> negExamples
  }

  object Examples {

    def objDef(pairs: (Symbol, Symbol)*) = ObjectDef(Map(pairs:_*))

    val basicTypes = Map[Symbol, TypeDef](
      'number -> objDef(
        'number -> 'number,
        'plus -> 'numberMethod
      ),
      'numberMethod -> FuncDef('number, 'number),
      'string -> objDef(
        'string -> 'string
      ),
      'bool -> objDef(
        'bool -> 'bool
      ),
      'string2bool -> FuncDef('string, 'bool),
      'object -> objDef()
    )

    val pointExample = {
      basicTypes ++
        Map[Symbol, TypeDef](
          'point -> objDef(
            'x -> 'number,
            'moveX -> 'moveXType1
          ),
          'moveXType1 -> FuncDef('number, 'point),
          'point2D -> objDef(
            'x -> 'number,
            'y -> 'number,
            'moveX -> 'moveXType2,
            'moveY -> 'moveXType2
          ),
          'moveXType2 -> FuncDef('number, 'point2D),
          'stringPoint -> objDef(
            'x -> 'string,
            'moveX -> 'moveXType1
          )
        )
    }

    val largerExample = {
      basicTypes ++ Map[Symbol, TypeDef](
        'BloomFilter -> objDef(
          'mayContain -> 'string2bool,
          'createStore -> 'number2object,
          'hash1 -> 'string2number
        ),
        'number2object -> FuncDef('number, 'object),
        'string2number -> FuncDef('string, 'number),
//        'BinarySearchTree -> objDef(
//          'insert -> Symbol("BinarySearch.insert")
//        ),
//        Symbol("BinarySearch.insert") -> FuncDef('any, 'BinarySearchTreeNode)
      )
    }

    val all: IndexedSeq[Map[Symbol, TypeDef]] = IndexedSeq(pointExample, largerExample)
  }

  def main(args: Array[String]): Unit = {
    implicit val random: Random = new Random()

    val encoder = new TypeEncoder(EncoderParams.small)

    val examples = Examples.all
    val preComputes = examples.map{ typeDefs =>
      val (tConstraints, fConstraints) = typeDefsToConstraints(typeDefs)
      val (posRelations, negRelations) = exampleToGroundTruths(typeDefs)
      (tConstraints, fConstraints) -> (posRelations, negRelations)
    }

    val posVec = Tensor(1, 0).reshape(1, -1)
    val negVec = Tensor(0, 1).reshape(1, -1)

    val optimizer = Adam(learningRate = 0.001)


    for (step <- 0 until 1000;
         ((tConstraints, fConstraints), (posRelations, negRelations)) <- preComputes) {

      val relationNum = math.min(posRelations.length, negRelations.length)
      val posExamples = random.shuffle(posRelations).take(relationNum)
      val negExamples = random.shuffle(negRelations).take(relationNum)

      val target = numsca.concatenate(posExamples.map { _ => posVec }
        ++ negExamples.map { _ => negVec }, axis = 0)
      if (step == 0) {
        println(s"example num: ${posExamples.length * 2}")
      }

      val typeEncoding = encoder.encode(tConstraints, fConstraints, iterations = 5)
      val predictions = encoder.subtypePredict(typeEncoding, posExamples ++ negExamples)
      val loss = crossEntropyOnSoftmax(predictions, target)

      println(s"[$step] average loss: ${mean(loss).value}")
      println(s"accuracy = ${accuracy(predictions.value, target(:>, 1).data.map(_.toInt))}")

      optimizer.minimize(loss, encoder.pc.allParams, weightDecay = Some(1e-4))
    }
  }
}
