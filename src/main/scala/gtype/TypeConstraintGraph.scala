package gtype

import funcdiff.{CompNode, LayerFactory, ParamNode}

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

  //  case class IsSubType(tyName: Symbol) extends TypeConstraint

  //  case class IsParentType(tyName: Symbol) extends TypeConstraint

  sealed trait FieldConstraint

  case class AppearAsField(objTypeName: Symbol, fieldType: Symbol) extends FieldConstraint

  def typeDefsToContext(typeDefs: Map[Symbol, TypeDef]): TypeContext = {
    val typeUnfold = ???

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
                          typeMap: Map[Symbol,CompNode])

  class ConstraintsEncoder(encoderParams: EncoderParams
                          ) {

    import encoderParams._

    val pc = ParamCollection()
    val labelPath: SymbolPath = SymbolPath.empty / 'label
    val fieldPath: SymbolPath = SymbolPath.empty / 'field
    val typePath: SymbolPath = SymbolPath.empty / 'type


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

          val aggregated = total(constraints.map {
            case ContainsField(fieldName, fieldType) =>
              val x = labelMap(fieldName).concat(fieldMap(fieldName), axis = 1).concat(typeMap(fieldType), axis = 1)
              linear('ContainsFieldLinear, nOut = constraintDim)(x)
            case FuncDef(argType, returnType) =>
              val x = typeMap(argType).concat(typeMap(returnType), axis = 1)
              linear('FuncDefLinear, nOut = constraintDim)(x)
          }.toIndexedSeq)

          tyName -> gru('UpdateGRU)(state = typeMap(tyName), input = aggregated)
        }

        val newFieldMap = fieldConstraints.map { case (fieldName, constraints) =>
          val fieldFactory = LayerFactory(fieldPath, pc)
          import fieldFactory._

          val aggregated = total(constraints.map{
            case AppearAsField(objTypeName, fieldType) =>
              val x = typeMap(objTypeName).concat(typeMap(fieldType), axis = 1)
              linear('AppearsInLinear, nOut = constraintDim)(x)
          }.toIndexedSeq)

          fieldName -> gru('UpdateGRU)(state = fieldMap(fieldName), input = aggregated)
        }

        typeMap = newTypeMap
        fieldMap = newFieldMap
      }
      TypeEncoding(labelMap, fieldMap, typeMap)
    }

  }

  object Examples {
    val pointExample = {
      val typeDefs = Map[Symbol, TypeDef](
        'number -> ObjectDef(Map(
          'number -> 'number
        )),
        'point -> ObjectDef(Map(
          'x -> 'number,
          'moveX -> 'moveXType1
        )),
        'moveXType1 -> FuncDef('number, 'point),
        'point2D -> ObjectDef(Map(
          'x -> 'number,
          'y -> 'number,
          'moveX -> 'moveXType2,
          'moveY -> 'moveXType2
        )),
        'moveXType2 -> FuncDef('number, 'point2D),
        'string -> ObjectDef(Map(
          'string -> 'string
        )),
        'stringPoint -> ObjectDef(Map(
          'x -> 'string,
          'moveX -> 'moveXType1
        ))
      )
    }
  }
}
