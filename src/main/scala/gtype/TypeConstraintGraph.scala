package gtype

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

  case class IsSubType(tyName: Symbol) extends TypeConstraint

  case class IsParentType(tyName: Symbol) extends TypeConstraint

  sealed trait FieldConstraint

  case class AppearAsField(objTypeName: Symbol, fieldType: Symbol) extends FieldConstraint

  def contextToConstraints(typeDefs: Map[Symbol, TypeDef]):
    (Map[Symbol, List[TypeConstraint]], Map[Symbol, List[FieldConstraint]]) = {

    import collection.mutable
    val tConstraints = mutable.HashMap[Symbol, List[TypeConstraint]]()
    val fConstraints = mutable.HashMap[Symbol, List[FieldConstraint]]()

    def addConstraint[T](map: mutable.HashMap[Symbol, List[T]], symbol: Symbol)(x: T) = {
      map(symbol) = x :: map.getOrElseUpdate(symbol, List())
    }

    typeDefs.foreach{ case (tyName, typeDef) =>
      typeDef match {
        case fd: FuncDef =>
          addConstraint(tConstraints, tyName) { fd }
        case ObjectDef(fields) =>
          fields.foreach{ case (field, ty) =>
            addConstraint(tConstraints, tyName){ ContainsField(field, ty) }
            addConstraint(fConstraints, field) { AppearAsField(tyName, ty) }
          }
      }
    }

    tConstraints.toMap -> fConstraints.toMap
  }
}
