package gtype

sealed trait TypeCheckError {

}

case class SubtypeError(child: GType, parent: GType) extends TypeCheckError

case class ApplyError(expr: GExpr, inferredType: GType) extends TypeCheckError

case class AccessError(expr: GExpr, field: Symbol, inferredType: GType) extends TypeCheckError

