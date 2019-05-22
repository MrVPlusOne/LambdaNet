package lambdanet.surface

import lambdanet.types._
import lambdanet.types.GType._

import scala.language.implicitConversions

// === Expression definitions ===

// @formatter:off
/**
  *  e :=                         ([[GExpr]])
  *     | x                       ([[Var]])
  *     | c                       ([[Const]])
  *     | e(e,...,e)              ([[FuncCall]])
  *     | e as t                  ([[Cast]])
  *     | { l: e, ..., l: e }     ([[ObjLiteral]])
  *     | e.l                     ([[Access]])
  *     | if[α] e then e else e   ([[IfExpr]])
  *
  *  where x, l are [[Symbol]],
  *        t is [[GType]],
  *        α is [[GTMark]]
  */
// @formatter:on
sealed trait GExpr {
  def call(args: GExpr*): FuncCall = {
    FuncCall(this, args.toList)
  }

  def cast(ty: GType): Cast = Cast(this, ty)

  def m(field: Symbol) = Access(this, field)

  def :=(expr: GExpr) = AssignStmt(this, expr)

  def prettyPrint: String = GExpr.prettyPrint(this)

  override def toString: String = prettyPrint
}

case class Var(name: Symbol) extends GExpr

case class Const(value: Any, ty: GType) extends GExpr

case class FuncCall(f: GExpr, args: List[GExpr]) extends GExpr

case class Cast(expr: GExpr, ty: GType) extends GExpr

case class ObjLiteral(fields: Map[Symbol, GExpr]) extends GExpr

case class Access(expr: GExpr, field: Symbol) extends GExpr

case class IfExpr(cond: GExpr, e1: GExpr, e2: GExpr, resultType: GTMark) extends GExpr

// === End of Expression definitions ===

/**
  * A context used for type checking expressions
  */
case class ExprContext(varAssign: Map[Symbol, GType], typeContext: TypeContext) {
  def newTypeVar(name: Symbol, objectType: ObjectType): ExprContext = {
    copy(typeContext = typeContext.newTypeVar(name, objectType))
  }

  def newVar(arg: Symbol, argT: GType): ExprContext = {
    copy(varAssign = {
      assert(
        !varAssign.contains(arg),
        s"new definition (${arg.name}: $argT) shadows outer definition of type ${varAssign(arg)}"
      )
      varAssign.updated(arg, argT)
    })
  }

}

object GExpr {

  def prettyPrint(expr: GExpr): String = {
    /*
     *  e :=                         ([[GExpr]])
     *     | x                       ([[Var]])
     *     | c                       ([[Const]])
     *     | e(e,...,e)              ([[FuncCall]])
     *     | e as t                  ([[Cast]])
     *     | { l: e, ..., l: e }     ([[ObjLiteral]])
     *     | e.l                     ([[Access]])
     *     | if[α] e then e else e   ([[IfExpr]])
     */
    expr match {
      case Var(s)           => s.name
      case Const(value, ty) => s"($value: $ty)"
      case Cast(expr, ty)   => ???
      case FuncCall(f, args) =>
        s"$f${args.mkString("(", ", ", ")")}"
      case ObjLiteral(fields) =>
        fields.map { case (f, v) => s"$f: $v" }.mkString("{", ", ", "}")
      case Access(receiver, label) =>
        s"$receiver.${label.name}"
      case IfExpr(cond, e1, e2, _) =>
        s"($cond ? $e1 : $e2)"
    }
  }

  trait GExprAPI extends GTypeAPI {
    implicit def symbol2Var(name: Symbol): Var = Var(name)

    def mkObj(fields: (Symbol, GExpr)*) = ObjLiteral(fields.toMap)

    def C(v: Any, ty: GType) = Const(v, ty)

    def I(i: Int) = Const(i, 'int)

    def B(b: Boolean) = Const(b, 'bool)

    def N(n: Double) = Const(n, 'number)

    val undefined = Const("undefined", any)

    def NEW(name: Symbol)(args: GExpr*): GExpr =
      FuncCall(GStmt.constructorName, args.toList)
  }

  object API extends GExprAPI

  /**
    * Check whether an expression is well-typed and infer a most precise type. Assumes the expression
    * contains no [[GTHole]]s.
    *
    * @return a type and a set of [[lambdanet.types.GType.TypeCheckError]].
    */
  def typeCheckInfer(expr: GExpr, context: ExprContext): (GType, Set[TypeCheckError]) = {
    import context._

    expr match {
      case Var(name) => varAssign(name) -> Set()
      case c: Const  => c.ty -> Set()
      case FuncCall(f, args) =>
        val (fT, e1) = typeCheckInfer(f, context)
        val (xTs, e2s) = args.map { x =>
          typeCheckInfer(x, context)
        }.unzip
        val e2 = e2s.fold(Set[TypeCheckError]()) {
          _ ++ _
        }
        fT match {
          case AnyType            => AnyType -> (e1 ++ e2)
          case FuncType(from, to) =>
            // allow some arguments to be omitted, as in Typescript
            val e3 = xTs.zip(from).foldLeft(Set[TypeCheckError]()) {
              case (errors, (cT, pT)) =>
                errors ++ typeContext.mkSubtypeError(cT, pT)
            }
            to -> (e1 ++ e2 ++ e3)
          case _ =>
            val e3 = ApplyError(f, fT)
            AnyType -> (e1 ++ e2 + e3)
        }
      case Cast(e, t) =>
        val (et, errors) = typeCheckInfer(e, context)
        val e1 = typeContext.mkSubtypeError(et, t)
        t -> (errors ++ e1)
      case ObjLiteral(fields) =>
        var errors = Set[TypeCheckError]()
        val fieldTypes = fields.map {
          case (name, e) =>
            val (et, err) = typeCheckInfer(e, context)
            errors ++= err
            name -> et
        }
        ObjectType(fieldTypes) -> errors
      case Access(e, field) =>
        val (et, errors) = typeCheckInfer(e, context)
        val et1 = et match {
          case TyVar(id) => typeContext.typeUnfold(id)
          case _         => et
        }
        et1 match {
          case AnyType => AnyType -> errors
          case ObjectType(fields) if fields.contains(field) =>
            fields(field) -> errors
          case _ =>
            AnyType -> (errors + AccessError(e, field, et))
        }
      case IfExpr(cond, e1, e2, resultType: GType) =>
        val (condT, errs0) = typeCheckInfer(cond, context)
        val (e1T, errs1) = typeCheckInfer(e1, context)
        val (e2T, errs2) = typeCheckInfer(e2, context)
        val allErrors = errs0 ++ errs1 ++ errs2 ++
          typeContext.mkSubtypeError(condT, GType.boolType) ++
          typeContext.mkSubtypeError(e1T, resultType) ++
          typeContext.mkSubtypeError(e2T, resultType)
        resultType -> allErrors
      case _ => throw new NotImplementedError("Expressions with GTHoles not supported.")
    }
  }
}
