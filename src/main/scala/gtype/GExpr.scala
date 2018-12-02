package gtype

import scala.language.implicitConversions

// === Expression definitions ===

/**
  *  e :=                         ([[GExpr]])
  *     | x                       ([[Var]])
  *     | c                       ([[Const]])
  *     | (x: α) => e             ([[Lambda]])
  *     | e e                     ([[Apply]])
  *     | e[t]                    ([[Cast]])
  *     | { l: e, ..., l: e }     ([[Constructor]])
  *     | e.l                     ([[Access]])
  *     | let (x: α) = e in e     ([[LetBind]])
  *     | if[α] e then e else e      ([[IfExpr]])
  *
  *  where l is [[Symbol]], t is [[GType]], and α is [[GTMark]]
  */
sealed trait GExpr {
  def call(e0: GExpr, es: GExpr*): Apply = {
    es.foldLeft(Apply(this, e0)){ case (a, e) => Apply(a,e) }
  }

  def cast(ty: GType): Cast = Cast(this, ty)

  def m(field: Symbol) = Access(this, field)
}

case class Var(name: Symbol) extends GExpr

case class Const(value: Any, ty: GType) extends GExpr

case class Lambda(arg: (Symbol, GTMark), expr: GExpr) extends GExpr

case class Apply(f: GExpr, x: GExpr) extends GExpr

case class Cast(expr: GExpr, ty: GType) extends GExpr

case class Constructor(fields: Map[Symbol, GExpr]) extends GExpr

case class Access(expr: GExpr, field: Symbol) extends GExpr

case class LetBind(bind: (Symbol, GTMark, GExpr), expr: GExpr) extends GExpr

case class IfExpr(cond: GExpr, e1: GExpr, e2: GExpr, resultType: GTMark) extends GExpr

// === End of Expression definitions ===

/**
  * A context used for type checking expressions
  */
case class ExprContext(varAssign: Map[Symbol, GType], typeContext: TypeContext) {
  def newVar(arg: Symbol, argT: GType): ExprContext = {
    copy(varAssign = varAssign.updated(arg, argT))
  }

}


object GExpr {

  val boolType = BaseType(Symbol("𝔹"))

  object API {
    implicit def symbol2Var(name: Symbol): Var = Var(name)

    def mkObj(fields: (Symbol, GExpr)*) = Constructor(fields.toMap)

    def C(v: Any, ty: GType) = Const(v, ty)

    def FUN(arg: Symbol, ty: GTMark)(body: GExpr): Lambda = {
      Lambda(arg->ty, body)
    }

    case class LETBuild(arg: Symbol, ty: GTMark, expr: GExpr){
      def IN(body: GExpr): LetBind = {
        LetBind((arg, ty, expr), body)
      }
    }

    def LET(arg: Symbol, ty: GTMark)(expr: GExpr): LETBuild = {
      LETBuild(arg, ty, expr)
    }

    case class IFBuild(b: GExpr, resultType: GType, e1: GExpr){
      def ELSE(e2: GExpr) = IfExpr(b, e1, e2, resultType)
    }

    def IF(b: GExpr, resultType: GType)(e1: GExpr) = IFBuild(b, resultType, e1)
  }

  /**
    * Check whether an expression is well-typed and infer a most precise type. Assumes the expression
    * contains no [[GTHole]]s.
    *
    * @return a type and a set of [[TypeCheckError]].
    */
  def typeCheckInfer(expr: GExpr, context: ExprContext): (GType, Set[TypeCheckError]) = {
    import context._

    expr match {
      case Var(name) => varAssign(name) -> Set()
      case c: Const => c.ty -> Set()
      case Lambda((arg, argT: GType), body) =>
        val (t, errors) = typeCheckInfer(body, context.newVar(arg, argT))
        argT.arrow(t) -> errors
      case Apply(f, x) =>
        val (fT, e1) = typeCheckInfer(f, context)
        val (xT, e2) = typeCheckInfer(x, context)
        fT match {
          case AnyType => AnyType -> (e1 ++ e2)
          case FuncType(from, to) =>
            val e3 = typeContext.mkSubTypeError(xT, from).toSet
            to -> (e1 ++ e2 ++ e3)
          case _ =>
            val e3 = ApplyError(f, fT)
            AnyType -> (e1 ++ e2 + e3)
        }
      case Cast(e, t) =>
        val (et, errors) = typeCheckInfer(e, context)
        val e1 = typeContext.mkSubTypeError(et, t).toSet
        t -> (errors ++ e1)
      case Constructor(fields) =>
        var errors = Set[TypeCheckError]()
        val fieldTypes = fields.map{ case (name, e) =>
            val (et, err) = typeCheckInfer(e, context)
            errors ++= err
            name -> et
        }
        ObjectType(fieldTypes) -> errors
      case Access(e, field) =>
        val (et, errors) = typeCheckInfer(e, context)
        val et1 = et match {
          case TyVar(id) => typeContext.typeUnfold(id)
          case _ => et
        }
        et1 match {
          case AnyType => AnyType -> errors
          case ObjectType(fields) if fields.contains(field) =>
            fields(field) -> errors
          case _ =>
            AnyType -> (errors + AccessError(e, field, et))
        }
      case LetBind((arg, argT: GType, e1), body) =>
        val newContext = context.newVar(arg, argT)
        val (e1T, errors1) = typeCheckInfer(e1, newContext)
        val (t, errors2) = typeCheckInfer(body, newContext)
        t -> (errors1 ++ errors2 ++ typeContext.mkSubTypeError(e1T, argT).toSet)
      case IfExpr(cond, e1, e2, resultType: GType) =>
        val (condT, errs0) = typeCheckInfer(cond, context)
        val (e1T, errs1) = typeCheckInfer(e1, context)
        val (e2T, errs2) = typeCheckInfer(e2, context)
        val allErrors = errs0 ++ errs1 ++ errs2 ++
          typeContext.mkSubTypeError(condT, boolType).toSet ++
          typeContext.mkSubTypeError(e1T, resultType).toSet ++
          typeContext.mkSubTypeError(e2T, resultType).toSet
        resultType -> allErrors
    }
  }
}