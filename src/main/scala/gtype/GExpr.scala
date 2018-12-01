package gtype

// === Expression definitions ===

/**
  *  e :=                         ([[gtype.GExpr]])
  *     | x                       ([[gtype.Var]])
  *     | c                       ([[gtype.Const]])
  *     | (x: t) => e             ([[gtype.Lambda]])
  *     | e e                     ([[gtype.Apply]])
  *     | e[t]                    ([[gtype.Cast]])
  *     | { l: e, ..., l: e }     ([[gtype.Constructor]])
  *     | e.l                     ([[gtype.Access]])
  *     | e.l = e                 ([[gtype.Update]])
  *     | let (x: t) = e in e     ([[gtype.LetBind]])
  *     | if e then e else e      ([[gtype.IfExpr]])
  *
  *  where l is [[String]] and t is [[gtype.GType]]
  */
sealed trait GExpr {

}

case class Var(name: String) extends GExpr

case class Const(value: Any, ty: GType) extends GExpr

case class Lambda(arg: (String, GType), expr: GExpr) extends GExpr

case class Apply(f: GExpr, x: GExpr) extends GExpr

case class Cast(expr: GExpr, ty: GType) extends GExpr

case class Constructor(fields: Map[String, GExpr]) extends GExpr

case class Access(expr: GExpr, field: String) extends GExpr

case class Update(expr: GExpr, field: String, v: GExpr) extends GExpr

case class LetBind(bind: (String, GType), expr: GExpr) extends GExpr

case class IfExpr(cond: GExpr, e1: GExpr, e2: GExpr) extends GExpr

// === End of Expression definitions ===

object GExpr {

}