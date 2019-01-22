package infer

import gtype.{GTHole, GType, JSExamples}
import funcdiff.SimpleMath.Extensions._

/**
  * An intermediate program representation useful for type inference
  */
object IR {

  // @formatter:off
  /** a simple expression
    *
    *  e :=                         ([[IRExpr]])
    *     | x                       ([[Var]])
    *     | c: t                    ([[Const]])
    *     | x(x,...,x)              ([[FuncCall]])
    *     | { l: x, ..., l: x }     ([[ObjLiteral]])
    *     | e.l                     ([[FieldAccess]])
    *     | if x then x else x      ([[IfExpr]])
    *
    *  where l is [[Symbol]],
    *        t is [[GType]]
    *
    * */
  // @formatter:on
  sealed trait IRExpr {
    def prettyPrint: String

    override def toString: String = prettyPrint
  }

  case class Var(symbol: Symbol) extends IRExpr {
    def prettyPrint: String = symbol.name
  }
  case class Const(value: Any, ty: GType) extends IRExpr {
    def prettyPrint: String = s"($value: $ty)"
  }
  case class FuncCall(f: Var, args: List[Var]) extends IRExpr {
    def prettyPrint: String = s"$f${args.mkString("(", ", ", ")")}"
  }
  case class ObjLiteral(fields: Map[Symbol, Var]) extends IRExpr {
    def prettyPrint: String =
      fields.map { case (f, v) => s"$f: $v" }.mkString("{", ", ", "}")
  }
  case class FieldAccess(receiver: Var, label: Symbol) extends IRExpr {
    def prettyPrint: String = s"$receiver.${label.name}"
  }
  case class IfExpr(cond: Var, e1: Var, e2: Var) extends IRExpr {
    def prettyPrint: String = s"($cond ? $e1 : $e2)"
  }

  /**
    * T :=                      ([[IRType]])
    *   | v                     ([[VarIRType]])
    *   | t                     ([[ConcreteIRType]])
    *   | (T,..., T) -> T       ([[FuncIRType]])
    *   | {l: T, ..., l: T}     ([[ObjectIRType]])
    *
    *   where t is a concrete type [[GType]].
    */
  sealed trait IRType {
    def replaceLeaves(sub: VarIRType => IRType): IRType = IRType.replaceLeaves(this, sub)
  }

  sealed trait BaseIRType extends IRType

  case class ConcreteIRType(ty: GType) extends BaseIRType

  case class VarIRType(id: Int, origin: Option[GTHole]) extends BaseIRType {
    override def toString: String = s"𝒯$id"
  }

  case class FuncIRType(args: List[IRType], to: IRType) extends IRType

  case class ObjectIRType(fields: Map[Symbol, IRType]) extends IRType

  object IRType {
    def replaceLeaves(ty: IRType, sub: VarIRType => IRType): IRType = ty match {
      case c: ConcreteIRType => c
      case v: VarIRType      => sub(v)
      case FuncIRType(args, to) =>
        FuncIRType(args.map(replaceLeaves(_, sub)), replaceLeaves(to, sub))
      case ObjectIRType(fields) =>
        ObjectIRType(fields.mapValuesNow(t => replaceLeaves(t, sub)))
    }
  }

  case class IRCtx(
    typeMap: Map[Var, IRType],
    returnType: IRType,
    objectDefs: Map[Symbol, Either[ObjectIRType, gtype.ObjectType]]
  )

  object IRCtx {
    val empty: IRCtx = IRCtx(Map(), ConcreteIRType(GType.voidType), Map())
    val jsCtx: IRCtx = {
      val typeMap = JSExamples.exprContext.varAssign.map {
        case (s, t) => Var(s) -> ConcreteIRType(t)
      }
      val objDef = JSExamples.typeContext.typeUnfold
        .mapValuesNow(x => Right(x.asInstanceOf[gtype.ObjectType]))
      IRCtx(typeMap, ConcreteIRType(GType.voidType), objDef)
    }
  }

  // @formatter:off
  /**
    *
    * a statement in Single Assignment Form
    *
    * S :=                                  ([[IRStmt]])
    *   | var x: τ = e                      ([[VarDef]])
    *   | x := x                            ([[Assign]])
    *   | [return] x                        ([[ReturnStmt]])
    *   | if(x) S else S                    ([[IfStmt]])
    *   | while(x) S                        ([[WhileStmt]])
    *   | { S; ...; S }                     ([[BlockStmt]])
    *   | function x (x: α, ..., x:α): α    ([[FuncDef]])
    *   | class x (l: α, ..., l:α)          ([[ClassDef]])
    *     ↳ [extends x]{ f, ..., f }
    *
    * where x is [[Var]]
    *       l is [[Symbol]],
    *       τ is [[IRType]],
    *       e is [[IRExpr]],
    *       f is [[FuncDef]]
    * */
  // @formatter:on
  sealed trait IRStmt {
    def prettyPrint(indentSpaces: Int = 2): String = {
      IRStmt
        .prettyPrintHelper(0, this)
        .map {
          case (indent, text) => " " * (indent * indentSpaces) + text
        }
        .mkString("\n")
    }

    override def toString: String = prettyPrint()
  }

  case class VarDef(v: Var, mark: BaseIRType, rhs: IRExpr) extends IRStmt

  case class Assign(lhs: Var, rhs: Var) extends IRStmt

  case class ReturnStmt(v: Var) extends IRStmt

  case class IfStmt(cond: Var, e1: IRStmt, e2: IRStmt) extends IRStmt

  case class WhileStmt(cond: Var, body: IRStmt) extends IRStmt

  case class BlockStmt(stmts: Vector[IRStmt]) extends IRStmt

  case class FuncDef(
    name: Symbol,
    args: List[(Var, BaseIRType)],
    returnType: BaseIRType,
    body: Vector[IRStmt]
  ) extends IRStmt {
    val funcT = FuncIRType(args.map(_._2), returnType)
  }

  case class ClassDef(
    name: Symbol,
    superType: Option[Symbol] = None,
    constructor: FuncDef,
    vars: Map[Symbol, BaseIRType],
    funcDefs: Vector[FuncDef]
  ) extends IRStmt {
//    require(constructor.funcV == name)
//    require(constructor.returnType == any)
  }

  object ClassDef {
    val thisVar = Var(gtype.ClassDef.thisSymbol)
//    val superVar = Var(gtype.ClassDef.superSymbol)
  }

  object IRStmt {
    def prettyPrintHelper(indent: Int, stmt: IRStmt): Vector[(Int, String)] = {
      stmt match {
        case VarDef(v, ty, rhs) => Vector(indent -> s"let $v: $ty = $rhs;")
        case Assign(lhs, rhs)   => Vector(indent -> s"$lhs = $rhs;")
        case ReturnStmt(v)      => Vector(indent -> s"return $v;")
        case IfStmt(cond, e1, e2) =>
          Vector(indent -> s"if ($cond)") ++
            prettyPrintHelper(indent, e1) ++ Vector(indent -> "else") ++
            prettyPrintHelper(indent, e2)
        case WhileStmt(cond, body) =>
          (indent -> s"while ($cond)") +: prettyPrintHelper(indent, body)
        case BlockStmt(stmts) =>
          (indent -> "{") +: stmts.flatMap(
            s => prettyPrintHelper(indent + 1, s)
          ) :+ (indent -> "}")
        case FuncDef(funcName, args, returnType, body) =>
          val argList = args
            .map { case (v, tv) => s"$v: $tv" }
            .mkString("(", ", ", ")")
          val returnMark =
            if (returnType == ConcreteIRType(GType.voidType)) "" else s": $returnType"
          Vector(indent -> s"function $funcName$argList$returnMark {") ++
            body.flatMap(s => prettyPrintHelper(indent + 1, s)) ++ Vector(
            indent -> "}"
          )
        case ClassDef(name, superType, constructor, vars, funcDefs) =>
          val superPart = superType
            .map(t => s"extends $t")
            .getOrElse("")
          Vector(indent -> s"class $name $superPart {") ++
            vars.toList.map {
              case (fieldName, tv) =>
                (indent + 1, s"${fieldName.name}: $tv;")
            } ++
            (constructor +: funcDefs).flatMap(
              fDef => prettyPrintHelper(indent + 1, fDef)
            ) ++
            Vector(indent -> "}")
      }
    }
  }
  def tryToBlock(stmts: Vector[IRStmt]): IRStmt = {
    if (stmts.length == 1) {
      stmts.head
    } else {
      BlockStmt(stmts)
    }
  }

}
