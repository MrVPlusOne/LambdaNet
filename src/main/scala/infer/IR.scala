package infer

import gtype._
import gtype.ExportLevel.asPrefix
import gtype.GModule.ProjectPath

/**
  * An intermediate program representation useful for type inference
  */
object IR {

  type IRTypeId = Int
  type ClassName = Symbol

  case class IRModule(
    path: ProjectPath,
    imports: Vector[ImportStmt],
    exportStmts: Vector[ExportStmt],
    exports: ModuleExports,
    stmts: Vector[IRStmt]
  )


  object ExportCategory extends Enumeration {
    val Term, Class, TypeAlias = Value
  }

  case class ModuleExports(
    definitions: Map[Symbol, (IRType, ExportCategory.Value)],
    defaultVar: Option[(Var, IRType)],
    defaultType: Option[(ClassName, IRType)]
  ){
    def terms: Iterator[(Symbol, IRType)] = definitions.toIterator.collect{
      case (n, (t, ExportCategory.Term)) => n -> t
    }

    def types: Iterator[(Symbol, IRType)] = definitions.toIterator.collect{
      case (n, (t, cat)) if cat == ExportCategory.Class || cat == ExportCategory.TypeAlias => n -> t
    }
  }

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

  case class Var(content: Either[Int, Symbol]) extends IRExpr {
    val nameOpt: Option[Symbol] = content.right.toOption

    def prettyPrint: String = content match {
      case Left(id)    => s"𝓥$id"
      case Right(name) => name.name
    }
  }

  def namedVar(name: Symbol) = Var(Right(name))

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

  case class IRType(id: Int, name: Option[Symbol], freezeToType: Option[GType]) {
    override def toString: String = {
      val parts = name.map(n => s"{${n.name}}").toList ++ freezeToType
        .map(t => s"[$t]")
        .toList
      s"𝒯$id${parts.mkString}"
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
    *   | function x (x: τ, ..., x:τ): τ S  ([[FuncDef]])
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

  case class VarDef(v: Var, mark: IRType, rhs: IRExpr, exportLevel: ExportLevel.Value)
      extends IRStmt

  case class Assign(lhs: Var, rhs: Var) extends IRStmt

  case class ReturnStmt(v: Var) extends IRStmt

  case class IfStmt(cond: Var, e1: IRStmt, e2: IRStmt) extends IRStmt

  case class WhileStmt(cond: Var, body: IRStmt) extends IRStmt

  case class BlockStmt(stmts: Vector[IRStmt]) extends IRStmt

  case class FuncDef(
    name: Symbol,
    args: List[(Var, IRType)],
    returnType: IRType,
    body: Vector[IRStmt],
    funcT: IRType,
    exportLevel: ExportLevel.Value
  ) extends IRStmt

  case class ClassDef(
    name: Symbol,
    superType: Option[Symbol] = None,
    constructor: FuncDef,
    vars: Map[Symbol, IRType],
    funcDefs: Vector[FuncDef],
    classT: IRType,
    exportLevel: ExportLevel.Value
  ) extends IRStmt {
    require(constructor.name == gtype.ClassDef.constructorName(name))
    require(constructor.returnType == classT)
  }

  object ClassDef {
    val thisVar = namedVar(gtype.ClassDef.thisSymbol)
//    val superVar = Var(gtype.ClassDef.superSymbol)
  }

  object IRStmt {
    def prettyPrintHelper(indent: Int, stmt: IRStmt): Vector[(Int, String)] = {
      stmt match {
        case VarDef(v, ty, rhs, level) =>
          Vector(indent -> s"${asPrefix(level)}let $v: $ty = $rhs;")
        case Assign(lhs, rhs) => Vector(indent -> s"$lhs = $rhs;")
        case ReturnStmt(v)    => Vector(indent -> s"return $v;")
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
        case FuncDef(funcName, args, returnType, body, funcT, level) =>
          val argList = args
            .map { case (v, tv) => s"$v: $tv" }
            .mkString("(", ", ", ")")
          val returnMark =
            if (returnType.freezeToType.contains(GType.voidType)) ""
            else s": $returnType"
          Vector(
            indent -> s"${asPrefix(level)}function ${funcName.name}:$funcT $argList$returnMark {"
          ) ++
            body.flatMap(s => prettyPrintHelper(indent + 1, s)) ++ Vector(
            indent -> "}"
          )
        case ClassDef(name, superType, constructor, vars, funcDefs, classT, level) =>
          val superPart = superType
            .map(t => s"extends $t")
            .getOrElse("")
          Vector(indent -> s"${asPrefix(level)}class ${name.name}: $classT $superPart {") ++
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
  def groupInBlock(stmts: Vector[IRStmt]): IRStmt = {
    if (stmts.length == 1) {
      stmts.head
    } else {
      BlockStmt(stmts)
    }
  }
}
