package lambdanet.translation

import lambdanet._
import types._

object IR {
  type VarName = Symbol
  type TypeName = Symbol

  case class IRModule(
      path: ProjectPath,
      imports: Vector[ImportStmt],
      exportStmts: Vector[ExportStmt],
      stmts: Vector[IRStmt]
  ) {
    def moduleStats: IRModuleStats = {
      var fieldsUsed, fieldsDefined: Set[Symbol] = Set()

      /** collects fields definitions and usages */
      def processExpr(expr: IRExpr): Unit = expr match {
        case ObjLiteral(fields) =>
          fieldsDefined ++= fields.keySet
        case FieldAccess(_, label) =>
          fieldsUsed += label
        case _ =>
      }

      /** collects fields definitions and usages */
      def processStmt(stmt: IRStmt): Unit = stmt match {
        case s: VarDef => processExpr(s.rhs)
        case s: IfStmt =>
          processStmt(s.e1)
          processStmt(s.e2)
        case s: WhileStmt => processStmt(s.body)
        case s: BlockStmt => s.stmts.foreach(processStmt)
        case s: FuncDef   => s.body.stmts.foreach(processStmt)
        case s: ClassDef =>
          s.funcDefs.foreach(processStmt)
          fieldsDefined ++= s.funcDefs.map(_.name)
          fieldsDefined ++= s.vars.keySet
        case _ =>
      }

      stmts.foreach(processStmt)

      IRModuleStats(fieldsUsed, fieldsDefined)
    }
  }

  case class IRModuleStats(
      fieldsUsed: Set[Symbol],
      fieldsDefined: Set[Symbol]
  )

  // @formatter:off
  /** a simple expression
    *
    *  e :=                         ([[IRExpr]])
    *     | x                       ([[Ground]])
    *     | x(x,...,x)              ([[FuncCall]])
    *     | { l: x, ..., l: x }     ([[ObjLiteral]])
    *     | e.l                     ([[FieldAccess]])
    *     | if x then x else x      ([[IfExpr]])
    *
    *  x :=
    *     | v                       ([[Var]])
    *     | c: t                    ([[Const]])
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

  sealed trait Ground extends IRExpr

  case class Var(content: Either[Int, Symbol]) extends Ground {
    val nameOpt: Option[Symbol] = content.right.toOption

    def prettyPrint: String = content match {
      case Left(id)    => s"𝓥$id"
      case Right(name) => name.name
    }
  }

  def namedVar(name: Symbol) = Var(Right(name))

  case class Const(value: Any, ty: GType) extends Ground {
    def prettyPrint: String = s"($value: $ty)"
  }
  case class FuncCall(f: Ground, args: Vector[Ground]) extends IRExpr {
    def prettyPrint: String = s"$f${args.mkString("(", ", ", ")")}"
  }
  case class ObjLiteral(fields: Map[Symbol, Ground]) extends IRExpr {
    def prettyPrint: String =
      fields.map { case (f, v) => s"$f: $v" }.mkString("{", ", ", "}")
  }
  case class FieldAccess(receiver: Ground, label: Symbol) extends IRExpr {
    def prettyPrint: String = s"$receiver.${label.name}"
  }
  case class IfExpr(cond: Ground, e1: Ground, e2: Ground) extends IRExpr {
    def prettyPrint: String = s"($cond ? $e1 : $e2)"
  }
  case class Cast(expr: Ground, ty: GType) extends IRExpr {
    def prettyPrint: String = s"(${expr.prettyPrint} as $ty)"
  }
  // @formatter:off
  /**
    *
    * a statement in Single Assignment Form
    *
    * S :=                                  ([[IRStmt]])
    *   | var x: α = e                      ([[VarDef]])
    *   | x := x                            ([[Assign]])
    *   | [return] x                        ([[ReturnStmt]])
    *   | if(x) S else S                    ([[IfStmt]])
    *   | while(x) S                        ([[WhileStmt]])
    *   | { S; ...; S }                     ([[BlockStmt]])
    *   | function x (x: τ, ..., x:τ): τ S  ([[FuncDef]])
    *   | class x (l: α, ..., l:α)          ([[ClassDef]])
    *     ↳ [extends x]{ f, ..., f }
    *   | type x = t                        ([[TypeAliasStmt]])
    *   | namespace x B                     ([[Namespace]])
    *
    * where x is [[Var]]
    *       l is [[Symbol]],
    *       α is [[GTMark]],
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

  case class VarDef(
      v: Var,
      mark: GTMark,
      rhs: IRExpr,
      exportLevel: ExportLevel.Value
  ) extends IRStmt

  case class Assign(lhs: Var, rhs: Ground) extends IRStmt

  case class ReturnStmt(v: Ground) extends IRStmt

  case class IfStmt(cond: Ground, e1: BlockStmt, e2: BlockStmt) extends IRStmt

  case class WhileStmt(cond: Ground, body: BlockStmt) extends IRStmt

  case class BlockStmt(stmts: Vector[IRStmt]) extends IRStmt

  case class FuncDef(
      name: Symbol,
      args: Vector[(Var, GTMark)],
      returnType: GTMark,
      body: BlockStmt,
      exportLevel: ExportLevel.Value
  ) extends IRStmt

  case class ClassDef(
      name: TypeName,
      superType: Option[TypeName] = None,
      vars: Map[TypeName, GTMark],
      funcDefs: Vector[FuncDef],
      exportLevel: ExportLevel.Value
  ) extends IRStmt {
    val companion: Var = Var(Right(name))
  }

  case class TypeAliasStmt(
      name: Symbol,
      ty: GType,
      level: ExportLevel.Value
  ) extends IRStmt

  case class Namespace(name: Symbol, block: BlockStmt) extends IRStmt

  object IRStmt {
    def prettyPrintHelper(indent: Int, stmt: IRStmt): Vector[(Int, String)] = {
      import ExportLevel._

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
        case FuncDef(funcName, args, returnType, body, level) =>
          val argList = args
            .map { case (v, tv) => s"$v: $tv" }
            .mkString("(", ", ", ")")
          Vector(
            indent -> s"${asPrefix(level)}function ${funcName.name} $argList: $returnType"
          ) ++ prettyPrintHelper(indent, body)

        case ClassDef(name, superType, vars, funcDefs, level) =>
          val superPart = superType
            .map(t => s"extends $t")
            .getOrElse("")
          Vector(indent -> s"${asPrefix(level)}class ${name.name} $superPart {") ++
            vars.toList.map {
              case (fieldName, tv) =>
                (indent + 1, s"${fieldName.name}: $tv;")
            } ++
            funcDefs.flatMap(
              fDef => prettyPrintHelper(indent + 1, fDef)
            ) ++
            Vector(indent -> "}")
        case TypeAliasStmt(name, ty, level) =>
          Vector(indent -> s"${asPrefix(level)} type $name = $ty;")
        case Namespace(name, block) =>
          (indent -> s"namespace ${name.name}") +:
            prettyPrintHelper(indent, block)
      }
    }
  }

}
