package lambdanet.translation

import lambdanet._
import lambdanet.surface._
import lambdanet.types.GType

/**
  * An intermediate program representation useful for type inference
  */
object OldIR {
  type VarName = Symbol
  type TypeName = Symbol

  case class IRModule(
      path: ProjectPath,
      imports: Vector[ImportStmt],
      exportStmts: Vector[ExportStmt],
      exports: ModuleExports,
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

  object ExportCategory extends Enumeration {
    val Term, Class, TypeAlias = Value
  }

  type Exported = Boolean

  case class ModuleExports(
      definitions: Map[(Symbol, ExportCategory.Value), (IRType, Exported)],
      defaultVar: Option[(Var, IRType)],
      defaultType: Option[(TypeName, IRType)]
  ) {
    lazy val terms: Map[Symbol, (IRType, Exported)] = definitions.toIterator.collect {
      case ((n, ExportCategory.Term), t) => n -> t
    }.toMap

    lazy val typeAliases: Map[Symbol, (IRType, Exported)] =
      definitions.toIterator.collect {
        case ((n, cat), t) if cat == ExportCategory.TypeAlias =>
          n -> t
      }.toMap

    lazy val classes: Map[Symbol, (IRType, Exported)] = definitions.toIterator.collect {
      case ((n, cat), t) if cat == ExportCategory.Class =>
        n -> t
    }.toMap
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

  class IRType(
      protected val id: Int,
      val name: Option[Symbol],
      val annotation: Option[TypeAnnotation],
      val libId: Option[Symbol]
  ) extends IdEquality {
    def showDetails: String = {
      val parts = name.map(n => s"{${n.name}}").toList ++ annotation
        .map(t => s"[$t]")
        .toList
      s"𝒯$id${parts.mkString}"
    }

    override def toString: String = {
      val namePart = name.map(n => s"{${n.name}}").getOrElse("")
      s"𝒯$id$namePart"
    }
  }

  object IRType {
    def apply(
        id: Int,
        name: Option[Symbol],
        annotation: Option[TypeAnnotation],
        libId: Option[Symbol]
    ): IRType = new IRType(id, name, annotation, libId)
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

  case class IfStmt(cond: Var, e1: BlockStmt, e2: BlockStmt) extends IRStmt

  case class WhileStmt(cond: Var, body: BlockStmt) extends IRStmt

  case class BlockStmt(stmts: Vector[IRStmt]) extends IRStmt

  case class FuncDef(
      name: Symbol,
      args: List[(Var, IRType)],
      returnType: IRType,
      body: BlockStmt,
      funcT: IRType,
      exportLevel: ExportLevel.Value
  ) extends IRStmt

  case class ClassDef(
      name: TypeName,
      superType: Option[TypeName] = None,
      vars: Map[TypeName, IRType],
      funcDefs: Vector[FuncDef],
      classT: IRType,
      companionT: IRType,
      exportLevel: ExportLevel.Value
  ) extends IRStmt

  case class TypeAliasIRStmt(aliasT: IRType, level: ExportLevel.Value) extends IRStmt {
    require(aliasT.annotation.nonEmpty)

    val name: TypeName = aliasT.name.get
  }

  object ClassDef {
    val thisVar = namedVar(thisSymbol)
    val superVar = namedVar(superSymbol)
  }

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
        case FuncDef(funcName, args, returnType, body, funcT, level) =>
          val argList = args
            .map { case (v, tv) => s"$v: $tv" }
            .mkString("(", ", ", ")")
          val returnMark =
            if (returnType.annotation.contains(GType.voidType)) ""
            else s": $returnType"
          Vector(
            indent -> s"${asPrefix(level)}function ${funcName.name}:$funcT $argList$returnMark"
          ) ++ prettyPrintHelper(indent, body)

        case ClassDef(name, superType, vars, funcDefs, classT, _, level) =>
          val superPart = superType
            .map(t => s"extends $t")
            .getOrElse("")
          Vector(indent -> s"${asPrefix(level)}class ${name.name}: $classT $superPart {") ++
            vars.toList.map {
              case (fieldName, tv) =>
                (indent + 1, s"${fieldName.name}: $tv;")
            } ++
            funcDefs.flatMap(
              fDef => prettyPrintHelper(indent + 1, fDef)
            ) ++
            Vector(indent -> "}")
        case TypeAliasIRStmt(aliasT, level) =>
          Vector(indent -> s"${asPrefix(level)}alias: $aliasT;")
      }
    }
  }

  def groupInBlock(stmts: Vector[IRStmt]): BlockStmt = {
    stmts match {
      case Vector(b: BlockStmt) => b
      case _                    => BlockStmt(stmts)
    }
  }
}
