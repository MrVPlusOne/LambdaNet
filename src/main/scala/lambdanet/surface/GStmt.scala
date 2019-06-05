package lambdanet.surface

import lambdanet._
import lambdanet.translation.{IRTranslation, PLangTranslation}
import funcdiff.SimpleMath.Extensions._

import scala.language.implicitConversions

// @formatter:off
/** a program statement
  *
  * S :=                                    ([[GStmt]])
  *       var x: α = e                      ([[VarDef]])
  *       e := e                            ([[AssignStmt]])
  *       [return] e                        ([[ExprStmt]])
  *       if e then S else S                ([[IfStmt]])
  *       while e do S                      ([[WhileStmt]])
  *       // comment                        ([[CommentStmt]])
  * B in  { S; ...; S }                     ([[BlockStmt]])
  * f in  function x (x: α, ..., x:α): α B  ([[FuncDef]])
  *       class x (l: α, ..., l:α)          ([[ClassDef]])
  *       ↳ [extends x]{ f, ..., f }
  *       type x = t                        ([[TypeAliasStmt]])
  *       namespace x B                     ([[Namespace]])
  *       import ...                        ([[GImport]])
  *       export ...                        ([[GExport]])
  *
  * where x and l are [[Symbol]],
  *       t is [[GType]]
  *       α is `TypeTyAnnotation`,
  *       e is [[GExpr]],
  * */
// @formatter:on
sealed trait GStmt {
  def prettyPrint(indentSpaces: Int = 2, indent: Int = 0): String = {
    GStmt
      .prettyPrintHelper(indent, this)
      .map {
        case (dent, text) => " " * (dent * indentSpaces) + text
      }
      .mkString("\n")
  }

  override def toString: String = prettyPrint()
}

// === Start of Statement definitions ====

case class VarDef(
    name: Symbol,
    annot: TyAnnot,
    init: GExpr,
    isConst: Boolean,
    exportLevel: ExportLevel.Value
) extends GStmt

case class AssignStmt(lhs: GExpr, rhs: GExpr) extends GStmt {
//  assert(
//    !lhs.isInstanceOf[Const],
//    s"assign to a const(origin: line ${lhs.asInstanceOf[Const].line}): $lhs"
//  )
  // The lhs can be Const, e.g., in [a,b]=c;
}

case class ExprStmt(e: GExpr, isReturn: Boolean) extends GStmt

case class IfStmt(cond: GExpr, branch1: GStmt, branch2: GStmt) extends GStmt

case class WhileStmt(cond: GExpr, body: GStmt) extends GStmt

case class CommentStmt(text: String) extends GStmt

case class BlockStmt(stmts: Vector[GStmt]) extends GStmt

case class FuncDef(
    name: Symbol,
    tyVars: Vector[Symbol],
    args: Vector[(Symbol, TyAnnot)],
    returnType: TyAnnot,
    body: GStmt,
    exportLevel: ExportLevel.Value
) extends GStmt {
  def functionType: FuncType = {
    FuncType(args.map(_._2.get).toList, returnType.asInstanceOf[GType])
  }

}

case class ClassDef(
    name: Symbol,
    tyVars: Vector[Symbol],
    superType: Option[Symbol] = None,
    vars: Map[Symbol, TyAnnot],
    funcDefs: Vector[FuncDef],
    exportLevel: ExportLevel.Value
) extends GStmt {
  def objectType: GType = {
    ObjectType(
      vars.mapValuesNow(_.get) ++
        funcDefs.map(fd => fd.name -> fd.functionType)
    )
  }

}

case class TypeAliasStmt(
    name: Symbol,
    tyVars: Vector[Symbol],
    ty: GType,
    exportLevel: ExportLevel.Value
) extends GStmt

case class Namespace(
    name: Symbol,
    block: BlockStmt,
    exportLevel: ExportLevel.Value
) extends GStmt

case class GImport(content: ImportStmt) extends GStmt
case class GExport(content: ExportStmt) extends GStmt
// === End of Statement definitions ====

object GStmt {

  type IsStatic = Boolean

  /*
   * S :=                                    ([[GStmt]])
   *       var x: α = e                      ([[VarDef]])
   *       e := e                            ([[AssignStmt]])
   *       [return] e                        ([[ExprStmt]])
   *       if e then S else S                ([[IfStmt]])
   *       while e do S                      ([[WhileStmt]])
   * B in  { S; ...; S }                     ([[BlockStmt]])
   * f in  function x (x: α, ..., x:α): α B  ([[FuncDef]])
   *       class x (l: α, ..., l:α)          ([[ClassDef]])
   *       ↳ [extends x]{ f, ..., f }
   */
  def prettyPrintHelper(indent: Int, stmt: GStmt): Vector[(Int, String)] = {
    import ExportLevel.asPrefix
    stmt match {
      case VarDef(x, ty, init, isConst, level) =>
        val keyword = if (isConst) "const" else "let"
        Vector(indent -> s"${asPrefix(level)}$keyword ${x.name}: $ty = $init;")
      case AssignStmt(lhs, rhs) =>
        Vector(indent -> s"$lhs = $rhs;")
      case ExprStmt(v, isReturn) =>
        val returnModifier = if (isReturn) "return " else ""
        Vector(indent -> s"$returnModifier$v;")
      case IfStmt(cond, e1, e2) =>
        lazy val elsePart = {
          if (e2 == BlockStmt(Vector())) Vector()
          else Vector(indent -> "else") ++ prettyPrintHelper(indent, e2)
        }
        Vector(indent -> s"if ($cond)") ++
          prettyPrintHelper(indent, e1) ++ elsePart
      case WhileStmt(cond, body) =>
        (indent -> s"while ($cond)") +: prettyPrintHelper(indent, body)
      case CommentStmt(text) =>
        Vector(indent -> ("/* " + text + " */"))
      case BlockStmt(stmts) =>
        (indent -> "{") +: stmts.flatMap(
          s => prettyPrintHelper(indent + 1, s)
        ) :+ (indent -> "}")
      case FuncDef(funcName, tyVars, args, returnType, body, level) =>
        val argList = args
          .map { case (v, tv) => s"${v.name}: $tv" }
          .mkString("(", ", ", ")")
        val tyVarPart = tyVarClause(tyVars)
        Vector(
          indent -> s"${asPrefix(level)}function ${funcName.name}$tyVarPart $argList: $returnType"
        ) ++
          prettyPrintHelper(indent, makeSureInBlock(body))
      case ClassDef(
          name,
          tyVars,
          superType,
          vars,
          funcDefs,
          level
          ) =>
        val superPart = superType
          .map(t => s" extends $t")
          .getOrElse("")
        val tyVarPart = tyVarClause(tyVars)
        Vector(
          indent -> s"${asPrefix(level)}class ${name.name}$tyVarPart$superPart {"
        ) ++
          vars.toList.map {
            case (fieldName, annot) =>
              (indent + 1, s"${fieldName.name}: $annot;")
          } ++
          funcDefs.flatMap { fDef =>
            prettyPrintHelper(indent + 1, fDef)
          } ++
          Vector(indent -> "}")
      case TypeAliasStmt(name, tyVars, ty, level) =>
        val tyVarList =
          if (tyVars.isEmpty) "" else tyVars.map(_.name).mkString("<", ",", ">")
        Vector(
          indent -> s"${asPrefix(level)}type ${name.name}$tyVarList = $ty;"
        )
      case Namespace(name, block, level) =>
        (indent -> s"${asPrefix(level)}namespace ${name.name}") +:
          prettyPrintHelper(indent, block)
      case GImport(content) =>
        Vector(indent -> content.toString)
      case GExport(content) =>
        Vector(indent -> content.toString)
    }
  }

  def tyVarClause(tyVars: Vector[Symbol]): String = {
    if (tyVars.isEmpty) ""
    else tyVars.map(_.name).mkString("<", ", ", ">")
  }

  /**
    * @param f is required to has the same return statement type as its input
    * */
  def modifyChildren(stmt: GStmt)(f: GStmt => GStmt): GStmt = {
    def rec(stmt: GStmt): GStmt = stmt match {
      case IfStmt(cond, branch1, branch2) =>
        f(IfStmt(cond, rec(branch1), rec(branch2)))
      case WhileStmt(cond, body) => f(WhileStmt(cond, rec(body)))
      case BlockStmt(stmts)      => f(BlockStmt(stmts.map(rec)))
      case fDef: FuncDef         => f(fDef.copy(body = rec(fDef.body)))
      case cDef: ClassDef =>
        val c1 = cDef.copy(
          funcDefs = cDef.funcDefs.map { x =>
            rec(x).asInstanceOf[FuncDef]
          }
        )
        f(c1)
      case other => f(other)
    }
    rec(stmt)
  }

  def extractSignature(
      funcDef: FuncDef,
      eliminateTVars: Boolean = true
  ): FuncType = {
    val fT = funcDef.args.map(_._2.get).toList -: funcDef.returnType.get
      .asInstanceOf[GType]
    if (eliminateTVars) {
      PLangTranslation
        .monotype(fT)(funcDef.tyVars.toSet)
        .asInstanceOf[FuncType]
    } else fT
  }

  def makeSureInBlock(stmt: GStmt): BlockStmt = {
    stmt match {
      case b: BlockStmt => b
      case _            => BlockStmt(Vector(stmt))
    }
  }

  val constructorName: Symbol = 'CONSTRUCTOR

  def isConstructor(name: Symbol): Boolean = {
    name.name.endsWith(constructorName.name)
  }
}
