package gtype

import scala.language.implicitConversions
import GType.API._
import gtype.GExpr.GExprAPI
import funcdiff.SimpleMath.Extensions._
import collection.mutable

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
  *
  * where x and l are [[Symbol]],
  *       α is [[GTMark]],
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
  x: Symbol,
  ty: GTMark,
  init: GExpr,
  isConst: Boolean,
  exportLevel: ExportLevel.Value
) extends GStmt

case class AssignStmt(lhs: GExpr, rhs: GExpr) extends GStmt

case class ExprStmt(e: GExpr, isReturn: Boolean) extends GStmt

case class IfStmt(cond: GExpr, branch1: GStmt, branch2: GStmt) extends GStmt

case class WhileStmt(cond: GExpr, body: GStmt) extends GStmt

case class CommentStmt(text: String) extends GStmt

case class BlockStmt(stmts: Vector[GStmt]) extends GStmt

case class FuncDef(
  name: Symbol,
  tyVars: List[Symbol],
  args: List[(Symbol, GTMark)],
  returnType: GTMark,
  body: GStmt,
  exportLevel: ExportLevel.Value
) extends GStmt {
  def functionType: FuncType = {
    FuncType(args.map(_._2.asInstanceOf[GType]), returnType.asInstanceOf[GType])
  }

}

case class ClassDef(
  name: Symbol,
  tyVars: List[Symbol],
  superType: Option[Symbol] = None,
  constructor: FuncDef,
  vars: Map[Symbol, (GTMark, Boolean)],
  funcDefs: Vector[(FuncDef, Boolean)],
  exportLevel: ExportLevel.Value,
  isAbstract: Boolean
) extends GStmt {
  require(constructor.name == GStmt.constructorName)
  require(constructor.returnType == GType.voidType, s"but get: ${constructor.returnType}")
}

case class TypeAliasStmt(
  name: Symbol,
  tyVars: List[Symbol],
  ty: GType,
  level: ExportLevel.Value
) extends GStmt


// === End of Statement definitions ====

object GStmt {

  import GExpr.typeCheckInfer

  val returnSymbol = 'return

  def staticName(symbol: Symbol) = Symbol(s"static ${symbol.name}")

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
      case ClassDef(name, tyVars, superType, constructor, vars, funcDefs, level, isAbstract) =>
        val superPart = superType
          .map(t => s" extends $t")
          .getOrElse("")
        val tyVarPart = tyVarClause(tyVars)
        val abstractPart = if(isAbstract) "abstract " else ""
        Vector(indent -> s"${asPrefix(level)}${abstractPart}class ${name.name}$tyVarPart$superPart {") ++
          vars.toList.map {
            case (fieldName, tv) =>
              (indent + 1, s"${fieldName.name}: $tv;")
          } ++
          ((constructor, false) +: funcDefs).flatMap{
            case (fDef, isStatic) => prettyPrintHelper(indent + 1, fDef.copy(name = staticName(name)))
          } ++
          Vector(indent -> "}")
      case TypeAliasStmt(name, tyVars, ty, level) =>
        val tyVarList = if(tyVars.isEmpty) "" else tyVars.map(_.name).mkString("<", ",", ">")
        Vector(indent -> s"${asPrefix(level)}type ${name.name}$tyVarList = $ty;")
    }
  }

  def tyVarClause(tyVars: List[Symbol]): String = {
    if (tyVars.isEmpty) ""
    else tyVars.map(_.name).mkString("<", ", ", ">")
  }

  /**
    * @param f is required to has the same return statement type as its input
    * */
  def modifyChildren(stmt: GStmt)(f: GStmt => GStmt): GStmt = {
    def rec(stmt: GStmt): GStmt = stmt match {
      case IfStmt(cond, branch1, branch2) => f(IfStmt(cond, rec(branch1), rec(branch2)))
      case WhileStmt(cond, body)          => f(WhileStmt(cond, rec(body)))
      case BlockStmt(stmts)               => f(BlockStmt(stmts.map(rec)))
      case fDef: FuncDef                  => f(fDef.copy(body = rec(fDef.body)))
      case cDef: ClassDef =>
        val c1 = cDef.copy(
          constructor = rec(cDef.constructor).asInstanceOf[FuncDef],
          funcDefs = cDef.funcDefs.map{ case (x, isStatic) =>
            rec(x).asInstanceOf[FuncDef] -> isStatic }
        )
        f(c1)
      case other => f(other)
    }
    rec(stmt)
  }

  def assertAllTypesStripped(stmt: GStmt): Unit = {
    def fail(s: GStmt): Nothing = {
      throw new AssertionError(s"Type annotation appears in: $s")
    }

    modifyChildren(stmt) {
      case s: VarDef => if (s.ty.isInstanceOf[GType]) fail(s) else s
      case s: FuncDef =>
        if (s.args.exists(_._2.isInstanceOf[GType]) ||
            s.returnType.isInstanceOf[GType] && s.returnType != GType.voidType) fail(s)
        else s
      case s: ClassDef =>
        if (s.vars.exists(_._2._1.isInstanceOf[GType])) fail(s)
        else s
      case other => other
    }
  }

  case class TypeAnnotation(ty: GType, needInfer: Boolean) {
    override def toString: String = {
      s"$ty${if (needInfer) "*" else ""}"
    }
  }

  /** An context used for constructing programs written in [[GStmt]] */
  class TypeHoleContext {
    var typeHoleId: Int = 0
    val holeTypeMap: mutable.HashMap[GTHole, GType] = mutable.HashMap[GTHole, GType]()
    val userAnnotatedSet: mutable.HashSet[GTHole] = mutable.HashSet[GTHole]()

    def newTHole(mark: Option[TypeAnnotation]): GTHole = {
      val h = GTHole(typeHoleId)
      typeHoleId += 1
      mark.foreach { m =>
        assert(!holeTypeMap.contains(h))
        holeTypeMap(h) = m.ty
        if (m.needInfer) {
          userAnnotatedSet += h
        }
      }
      h
    }
  }

  /** Replace all the type annotations with [[GTHole]]s */
  trait GStmtAPI extends GExprAPI {
    var typeHoleContext: TypeHoleContext = new TypeHoleContext()

    implicit def expr2Stmt(expr: GExpr): GStmt = ExprStmt(expr, isReturn = false)

    def RETURN(expr: GExpr) = ExprStmt(expr, isReturn = true)

    def VAR(
      x: Symbol,
      ty: GType,
      isConst: Boolean = false
    )(init: GExpr): VarDef = {
      VarDef(
        x,
        typeHoleContext.newTHole(Some(TypeAnnotation(ty, needInfer = true))),
        init,
        isConst,
        exportLevel = ExportLevel.Public
      )
    }

    def VAR(x: Symbol)(init: GExpr): VarDef = {
      VarDef(
        x,
        typeHoleContext.newTHole(None),
        init,
        isConst = false,
        exportLevel = ExportLevel.Public
      )
    }

    def BLOCK(stmts: GStmt*): BlockStmt = {
      BlockStmt(stmts.toVector)
    }

    def WHILE(cond: GExpr)(stmts: GStmt*): WhileStmt = {
      WhileStmt(cond, BLOCK(stmts: _*))
    }

    case class IFBuild(b: GExpr, branch1: BlockStmt, mkBranch2: BlockStmt => GStmt) {
      def ELSE(branch2: GStmt*) = IfStmt(b, branch1, mkBranch2(BLOCK(branch2: _*)))

      def NoElse: IfStmt = IfStmt(b, branch1, mkBranch2(BLOCK()))

      def EIF(cond: GExpr)(body: GStmt*): IFBuild = {
        IFBuild(b, branch1, block => {
          IfStmt(cond, BLOCK(body: _*), mkBranch2(block))
        })
      }
    }

    def IF(b: GExpr)(branch1: GStmt*) = IFBuild(b, BLOCK(branch1: _*), identity)

    def stripArgs(args: Seq[(Symbol, GType)]): List[(Symbol, GTHole)] = {
      args.toList.map {
        case (s, t) =>
          s -> typeHoleContext.newTHole(Some(TypeAnnotation(t, needInfer = true)))
      }
    }

    def stripType(t: GType): GTHole = {
      typeHoleContext.newTHole(Some(TypeAnnotation(t, needInfer = true)))
    }

    def FUNC(name: Symbol, returnType: GType)(
      args: (Symbol, GType)*
    )(body: GStmt*): FuncDef = {
      val a1s = stripArgs(args)
      FuncDef(
        name,
        List(),
        a1s,
        stripType(returnType),
        BLOCK(body: _*),
        exportLevel = ExportLevel.Public
      )
    }

    def CONSTRUCTOR(className: Symbol, args: (Symbol, GType)*)(body: GStmt*): FuncDef = {
      FuncDef(
        constructorName,
        List(),
        stripArgs(args),
        GType.voidType,
        BLOCK(body: _*),
        exportLevel = ExportLevel.Public
      )
    }

  }

  object API extends GStmtAPI

  def extractSignature(funcDef: FuncDef): FuncType = {
    funcDef.args.map(_._2.asInstanceOf[GType]) -: funcDef.returnType.asInstanceOf[GType]
  }


  def makeSureInBlock(stmt: GStmt): BlockStmt = {
    stmt match {
      case b: BlockStmt => b
      case _            => BlockStmt(Vector(stmt))
    }
  }

  val constructorName: Symbol = 'CONSTRUCTOR

  def isConstructor(name: Symbol) = name.name.endsWith("-NEW")

  val thisSymbol = 'this
  val superSymbol = 'super
}
