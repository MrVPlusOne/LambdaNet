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
  def prettyPrint(indentSpaces: Int = 2): String = {
    GStmt
      .prettyPrintHelper(0, this)
      .map {
        case (indent, text) => " " * (indent * indentSpaces) + text
      }
      .mkString("\n")
  }

  override def toString: String = prettyPrint()
}

// === Start of Statement definitions ====

case class VarDef(x: Symbol, ty: GTMark, init: GExpr, isConst: Boolean) extends GStmt

case class AssignStmt(lhs: GExpr, rhs: GExpr) extends GStmt

case class ExprStmt(e: GExpr, isReturn: Boolean) extends GStmt

case class IfStmt(cond: GExpr, branch1: GStmt, branch2: GStmt) extends GStmt

case class WhileStmt(cond: GExpr, body: GStmt) extends GStmt

case class BlockStmt(stmts: Vector[GStmt]) extends GStmt

case class FuncDef(
  name: Symbol,
  args: List[(Symbol, GTMark)],
  returnType: GTMark,
  body: GStmt
) extends GStmt

case class ClassDef(
  name: Symbol,
  superType: Option[Symbol] = None,
  constructor: FuncDef,
  vars: Map[Symbol, GTMark],
  funcDefs: Vector[FuncDef]
) extends GStmt {
  require(constructor.name == ClassDef.constructorName(name))
  require(constructor.returnType == GType.voidType)
}

object ClassDef {
  val thisSymbol = 'this
  val superSymbol = 'super
  def constructorName(className: Symbol): Symbol = Symbol("NEW-" + className.name)
}

// === End of Statement definitions ====

object GStmt {

  import GExpr.typeCheckInfer

  val returnSymbol = 'return

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
    stmt match {
      case VarDef(x, ty, init, isConst) =>
        val constModifier = if (isConst) " const" else ""
        Vector(indent -> s"let$constModifier ${x.name}: $ty = $init;")
      case AssignStmt(lhs, rhs) =>
        Vector(indent -> s"$lhs = $rhs;")
      case ExprStmt(v, isReturn) =>
        val returnModifier = if (isReturn) "return " else ""
        Vector(indent -> s"$returnModifier$v;")
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
          .map { case (v, tv) => s"${v.name}: $tv" }
          .mkString("(", ", ", ")")
        Vector(indent -> s"function ${funcName.name} $argList: $returnType {") ++
          prettyPrintHelper(indent + 1, body) ++ Vector(
          indent -> "}"
        )
      case ClassDef(name, superType, constructor, vars, funcDefs) =>
        val superPart = superType
          .map(t => s" extends $t")
          .getOrElse("")
        Vector(indent -> s"class ${name.name}$superPart {") ++
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
          funcDefs = cDef.funcDefs.map(x => rec(x).asInstanceOf[FuncDef])
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
      case s @ VarDef(_, _: GType, _, _) => fail(s)
      case s @ FuncDef(_, args, returnType, _) =>
        if (args.exists(_._2.isInstanceOf[GType]) ||
            returnType.isInstanceOf[GType] && returnType != GType.voidType) fail(s)
        else s
      case s: ClassDef =>
        if (s.vars.exists(_._2.isInstanceOf[GType])) fail(s)
        else s
      case other => other
    }
  }

  /** An context used for constructing programs written in [[GStmt]] */
  class TypeHoleContext {
    var typeHoleId: Int = 0
    val holeTypeMap: mutable.HashMap[GTHole, GType] = mutable.HashMap[GTHole, GType]()

    def newTHole(ty: Option[GType]): GTHole = {
      val h = GTHole(typeHoleId)
      typeHoleId += 1
      ty.foreach { t =>
        assert(!holeTypeMap.contains(h))
        holeTypeMap(h) = t
      }
      h
    }

    def reset(): Unit = {
      typeHoleId = 0
      holeTypeMap.clear()
    }
  }

  /** Replace all the type annotations with [[GTHole]]s */
  trait GStmtAPI extends GExprAPI {
    val typeHoleContext: TypeHoleContext = new TypeHoleContext()

    implicit def expr2Stmt(expr: GExpr): GStmt = ExprStmt(expr, isReturn = false)

    def RETURN(expr: GExpr) = ExprStmt(expr, isReturn = true)

    // todo: Fix the isConst part
    def VAR(x: Symbol, ty: GType)(init: GExpr): VarDef = {
      VarDef(x, typeHoleContext.newTHole(Some(ty)), init, isConst = false)
    }

    // todo: Fix the isConst part
    def VAR(x: Symbol)(init: GExpr): VarDef = {
      VarDef(x, typeHoleContext.newTHole(None), init, isConst = false)
    }

    def BLOCK(stmts: GStmt*): BlockStmt = {
      BlockStmt(stmts.toVector)
    }

    def TryBLOCK(stmt: GStmt): BlockStmt = {
      stmt match {
        case b: BlockStmt => b
        case _            => BlockStmt(Vector(stmt))
      }
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
      args.toList.map { case (s, t) => s -> typeHoleContext.newTHole(Some(t)) }
    }

    def stripType(t: GType): GTHole = {
      typeHoleContext.newTHole(Some(t))
    }

    def FUNC(name: Symbol, returnType: GType)(
      args: (Symbol, GType)*
    )(body: GStmt*): FuncDef = {
      val a1s = stripArgs(args)
      FuncDef(name, a1s, stripType(returnType), BLOCK(body: _*))
    }

    def CONSTRUCTOR(className: Symbol, args: (Symbol, GType)*)(body: GStmt*): FuncDef = {
      FuncDef(
        ClassDef.constructorName(className),
        stripArgs(args),
        GType.voidType,
        BLOCK(body: _*)
      )
    }

    def CLASS(name: Symbol, superType: Option[Symbol] = None)(
      vars: (Symbol, GType)*
    )(constructor: FuncDef, methods: FuncDef*): ClassDef = {
      assert(vars.length == vars.toMap.size)
      ClassDef(name, superType, constructor, stripArgs(vars).toMap, methods.toVector)
    }
  }

  object API extends GStmtAPI

  def extractSignature(funcDef: FuncDef): FuncType = {
    funcDef.args.map(_._2.asInstanceOf[GType]) -: funcDef.returnType.asInstanceOf[GType]
  }

  /**
    * Allows forward reference to function definitions, but they will not escape their scope.
    **/
  def typeCheckBlock(
    block: BlockStmt,
    ctx: ExprContext,
    returnType: GType
  ): Set[TypeCheckError] = {
    var currentCtx = ctx
    block.stmts.foreach {
      case f: FuncDef =>
        val signatureType = extractSignature(f)
        currentCtx = currentCtx.newVar(f.name, signatureType)
      case ClassDef(name, superType, constructor, vars, funcDefs) =>
        val sT = superType
          .map { s =>
            currentCtx.typeContext.typeUnfold(s).asInstanceOf[ObjectType]
          }
          .getOrElse(obj())
        var fields = sT.fields ++ vars
        funcDefs.foreach { f =>
          fields = fields.updated(f.name, extractSignature(f))
        }
        val constructorType = extractSignature(constructor).copy(to = name)
        val objT = ObjectType(fields.mapValuesNow(_.asInstanceOf[GType]))
        currentCtx = currentCtx.newTypeVar(name, objT).newVar(name, constructorType)
      case _ =>
    }
    var currentErrs = Set[TypeCheckError]()
    block.stmts.foreach { stmt =>
      val (newCtx, errs) = typeCheckStmt(stmt, currentCtx, returnType)
      currentCtx = newCtx
      currentErrs ++= errs
    }
    currentErrs
  }

  /**
    * Allows forward reference to function definitions, but they will not escape their scope.
    **/
  private def typeCheckStmt(
    stmt: GStmt,
    ctx: ExprContext,
    returnType: GType
  ): (ExprContext, Set[TypeCheckError]) = {
    import ctx.typeContext.mkSubtypeError
    stmt match {
      case VarDef(x, ty, init, _) =>
        ty match {
          case ty: GType =>
            val ctx1 = ctx.newVar(x, ty)
            val (initT, es) = typeCheckInfer(init, ctx1)
            ctx1 -> (es ++ mkSubtypeError(initT, ty))
          case _: GTHole =>
            val ctx0 = ctx.newVar(x, any)
            val (initT, es) = typeCheckInfer(init, ctx0)
            val ctx1 = ctx.newVar(x, initT)
            ctx1 -> es
        }

      case AssignStmt(lhs, rhs) =>
        val (lT, e1) = typeCheckInfer(lhs, ctx)
        val (rT, e2) = typeCheckInfer(rhs, ctx)
        ctx -> (e1 ++ e2 ++ mkSubtypeError(rT, lT))
      case ExprStmt(e, isReturn) =>
        val (eT, es) = typeCheckInfer(e, ctx)
        ctx -> (if (isReturn) es ++ mkSubtypeError(eT, returnType) else es)
      case IfStmt(cond, branch1, branch2) =>
        val (condT, e0) = typeCheckInfer(cond, ctx)
        val (_, e1) = typeCheckStmt(branch1, ctx, returnType)
        val (_, e2) = typeCheckStmt(branch2, ctx, returnType)
        ctx -> (e0 ++ e1 ++ e2 ++ mkSubtypeError(condT, GType.boolType))
      case WhileStmt(cond, body) =>
        val (condT, e0) = typeCheckInfer(cond, ctx)
        val (_, e1) = typeCheckStmt(body, ctx, returnType)
        ctx -> (e0 ++ e1 ++ mkSubtypeError(condT, GType.boolType))
      case FuncDef(_, args, newReturn: GType, body) =>
        val ctxWithArgs = ctx.copy(
          varAssign =
            ctx.varAssign ++ args.toMap.mapValuesNow(_.asInstanceOf[GType])
        )
        val (_, errs) = typeCheckStmt(body, ctxWithArgs, newReturn)
        ctx -> errs
      case block: BlockStmt =>
        ctx -> typeCheckBlock(block, ctx, returnType)
      case ClassDef(name, superType, constructor, _, funcDefs) =>
        val ctxWithThis = ctx
          .newVar(ClassDef.thisSymbol, ctx.typeContext.typeUnfold(name))
          .newVar(
            ClassDef.superSymbol,
            superType
              .map(ctx.typeContext.typeUnfold)
              .getOrElse(obj())
          )
        var errors = Set[TypeCheckError]()
        (funcDefs :+ constructor).foreach { stmt =>
          val (_, e) = typeCheckStmt(stmt, ctxWithThis, returnType)
          errors ++= e
        }
        ctx -> errors
      case _ =>
        throw new NotImplementedError(s"GTHoles in Stmt: $stmt")
    }
  }
}
