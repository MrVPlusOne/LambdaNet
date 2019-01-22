package gtype

import scala.language.implicitConversions
import GType.API._
import gtype.GExpr.GExprAPI
import funcdiff.SimpleMath.Extensions._

// @formatter:off
/** a program statement
  *
  * S :=                                  ([[GStmt]])
  *   | var x: α = e                      ([[VarDef]])
  *   | e := e                            ([[AssignStmt]])
  *   | [return] e                        ([[ExprStmt]])
  *   | if e then e else e                ([[IfStmt]])
  *   | while e do e                      ([[WhileStmt]])
  *   | { S; ...; S }                     ([[BlockStmt]])
  *   | function x (x: α, ..., x:α): α    ([[FuncDef]])
  *   | class x (l: α, ..., l:α)          ([[ClassDef]])
  *     ↳ [extends x]{ f, ..., f }
  *
  * where x and l are [[Symbol]],
  *       α is [[GTMark]],
  *       e is [[GExpr]],
  *       f is [[FuncDef]]
  * */
// @formatter:on
sealed trait GStmt

// === Start of Statement definitions ====

case class VarDef(x: Symbol, ty: GTMark, init: GExpr, isConst: Boolean) extends GStmt

case class AssignStmt(lhs: GExpr, rhs: GExpr) extends GStmt

case class ExprStmt(e: GExpr, isReturn: Boolean) extends GStmt

case class IfStmt(cond: GExpr, branch1: GStmt, branch2: GStmt) extends GStmt

case class WhileStmt(cond: GExpr, body: GStmt) extends GStmt

case class BlockStmt(stmts: Vector[GStmt]) extends GStmt

case class FuncDef(name: Symbol, args: List[(Symbol, GTMark)], returnType: GTMark, body: GStmt)
    extends GStmt

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

  /** An context used for constructing programs written in [[GStmt]] */
  class SurfaceContext(){
    var typeHoleId: Int = 0

    def newTHole(): GTHole = {
      val h = GTHole(typeHoleId)
      typeHoleId += 1
      h
    }

    def reset(): Unit ={
      typeHoleId = 0
    }
  }

  trait GStmtAPI extends GExprAPI {
    implicit def expr2Stmt(expr: GExpr): GStmt = ExprStmt(expr, isReturn = false)

    def RETURN(expr: GExpr) = ExprStmt(expr, isReturn = true)

    // todo: Fix the isConst part
    def VAR(x: Symbol, ty: GTMark)(init: GExpr) = VarDef(x, ty, init, isConst = false)

    // todo: Fix the isConst part
    def VAR(x: Symbol)(init: GExpr)(implicit ctx: SurfaceContext) = {
      VarDef(x, ctx.newTHole(), init, isConst = false)
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

    def FUNC(name: Symbol, returnType: GTMark)(args: (Symbol, GTMark)*)(body: GStmt*): FuncDef = {
      FuncDef(name, args.toList, returnType, BLOCK(body: _*))
    }

    def CONSTRUCTOR(className: Symbol, args: (Symbol, GTMark)*)(body: GStmt*): FuncDef = {
      FuncDef(ClassDef.constructorName(className), args.toList, GType.voidType, BLOCK(body: _*))
    }

    def CLASS(name: Symbol, superType: Option[Symbol] = None)(
      vars: (Symbol, GTMark)*
    )(constructor: FuncDef, methods: FuncDef*): ClassDef = {
      assert(vars.length == vars.toMap.size)
      ClassDef(name, superType, constructor, vars.toMap, methods.toVector)
    }
  }

  object API extends GStmtAPI

  def extractSignature(funcDef: FuncDef): FuncType = {
    funcDef.args.map(_._2.asInstanceOf[GType]) -: funcDef.returnType.asInstanceOf[GType]
  }

  /**
    * Allows forward reference to function definitions, but they will not escape their scope.
    **/
  def typeCheckBlock(block: BlockStmt, ctx: ExprContext, returnType: GType): Set[TypeCheckError] = {
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
