package gtype

import scala.language.implicitConversions
import GType.API._
import gtype.GExpr.GExprAPI

case class GProgram() {

}

/** a program statement
  *
  * S :=                                    [[GStmt]]
  *   | var x: α = e                        [[VarDef]]
  *   | x := e                              [[AssignStmt]]
  *   | function x (x: α, ..., x:α): α      [[FuncDef]]
  *   | [return] e                          [[ExprStmt]]
  *   | if e then e else e                  [[IfStmt]]
  *   | while e do e                        [[WhileStmt]]
  *
  *   where x is [[Symbol]], α is [[GTMark]], and e is [[GExpr]]
  * */
sealed trait GStmt

// === Start of Statement definitions ====

case class VarDef(x: Symbol, ty: GTMark, init: GExpr) extends GStmt

case class AssignStmt(x: Symbol, expr: GExpr) extends GStmt

case class ExprStmt(e: GExpr, isReturn: Boolean) extends GStmt

case class IfStmt(cond: GExpr, branch1: GStmt, branch2: GStmt) extends GStmt

case class WhileStmt(cond: GExpr, body: GStmt) extends GStmt

case class BlockStmt(stmts: IndexedSeq[GStmt]) extends GStmt

case class FuncDef(name: Symbol, args: List[(Symbol, GTMark)],
                   returnType: GTMark, body: GStmt) extends GStmt

// === End of Statement definitions ====

object GStmt {
  import GExpr.typeCheckInfer

  trait GStmtAPI extends GExprAPI {
    implicit def expr2Stmt(expr: GExpr): GStmt = ExprStmt(expr, isReturn = false)

    def RETURN(expr: GExpr) = ExprStmt(expr, isReturn = true)

    def VAR(x: Symbol, ty: GTMark)(init: GExpr) = VarDef(x, ty, init)

    def VAR(x: Symbol)(init: GExpr) = VarDef(x, GTHole, init)

    def BLOCK(stmts: GStmt*): BlockStmt = {
      BlockStmt(stmts.toIndexedSeq)
    }

    def WHILE(cond: GExpr)(stmts: GStmt*): WhileStmt = {
      WhileStmt(cond, BLOCK(stmts: _*))
    }

    case class IFBuild(b: GExpr, branch1: Seq[GStmt]){
      def ELSE(branch2: GStmt*) = IfStmt(b, BLOCK(branch1:_*), BLOCK(branch2: _*))

      def NoElse: IfStmt = IfStmt(b, BLOCK(branch1:_*), BLOCK())

    }

    def IF(b: GExpr)(branch1: GStmt*) = IFBuild(b, branch1)

    def FUNC(name: Symbol, returnType: GTMark)(args: (Symbol, GTMark)*)(body: GStmt*): FuncDef = {
      FuncDef(name, args.toList, returnType, BLOCK(body: _*))
    }
  }

  object API extends GStmtAPI


  /**
    * Allows forward reference to function definitions, but they will not escape their scope.
    * */
  def typeCheckBlock(block: BlockStmt, ctx: ExprContext, returnType: GType): Set[TypeCheckError] = {
    var currentCtx = ctx
    block.stmts.foreach{
      case FuncDef(name, args, newReturn: GType, _) =>
        val signatureType = args.map(_._2.asInstanceOf[GType]) -: newReturn
        currentCtx = currentCtx.newVar(name, signatureType)
      case _ =>
    }
    var currentErrs = Set[TypeCheckError]()
    block.stmts.foreach{ stmt =>
      val (newCtx, errs) = typeCheckStmt(stmt, currentCtx, returnType)
      currentCtx = newCtx
      currentErrs ++= errs
    }
    currentErrs
  }

  /**
    * Allows forward reference to function definitions, but they will not escape their scope.
    * */
  private def typeCheckStmt(stmt: GStmt, ctx: ExprContext, returnType: GType): (ExprContext, Set[TypeCheckError]) = {
    import ctx.typeContext.mkSubTypeError
    stmt match {
      case VarDef(x, ty, init) =>
        ty match {
          case ty: GType =>
            val ctx1 = ctx.newVar(x, ty)
            val (initT, es) = typeCheckInfer(init, ctx1)
            ctx1 -> (es ++ mkSubTypeError(initT, ty))
          case GTHole =>
            val ctx0 = ctx.newVar(x, any)
            val (initT, es) = typeCheckInfer(init, ctx0)
            val ctx1 = ctx.newVar(x, initT)
            ctx1 -> es
        }

      case AssignStmt(x, expr) =>
        val (xT, es) = typeCheckInfer(expr, ctx)
        ctx -> (es ++ mkSubTypeError(xT, ctx.varAssign(x)))
      case ExprStmt(e, isReturn) =>
        val (eT, es) = typeCheckInfer(e, ctx)
        ctx -> (if(isReturn) es ++ mkSubTypeError(eT, returnType) else es)
      case IfStmt(cond, branch1, branch2) =>
        val (condT, e0) = typeCheckInfer(cond, ctx)
        val (_, e1) = typeCheckStmt(branch1, ctx, returnType)
        val (_, e2) = typeCheckStmt(branch2, ctx, returnType)
        ctx -> (e0 ++ e1 ++ e2 ++ mkSubTypeError(condT, GExpr.boolType))
      case WhileStmt(cond, body) =>
        val (condT, e0) = typeCheckInfer(cond, ctx)
        val (_, e1) = typeCheckStmt(body, ctx, returnType)
        ctx -> (e0 ++ e1 ++ mkSubTypeError(condT, GExpr.boolType))
      case FuncDef(_, args, newReturn: GType, body) =>
        val ctxWithArgs = ctx.copy(varAssign =
          ctx.varAssign ++ args.toMap.mapValues(_.asInstanceOf[GType]))
        val (_, errs) = typeCheckStmt(body, ctxWithArgs, newReturn)
        ctx -> errs
      case block: BlockStmt =>
        ctx -> typeCheckBlock(block, ctx, returnType)
      case _ =>
        throw new NotImplementedError(s"GTHoles in Stmt: $stmt")
    }
  }
}
