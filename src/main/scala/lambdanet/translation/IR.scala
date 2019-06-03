package lambdanet.translation

import lambdanet._
import lambdanet.translation.PredicateGraph.{PNode, PNodeAllocator, PType}
import types._
import IR._
import lambdanet.translation.QLang.{QExpr, QModule, QStmt}
import funcdiff.SimpleMath.Extensions._

import scala.collection.mutable

object IR {
  type VarName = Symbol
  type TypeName = Symbol

  case class IRModule(
      path: ProjectPath,
      stmts: Vector[IRStmt],
      mapping: Map[PNode, PAnnot]
  ) {
    // todo: move this to QLang
//    def moduleStats: IRModuleStats = {
//      var fieldsUsed, fieldsDefined: Set[Symbol] = Set()
//
//      /** collects fields definitions and usages */
//      def processExpr(expr: IRExpr): Unit = expr match {
//        case ObjLiteral(fields) =>
//          fieldsDefined ++= fields.keySet
//        case FieldAccess(_, label) =>
//          fieldsUsed += label
//        case _ =>
//      }
//
//      /** collects fields definitions and usages */
//      def processStmt(stmt: IRStmt): Unit = stmt match {
//        case s: VarDef => processExpr(s.rhs)
//        case s: IfStmt =>
//          processStmt(s.e1)
//          processStmt(s.e2)
//        case s: WhileStmt => processStmt(s.body)
//        case s: BlockStmt => s.stmts.foreach(processStmt)
//        case s: FuncDef   => s.body.stmts.foreach(processStmt)
//        case s: ClassDef =>
//          s.funcDefs.foreach(processStmt)
//          fieldsDefined ++= s.funcDefs.map(_.name)
//          fieldsDefined ++= s.vars.keySet
//        case _ =>
//      }
//
//      stmts.foreach(processStmt)
//
//      IRModuleStats(fieldsUsed, fieldsDefined)
//    }
  }

  case class IRModuleStats(
      fieldsUsed: Set[Symbol],
      fieldsDefined: Set[Symbol]
  )

  // @formatter:off
  /** a simple expression
    *
    *  e :=                         ([[IRExpr]])
    *     | x                       ([[Var]])
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

  case class Var(node: PNode) extends IRExpr {
    val nameOpt: Option[Symbol] = node.nameOpt

    def prettyPrint: String = node.toString
  }

  case class FuncCall(f: PNode, args: Vector[PNode]) extends IRExpr {
    def prettyPrint: String = s"$f${args.mkString("(", ", ", ")")}"
  }
  case class ObjLiteral(fields: Map[Symbol, PNode]) extends IRExpr {
    def prettyPrint: String =
      fields.map { case (f, v) => s"$f: $v" }.mkString("{", ", ", "}")
  }
  case class FieldAccess(receiver: PNode, label: Symbol) extends IRExpr {
    def prettyPrint: String = s"$receiver.${label.name}"
  }
  case class IfExpr(cond: PNode, e1: PNode, e2: PNode) extends IRExpr {
    def prettyPrint: String = s"($cond ? $e1 : $e2)"
  }
  case class Cast(expr: PNode, ty: PType) extends IRExpr {
    def prettyPrint: String = s"($expr as $ty)"
  }
  // @formatter:off
  /**
    *
    * a statement in Single Assignment Form
    *
    * S :=                                  ([[IRStmt]])
    *   | var x: α = e                      ([[VarDef]])
    *   | x := x                            ([[AssignStmt]])
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
    *       α is [[GTHole]],
    *       e is [[IRExpr]],
    *       f is [[FuncDef]]
    * */
  // @formatter:on
  sealed trait IRStmt {
//    def prettyPrint(indentSpaces: Int = 2): String = {
//      IRStmt
//        .prettyPrintHelper(0, this)
//        .map {
//          case (indent, text) => " " * (indent * indentSpaces) + text
//        }
//        .mkString("\n")
//    }
//
//    override def toString: String = prettyPrint()
  }

  case class VarDef(
      node: PNode,
      rhs: IRExpr,
      isConst: Boolean
  ) extends IRStmt

  case class AssignStmt(lhs: PNode, rhs: PNode) extends IRStmt

  case class ReturnStmt(v: PNode) extends IRStmt

  case class IfStmt(cond: PNode, e1: BlockStmt, e2: BlockStmt) extends IRStmt

  case class WhileStmt(cond: PNode, body: BlockStmt) extends IRStmt

  case class BlockStmt(stmts: Vector[IRStmt]) extends IRStmt

  case class FuncDef(
      funcNode: PNode,
      args: Vector[PNode],
      returnType: PNode,
      body: BlockStmt
  ) extends IRStmt

  case class ClassDef(
      classNode: PNode,
      superType: Option[PType],
      vars: Set[PNode],
      funcDefs: Vector[FuncDef]
  ) extends IRStmt

}

class IRTranslation(allocator: PNodeAllocator) {
  def fromQModule(module: QModule): IRModule = {
    val stmts1 = module.stmts.flatMap(translateStmt)
    IRModule(module.path, stmts1, module.mapping)
  }

  def translateStmt(s: QStmt): Vector[IRStmt] = {
    def translateFunc(f: QLang.FuncDef): FuncDef = {
      val body1 = groupInBlock(translateStmt(f.body))
      FuncDef(f.funcNode, f.args, f.returnType, body1)
    }

    s match {
      case QLang.VarDef(v, init, isConst) =>
        val (defs, initE) = translateExpr2(init)
        defs ++ Vector(VarDef(v, initE, isConst))
      case QLang.AssignStmt(lhs, rhs) =>
        val (lDefs, lE) = translateExpr2(lhs)
        val (rDefs, rE) = translateExpr2(rhs)
        val (defs3, lV) = exprAsPNode(lE)
        val (defs4, rV) = exprAsPNode(rE)
        (lDefs ++ rDefs ++ defs3 ++ defs4) :+ AssignStmt(lV, rV)
      case QLang.ExprStmt(expr, isReturn) =>
        val (defs, e) = translateExpr2(expr)
        val (defs2, r) = exprAsPNode(e)
        if (isReturn) defs ++ defs2 :+ ReturnStmt(r)
        else defs ++ defs2
      case QLang.IfStmt(cond, branch1, branch2) =>
        val (condDef, condE) = translateExpr2(cond)
        val (condDef2, condV) = exprAsPNode(condE)
        val branch1Stmt = groupInBlock(translateStmt(branch1))
        val branch2Stmt = groupInBlock(translateStmt(branch2))
        val ifStmt = IfStmt(condV, branch1Stmt, branch2Stmt)
        condDef ++ condDef2 :+ ifStmt
      case QLang.WhileStmt(cond, body) =>
        val (condDef, condE) = translateExpr2(cond)
        val (condDef2, condV) = exprAsPNode(condE)
        // recompute the conditional expression value at the end of the loop
        val condCompute =
          (condDef ++ condDef2).filterNot(_.isInstanceOf[VarDef])
        val bodyStmt = groupInBlock(translateStmt(body) ++ condCompute)
        condDef ++ condDef2 :+ WhileStmt(condV, bodyStmt)
      case b: QLang.BlockStmt =>
        Vector(groupInBlock(b.stmts.flatMap(translateStmt)))
      case f: QLang.FuncDef =>
        Vector(translateFunc(f))
      case QLang.ClassDef(classNode, superType, vars, funcDefs) =>
        Vector(
          ClassDef(
            classNode,
            superType,
            vars,
            funcDefs.map(translateFunc)
          )
        )
    }
  }

  /**
    * Translate an expr from the QLang language into an [[IRExpr]] in IR,
    * possibly generating some extra definitional statements
    */
  def translateExpr2(expr: QExpr): (Vector[IRStmt], IRExpr) = {
    def rec(expr: QExpr)(
        implicit defs: mutable.ListBuffer[IRStmt]
    ): IRExpr = {
      def asVar(expr: IRExpr): PNode = {
        val (stmts, v) = exprAsPNode(expr)
        defs.appendAll(stmts)
        v
      }

      expr match {
        case QLang.Var(n) => Var(n)
        case QLang.FuncCall(f, args) =>
          val fVar = asVar(rec(f))
          val argsVars = args.map(e => asVar(rec(e)))
          IR.FuncCall(fVar, argsVars)
        case QLang.Cast(e, ty) =>
          Cast(asVar(rec(e)), ty)
        case QLang.ObjLiteral(fields) =>
          ObjLiteral(fields.mapValuesNow(e => asVar(rec(e))))
        case QLang.Access(receiver, field) =>
          val v = asVar(rec(receiver))
          FieldAccess(v, field)
        case QLang.IfExpr(cond, e1, e2) =>
          val condV = asVar(rec(cond))
          val e1V = asVar(rec(e1))
          val e2V = asVar(rec(e2))
          IfExpr(condV, e1V, e2V)
      }

    }

    val defs = mutable.ListBuffer[IRStmt]()
    val e1 = rec(expr)(defs)
    (defs.toVector, e1)
  }

  private def exprAsPNode(expr: IRExpr): (Vector[IRStmt], PNode) =
    expr match {
      case v: Var => (Vector(), v.node)
      case _ =>
        val v = allocator.newNode(None, isType = false)
        Vector(
          VarDef(
            v,
            expr,
            isConst = true
          )
        ) -> v
    }

}
