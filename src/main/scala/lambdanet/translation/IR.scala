package lambdanet.translation

import lambdanet._
import lambdanet.translation.PredicateGraph.{
  PFunc,
  PNode,
  PNodeAllocator,
  PObject,
  PTyVar,
  PType
}
import IR._
import lambdanet.translation.QLang.{QExpr, QModule, QStmt}

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
  case class Cast(expr: PNode, ty: PNode) extends IRExpr {
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

  case class ReturnStmt(v: PNode, returnType: PNode) extends IRStmt

  case class IfStmt(cond: PNode, e1: BlockStmt, e2: BlockStmt) extends IRStmt

  case class WhileStmt(cond: PNode, body: BlockStmt) extends IRStmt

  case class BlockStmt(stmts: Vector[IRStmt]) extends IRStmt

  case class FuncDef(
      funcNode: PNode,
      args: Vector[PNode],
      returnType: PNode,
      body: BlockStmt
  ) extends IRStmt {
    def pType: PFunc = PFunc(args, returnType)
  }

  case class ClassDef(
      classNode: PNode,
      superTypes: Set[PTyVar],
      vars: Map[Symbol, PNode],
      funcDefs: Map[Symbol, FuncDef]
  ) extends IRStmt {
    def pType: PObject = PObject(vars ++ funcDefs.mapValuesNow(_.funcNode))
  }

}

class IRTranslation(allocator: PNodeAllocator) {
  private val mapping: mutable.HashMap[PNode, PAnnot] = mutable.HashMap()

  def fromQModule(module: QModule): IRModule = {
    mapping.clear()
    val stmts1 = module.stmts.flatMap(translateStmt)
    assert(mapping.keySet.intersect(module.mapping.keySet).isEmpty)
    IRModule(module.path, stmts1, module.mapping ++ mapping.toMap)
  }

  private def translateStmt(s: QStmt): Vector[IRStmt] = {
    def translateFunc(f: QLang.FuncDef): FuncDef = {
      val body1 = makeSureInBlock(translateStmt(f.body))
      FuncDef(f.funcNode, f.args, f.returnType, body1)
    }

    s match {
      case QLang.VarDef(v, init, isConst) =>
        val (defs, initE) = translateExpr2(init)
        defs ++ Vector(VarDef(v, initE, isConst))
      case QLang.AssignStmt(lhs, rhs) =>
        val (defs3, lV) = exprAsPNode(lhs)
        val (defs4, rV) = exprAsPNode(rhs)
        (defs3 ++ defs4) :+ AssignStmt(lV, rV)
      case QLang.ReturnStmt(expr, ret) =>
        val (defs2, r) = exprAsPNode(expr)
        defs2 :+ ReturnStmt(r, ret)
      case QLang.ExprStmt(expr) =>
        val (defs2, _) = exprAsPNode(expr)
        defs2
      case QLang.IfStmt(cond, branch1, branch2) =>
        val (condDef2, condV) = exprAsPNode(cond)
        val branch1Stmt = makeSureInBlock(translateStmt(branch1))
        val branch2Stmt = makeSureInBlock(translateStmt(branch2))
        val ifStmt = IfStmt(condV, branch1Stmt, branch2Stmt)
        condDef2 :+ ifStmt
      case QLang.WhileStmt(cond, body) =>
        val (condDef2, condV) = exprAsPNode(cond)
        // recompute the conditional expression value at the end of the loop
        val condCompute =
          condDef2.filterNot(_.isInstanceOf[VarDef])
        val bodyStmt = makeSureInBlock(translateStmt(body) ++ condCompute)
        condDef2 :+ WhileStmt(condV, bodyStmt)
      case b: QLang.BlockStmt =>
        Vector(makeSureInBlock(b.stmts.flatMap(translateStmt)))
      case f: QLang.FuncDef =>
        Vector(translateFunc(f))
      case QLang.ClassDef(classNode, superType, vars, funcDefs) =>
        Vector(
          ClassDef(
            classNode,
            superType,
            vars,
            funcDefs.mapValuesNow(translateFunc)
          )
        )
    }
  }

  /**
    * Translate an expr from the QLang language into an [[IRExpr]] in IR,
    * possibly generating some extra definitional statements
    */
  def translateExpr2(expr: QExpr): (Vector[IRStmt], IRExpr) = {
    import cats.data.Writer
    import cats.syntax.all._
    import cats.instances.all._

    type W[T] = Writer[Vector[IRStmt], T]

    def rec(expr: QExpr): W[IRExpr] = {
      def asVar(expr: QExpr): W[PNode] = {
        val (stmts, v) = exprAsPNode(expr)
        v.writer(stmts)
      }

      def pure(ex: IRExpr): W[IRExpr] = ex.pure[W]

      expr match {
        case QLang.Var(n) => pure(Var(n))
        case QLang.FuncCall(f, args) =>
          val fVar = asVar(f)
          val argsVars = args.map(arg => asVar(arg)).sequence
          for {
            fV <- fVar
            argsV <- argsVars
          } yield IR.FuncCall(fV, argsV)
        case QLang.Cast(e, ty) =>
          val node = allocator.newNode(None, isType = false)
          asVar(e).map { v =>
            if (v.fromProject) {
              mapping(node) = Annot.Fixed(ty)
            }
            Cast(v, node)
          }
        case QLang.ObjLiteral(fields) =>
          fields.toVector
            .map { case (k, e) => asVar(e).map(k -> _) }
            .sequence
            .map(_.toMap.pipe(ObjLiteral))
        case QLang.Access(receiver, field) =>
          asVar(receiver).map(FieldAccess(_, field))
        case QLang.IfExpr(cond, e1, e2) =>
          for {
            c <- asVar(cond)
            e1V <- asVar(e1)
            e2V <- asVar(e2)
          } yield IfExpr(c, e1V, e2V)
      }
    }

    rec(expr).run
  }

  private def exprAsPNode(qExpr: QExpr): (Vector[IRStmt], PNode) = {
    val (defs, expr) = translateExpr2(qExpr)
    val (defs2, n) = expr match {
      case v: Var => (Vector(), v.node)
      case _ =>
        val v = allocator.newNode(None, isType = false)
        mapping(v) = qExpr.tyAnnot
        Vector(
          VarDef(
            v,
            expr,
            isConst = true
          )
        ) -> v
    }
    (defs ++ defs2, n)
  }

}
