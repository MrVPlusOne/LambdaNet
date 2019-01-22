package infer

import funcdiff.SimpleMath.Extensions._
import gtype.GType
import infer.IR._

/** Encodes the relationships between different type variables */
object RelationGraph {

  sealed trait TyVarRelation

  case class EqualityRel(v1: IRType, v2: IRType) extends TyVarRelation
  case class FreezeType(v: IRType, ty: GType) extends TyVarRelation
  case class SubtypeRel(sub: IRType, sup: IRType) extends TyVarRelation
  case class AssignRel(lhs: IRType, rhs: IRType) extends TyVarRelation
  case class UsedAsBoolean(tyVar: IRType) extends TyVarRelation
  case class DefineRel(v: IRType, expr: TypeExpr) extends TyVarRelation

  sealed trait TypeExpr
  case class CallTypeExpr(f: IRType, args: List[IRType]) extends TypeExpr
  case class ObjLiteralTypeExpr(fields: Map[Symbol, IRType]) extends TypeExpr
  case class FieldAccessTypeExpr(objType: IRType, field: Symbol) extends TypeExpr

  def encodeIR(stmts: Vector[IRStmt], ctx: IRCtx): List[TyVarRelation] = {
    import collection.mutable

    /** Collect the variable and function definitions declared in a
      * block and return a new context */
    def collectDefinitions(stmts: Vector[IRStmt])(implicit ctx: IRCtx): IRCtx = {
      val classDefs = mutable.Map[Symbol, ObjectIRType]()
      def parseClassDef(c: ClassDef): Unit = {
        // fixme: need to handle cases where the child type's definition being processed first
        val superFields = c.superType
          .map { n =>
            classDefs.get(n).map(ir => Left(ir)).getOrElse(ctx.objectDefs(n)) match {
              case Left(ir)   => ir.fields
              case Right(obj) => obj.fields.mapValuesNow { ConcreteIRType }
            }
          }
          .getOrElse(Map())
        val methods = c.funcDefs.map { f =>
          f.name -> f.funcT
        }
        classDefs(c.name) = ObjectIRType(superFields ++ c.vars ++ methods)
      }

      stmts.collect {
        case c: ClassDef => parseClassDef(c)
      }

      val defs = stmts.collect {
        case VarDef(v, tyVar, _) => v -> tyVar
        case f: FuncDef          => Var(f.name) -> f.funcT
        case c: ClassDef =>
          val constructor =
            FuncIRType(c.constructor.args.map(_._2), ConcreteIRType(gtype.TyVar(c.name)))
          Var(c.constructor.name) -> constructor
      }

      ctx.copy(
        typeMap = ctx.typeMap ++ defs,
        objectDefs = ctx.objectDefs ++ classDefs.toMap.mapValuesNow(Left(_))
      )
    }

    val relations = mutable.ListBuffer[TyVarRelation]()

    def add(rel: TyVarRelation): Unit = {
      relations += rel
    }

    var substitution = Map[VarIRType, IRType]()
    //noinspection TypeAnnotation
    implicit val substituteRule = new SubstituteRule[VarIRType, IRType] {
      def substitute(v: IRType, sub: Map[VarIRType, IRType]): IRType =
        IRType.replaceLeaves(v, sub.getOrElse(_, v))
    }

    def encodeStmt(stmt: IRStmt)(implicit ctx: IRCtx): Unit = {
      import ctx._

      stmt match {
        case VarDef(v, _, rhs) =>
          // don't need to modify ctx here, collect definitions when processing blocks
          val tv = typeMap(v)
          rhs match {
            case v1: Var =>
              add(EqualityRel(tv, typeMap(v1)))
            case Const(_, ty) =>
              add(FreezeType(tv, ty))
            case FuncCall(f, args) =>
              add(DefineRel(tv, CallTypeExpr(typeMap(f), args.map(typeMap))))
            case ObjLiteral(fields) =>
              add(DefineRel(tv, ObjLiteralTypeExpr(fields.mapValuesNow(typeMap))))
            case FieldAccess(receiver, label) =>
              add(DefineRel(tv, FieldAccessTypeExpr(typeMap(receiver), label)))
            case IfExpr(cond, e1, e2) =>
              add(SubtypeRel(typeMap(e1), tv))
              add(SubtypeRel(typeMap(e2), tv))
              add(UsedAsBoolean(typeMap(cond)))
          }
        case Assign(lhs, rhs) =>
          add(SubtypeRel(typeMap(rhs), typeMap(lhs)))
        case ReturnStmt(v) =>
          add(SubtypeRel(typeMap(v), ctx.returnType))
        case IfStmt(cond, e1, e2) =>
          add(UsedAsBoolean(typeMap(cond)))
          encodeStmt(e1)
          encodeStmt(e2)
        case WhileStmt(cond, body) =>
          add(UsedAsBoolean(typeMap(cond)))
          encodeStmt(body)
        case block: BlockStmt =>
          val innerCtx = collectDefinitions(block.stmts)
//          println(s"Inner context: $innerCtx")
          block.stmts.foreach(s => encodeStmt(s)(innerCtx))
        case FuncDef(_, args, newReturnType, body) =>
          val innerCtx =
            collectDefinitions(body)(
              ctx.copy(typeMap = ctx.typeMap ++ args, returnType = newReturnType)
            )
          body.foreach(s => encodeStmt(s)(innerCtx))
        case ClassDef(name, _, constructor, _, funcDefs) =>
          val innerCtx =
            ctx.copy(
              typeMap = ctx.typeMap + (ClassDef.thisVar -> ConcreteIRType(
                gtype.TyVar(name)
              ))
            )
          (constructor +: funcDefs).foreach(s => encodeStmt(s)(innerCtx))
      }
    }

    encodeStmt(BlockStmt(stmts))(ctx)
    println("Constraints")
    relations.toList
  }
}
