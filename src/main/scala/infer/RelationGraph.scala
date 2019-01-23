package infer

import funcdiff.SimpleMath.Extensions._
import gtype.{GType, JSExamples}
import infer.IR._

/** Encodes the relationships between different type variables */
object RelationGraph {

  sealed trait TyVarRelation

  case class EqualityRel(v1: IRType, v2: IRType) extends TyVarRelation
  case class FreezeType(v: IRType, ty: GType) extends TyVarRelation
  case class SubtypeRel(sub: IRType, sup: IRType) extends TyVarRelation
  case class AssignRel(lhs: IRType, rhs: IRType) extends TyVarRelation
  case class UsedAsBoolean(tyVar: IRType) extends TyVarRelation
  case class InheritanceRel(child: IRType, parent: IRType) extends TyVarRelation
  case class DefineRel(v: IRType, expr: TypeExpr) extends TyVarRelation

  sealed trait TypeExpr
  case class FuncTypeExpr(argTypes: List[IRType], returnType: IRType) extends TypeExpr
  case class CallTypeExpr(f: IRType, args: List[IRType]) extends TypeExpr
  case class ObjLiteralTypeExpr(fields: Map[Symbol, IRType]) extends TypeExpr
  case class FieldAccessTypeExpr(objType: IRType, field: Symbol) extends TypeExpr

  case class EncodingCtx(
    typeMap: Map[Var, IRType],
    returnType: IRType,
    objectMap: Map[Symbol, IRType]
  )

  object EncodingCtx {
    val empty: EncodingCtx =
      EncodingCtx(Map(), KnownType(GType.voidType), Map())
    val jsCtx: EncodingCtx = {
      val typeMap = JSExamples.exprContext.varAssign.map {
        case (s, t) => namedVar(s) -> KnownType(t)
      }
      val objDef = JSExamples.typeContext.typeUnfold.mapValuesNow(KnownType)
      EncodingCtx(typeMap, KnownType(GType.voidType), objDef)
    }
  }

  def encodeIR(stmts: Vector[IRStmt], ctx: EncodingCtx): List[TyVarRelation] = {
    import collection.mutable

    val relations = mutable.ListBuffer[TyVarRelation]()

    def add(rel: TyVarRelation): Unit = {
      relations += rel
    }

    /** Collect the variable, function, and class definitions declared in a
      * block and return a new context */
    def collectDefinitions(
      stmts: Vector[IRStmt]
    )(implicit ctx: EncodingCtx): EncodingCtx = {
      val classDefs = stmts.collect {
        case c: ClassDef => c.name -> c.classT
      }

      val defs = stmts.collect {
        case VarDef(v, tyVar, _) => v -> tyVar
        case f: FuncDef          => namedVar(f.name) -> f.funcT
        case c: ClassDef         => namedVar(c.constructor.name) -> c.constructor.funcT
      }

      ctx.copy(typeMap = ctx.typeMap ++ defs, objectMap = ctx.objectMap ++ classDefs)
    }

    def encodeStmt(stmt: IRStmt)(implicit ctx: EncodingCtx): Unit = {
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
        case FuncDef(_, args, newReturnType, body, funcT) =>
          val innerCtx =
            collectDefinitions(body)(
              ctx.copy(typeMap = ctx.typeMap ++ args, returnType = newReturnType)
            )
          add(DefineRel(funcT, FuncTypeExpr(args.map(_._2), newReturnType)))
          body.foreach(s => encodeStmt(s)(innerCtx))
        case ClassDef(_, superType, constructor, vars, funcDefs, classT) =>
          superType.foreach { n =>
            val parentType = ctx.objectMap(n)
            add(InheritanceRel(classT, parentType))
          }
          val methods = funcDefs.map(f => f.name -> f.funcT)
          val objExpr = ObjLiteralTypeExpr(vars ++ methods)

          add(DefineRel(classT, objExpr))

          val innerCtx =
            ctx.copy(
              typeMap = ctx.typeMap + (ClassDef.thisVar -> classT)
            )
          (constructor +: funcDefs).foreach(s => encodeStmt(s)(innerCtx))
      }
    }

    encodeStmt(BlockStmt(stmts))(ctx)
    println("Constraints")
    relations.toList
  }
}
