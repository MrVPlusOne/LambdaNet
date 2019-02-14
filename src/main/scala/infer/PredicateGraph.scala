package infer

import funcdiff.SimpleMath.Extensions._
import gtype.{GType, JSExamples, TyVar}
import infer.IR._
import infer.IRTranslation.TranslationEnv

/** Encodes the relationships between different type variables */
object PredicateGraph {

  sealed trait TyVarPredicate

  case class EqualityRel(v1: IRType, v2: IRType) extends TyVarPredicate
  case class FreezeType(v: IRType, ty: GType) extends TyVarPredicate
  case class HasName(v: IRType, name: Symbol) extends TyVarPredicate
  case class SubtypeRel(sub: IRType, sup: IRType) extends TyVarPredicate
  case class AssignRel(lhs: IRType, rhs: IRType) extends TyVarPredicate
  case class UsedAsBoolean(tyVar: IRType) extends TyVarPredicate
  case class InheritanceRel(child: IRType, parent: IRType) extends TyVarPredicate
  case class DefineRel(v: IRType, expr: TypeExpr) extends TyVarPredicate

  sealed trait TypeExpr
  case class FuncTypeExpr(argTypes: List[IRType], returnType: IRType) extends TypeExpr
  case class CallTypeExpr(f: IRType, args: List[IRType]) extends TypeExpr
  case class ObjLiteralTypeExpr(fields: Map[Symbol, IRType]) extends TypeExpr
  case class FieldAccessTypeExpr(objType: IRType, field: Symbol) extends TypeExpr

  case class PredicateContext(
    varTypeMap: Map[Var, IRType],
    newTypeMap: Map[Symbol, IRType]
  )

  val returnVar: Var = namedVar(gtype.GStmt.returnSymbol)

  object PredicateContext {
    val empty: PredicateContext =
      PredicateContext(Map(), Map())

    def jsCtx(env: TranslationEnv): PredicateContext = {
      val typeMap = JSExamples.exprContext.varAssign.map {
        case (s, t) => namedVar(s) -> env.newTyVar(None, Some(s), Some(t))
      }
//      val objDef = JSExamples.typeContext.typeUnfold.keys
//        .map(s => s -> env.newTyVar(None, Some(s), Some(TyVar(s))))
//        .toMap
      PredicateContext(typeMap, Map())
    }
  }

  def encodeIR(
    stmts: Vector[IRStmt],
    ctx: PredicateContext
  ): (Vector[TyVarPredicate], PredicateContext) = {
    import collection.mutable

    val relations = mutable.ListBuffer[TyVarPredicate]()

    def add(rel: TyVarPredicate): Unit = {
      relations += rel
    }

    /** Collect the variable, function, and class definitions declared in a
      * block and return a new context */
    def collectDefinitions(
      stmts: Vector[IRStmt]
    )(implicit ctx: PredicateContext): PredicateContext = {
      val classDefs = stmts.collect {
        case c: ClassDef => c.name -> c.classT
      }

      val defs = stmts.collect {
        case VarDef(v, tyVar, _) => v -> tyVar
        case f: FuncDef          => namedVar(f.name) -> f.funcT
        case c: ClassDef         => namedVar(c.constructor.name) -> c.constructor.funcT
      }

      ctx.copy(
        varTypeMap = ctx.varTypeMap ++ defs,
        newTypeMap = ctx.newTypeMap ++ classDefs
      )
    }

    def encodeStmt(stmt: IRStmt)(implicit ctx: PredicateContext): Unit = {
      import ctx._

      stmt match {
        case VarDef(v, _, rhs) =>
          // don't need to modify ctx here, collect definitions when processing blocks
          val tv = varTypeMap(v)
          rhs match {
            case v1: Var =>
              add(EqualityRel(tv, varTypeMap(v1)))
            case Const(_, ty) =>
              add(FreezeType(tv, ty))
            case FuncCall(f, args) =>
              add(DefineRel(tv, CallTypeExpr(varTypeMap(f), args.map(varTypeMap))))
            case ObjLiteral(fields) =>
              add(DefineRel(tv, ObjLiteralTypeExpr(fields.mapValuesNow(varTypeMap))))
            case FieldAccess(receiver, label) =>
              add(DefineRel(tv, FieldAccessTypeExpr(varTypeMap(receiver), label)))
            case IfExpr(cond, e1, e2) =>
              add(SubtypeRel(varTypeMap(e1), tv))
              add(SubtypeRel(varTypeMap(e2), tv))
              add(UsedAsBoolean(varTypeMap(cond)))
          }
        case Assign(lhs, rhs) =>
          add(SubtypeRel(varTypeMap(rhs), varTypeMap(lhs)))
        case ReturnStmt(v) =>
          add(SubtypeRel(varTypeMap(v), ctx.varTypeMap(returnVar)))
        case IfStmt(cond, e1, e2) =>
          add(UsedAsBoolean(varTypeMap(cond)))
          encodeStmt(e1)
          encodeStmt(e2)
        case WhileStmt(cond, body) =>
          add(UsedAsBoolean(varTypeMap(cond)))
          encodeStmt(body)
        case block: BlockStmt =>
          val innerCtx = collectDefinitions(block.stmts)
//          println(s"Inner context: $innerCtx")
          block.stmts.foreach(s => encodeStmt(s)(innerCtx))
        case FuncDef(_, args, newReturnType, body, funcT) =>
          val innerCtx =
            collectDefinitions(body)(
              ctx.copy(
                varTypeMap = (ctx.varTypeMap ++ args) + (returnVar -> newReturnType)
              )
            )
          add(DefineRel(funcT, FuncTypeExpr(args.map(_._2), newReturnType)))
          body.foreach(s => encodeStmt(s)(innerCtx))
        case ClassDef(_, superType, constructor, vars, funcDefs, classT) =>
          superType.foreach { n =>
            val parentType = ctx.newTypeMap(n)
            add(InheritanceRel(classT, parentType))
          }
          val methods = funcDefs.map(f => f.name -> f.funcT)
          val objExpr = ObjLiteralTypeExpr(vars ++ methods)

          add(DefineRel(classT, objExpr))

          val innerCtx =
            ctx.copy(
              varTypeMap = ctx.varTypeMap + (ClassDef.thisVar -> classT)
            )
          (constructor +: funcDefs).foreach(s => encodeStmt(s)(innerCtx))
      }
    }

    encodeStmt(BlockStmt(stmts))(ctx)
    relations.toVector -> collectDefinitions(stmts)(ctx)
  }

  def encodeUnaryPredicates(vars: Iterable[IRType]): Vector[TyVarPredicate] = {
    import collection.mutable
    var newPredicates = mutable.ListBuffer[TyVarPredicate]()
    vars.foreach { tv =>
      tv.freezeToType.foreach(t => newPredicates += FreezeType(tv, t))
      tv.name.foreach(n => newPredicates += HasName(tv, n))
    }
    newPredicates.toVector
  }
}
