package lambdanet.translation

import lambdanet.{Annot, GroundType, ProjectPath}
import lambdanet.translation.IR._
import lambdanet.translation.PredicateGraph._
import PredicatesGeneration._
import funcdiff.SimpleMath
import lambdanet.surface.GModule
import lambdanet.translation.ImportsResolution.ModuleExports
import lambdanet.translation.PredicateGraph.PVar.PVarAllocator
import SimpleMath.Extensions._

import scala.collection.mutable

object PredicatesGeneration {

  case class PContext(
      terms: Map[Var, PNode],
      types: Map[TypeName, PNode],
      nameSpaces: Map[Symbol, PContext]
  ) {
    def termVar(v: Var): PVar = terms(v).asInstanceOf[PVar]
  }

  class UsageCounter {
    import cats.implicits._

    var usages: Map[PNode, Int] = Map()

    def useContType(ty: GroundType)(implicit ctx: PContext): PConst = {
      ctx.types(ty.id).asInstanceOf[PConst]
    }

    def useTerm(term: Var)(implicit ctx: PContext): PNode = {
      val t = ctx.terms(term)
      usages |+|= Map(t -> 1)
      t
    }

    def useNode(n: PNode): PNode = {
      usages |+|= Map(n -> 1)
      n
    }
  }
}

class PredicatesGeneration {

//  def createPredicateGraph(
//      modules: Map[ProjectPath, GModule],
//      libraryModules: Map[ProjectPath, ModuleExports],
//      pathMapping: ImportsResolution.PathMapping
//  ): PredicateGraph = {
//    val pVarAllocator = new PVarAllocator()
//
//    PLangTranslation.fromGModule
//
//    val importedContexts =
//      ImportsResolution.resolveImports(
//        Left(pVarAllocator),
//        modules,
//        libraryModules,
//        pathMapping
//      )
//
//    val libCounter = new UsageCounter()
//    import libCounter._
//
//    def encodeStmts(stmts: Vector[IRStmt], ctx: PContext) = {
//      val predicates = mutable.Set[TyPredicate]()
//
//      def add(pred: TyPredicate): Unit = {
//        predicates += pred
//      }
//
//      def useCond(cond: Var)(implicit ctx: PContext): Unit = {
//        useTerm(cond) match {
//          case pv: PVar => add(UsedAsBool(pv))
//          case _        =>
//        }
//      }
//
//      def collectDefinitions(
//          stmts: Vector[IRStmt]
//      )(implicit ctx: PContext): PContext = {
//        ???
//      }
//
//      // stores all variables that are associated with namespace access
//      val namespaceAccess = mutable.HashMap[PNode, PContext]()
//
//      def encodeStmt(stmt: IRStmt)(implicit ctx: PContext): Unit =
//        SimpleMath.withErrorMessage(s"--->\n${stmt.prettyPrint()}") {
//          stmt match {
//            case d: VarDef =>
//              val leftNode = useTerm(d.v).asInstanceOf[PVar]
//
//              def define(rhs: PExpr): Unit = {
//                add(DefineRel(leftNode, rhs))
//              }
//
//              d.rhs match {
//                case v1: Var =>
//                  define(useTerm(v1))
//                case FuncCall(f, args) =>
//                  define(PCall(useTerm(f), args.map(useTerm)))
//                case ObjLiteral(fields) =>
//                  define(PObject(fields.mapValuesNow(useTerm)))
//                case IfExpr(cond, e1, e2) =>
//                  add(SubtypeRel(useTerm(e1), leftNode))
//                  add(SubtypeRel(useTerm(e2), leftNode))
//                  useCond(cond)
//                case FieldAccess(receiver, label) =>
//                  val r = useTerm(receiver)
//                  (namespaceAccess.get(r) match {
//                    case Some(ex) => // already part of an access
//                      ex.nameSpaces.get(label) match {
//                        case Some(ex1) =>
//                          namespaceAccess(leftNode) = ex1
//                          None
//                        case None =>
//                          Some(useNode(ex.terms(label)))
//                      }
//                    case None =>
//                      receiver.nameOpt match {
//                        case Some(name) if ctx.nameSpaces.contains(name) =>
//                          namespaceAccess(leftNode) = ctx.nameSpaces(name)
//                          None
//                        case _ =>
//                          Some(PAccess(r, label))
//                      }
//                  }).foreach(define)
//              }
//            case Assign(lhs, rhs) =>
//              add(AssignRel(useTerm(lhs), useTerm(rhs)))
//            case ReturnStmt(v) =>
//              add(SubtypeRel(useTerm(v), useTerm(returnVar)))
//            case IfStmt(cond, e1, e2) =>
//              useCond(cond)
//              encodeStmt(e1)
//              encodeStmt(e2)
//            case WhileStmt(cond, body) =>
//              useCond(cond)
//              encodeStmt(body)
//            case block: BlockStmt =>
//              val innerCtx = collectDefinitions(block.stmts)
//              block.stmts.foreach(s => encodeStmt(s)(innerCtx))
//            case FuncDef(name, args, newReturnType, body, _) =>
//              val ctx1 = ctx.copy(
//                terms = {
//                  val map1 = (ctx.terms ++ args.map(p => p._1 -> pVarAllocator.newVar()p._2)) + (returnVar -> newReturnType)
//                  // special rule for constructors: 'this' has its return type
//                  if (isConstructor)
//                    map1 ++ Seq(
//                      ClassDef.thisVar -> newReturnType,
//                      ClassDef.superVar -> newReturnType
//                    )
//                  else map1
//                }
//              )
//              val fNode = useTerm(namedVar(name)).asInstanceOf[PVar]
//              add(DefineRel(fNode, PFunc(args.map(_._2), newReturnType)))
//              encodeStmt(body)(ctx1)
//            case ClassDef(_, superType, vars, funcDefs, classT, _, _) =>
//              vars.values.foreach(recordLabel)
//
//              val superMap = superType.map { n =>
//                val parentType = getTypeFromName(ctx, n)
//                add(InheritanceRel(classT, parentType))
//                ClassDef.superVar -> parentType
//              }
//              val methods = funcDefs.map(f => f.name -> f.funcT)
//              val objExpr = ObjLiteralTypeExpr(vars ++ methods)
//
//              add(DefineRel(classT, objExpr))
//
//              val innerCtx =
//                ctx.copy(
//                  useTerm = ctx.useTerm + (ClassDef.thisVar -> classT) ++ superMap.toList
//                )
//              funcDefs.foreach(s => encodeStmt(s)(innerCtx))
//            case _: TypeAliasIRStmt => //do nothing
//
//          }
//        }
//
//    }
//
//    ???
//  }

}
