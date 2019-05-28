package lambdanet.translation

import lambdanet.{IdAllocator, ProjectPath}
import lambdanet.translation.IR._
import lambdanet.translation.PredicateGraph._
import PredicatesGeneration._
import funcdiff.SimpleMath
import lambdanet.surface.GModule
import lambdanet.translation.ImportsResolution.ModuleExports
import lambdanet.translation.PredicateGraph.PVar.PVarAllocator
import lambdanet.types.{GType, GroundType}
import SimpleMath.Extensions._
import lambdanet.translation.OldPredicateGraph.ObjLiteralTypeExpr

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
  }
}

class PredicatesGeneration {

  def createPredicateGraph(
      modules: Map[ProjectPath, GModule],
      libraryModules: Map[ProjectPath, ModuleExports],
      pathMapping: ImportsResolution.PathMapping
  ): PredicateGraph = {
    val pVarAllocator = new PVarAllocator()
    val importedContexts =
      ImportsResolution.resolveImports(
        Left(pVarAllocator),
        modules,
        libraryModules,
        pathMapping
      )

    val libCounter = new UsageCounter()
    import libCounter._

    def encodeStmts(stmts: Vector[IRStmt], ctx: PContext) = {
      val predicates = mutable.Set[TyPredicate]()

      def add(pred: TyPredicate): Unit = {
        predicates += pred
      }

      def collectDefinitinos(
          stmts: Vector[IRStmt]
      )(implicit ctx: PContext): PContext = {
        ???
      }

      def encodeStmt(stmt: IRStmt)(implicit ctx: PContext): Unit =
        SimpleMath.withErrorMessage(s"--->\n${stmt.prettyPrint()}") {

          stmt match {
            case d: VarDef =>
              val leftNode = useTerm(d.v)
              val rightNode = d.rhs match {
                case v1: Var => useTerm(v1)
                case FuncCall(f, args) =>
                  PCall(useTerm(f), args.map(useTerm))
                case ObjLiteral(fields) =>
                  PObject(fields.mapValuesNow(useTerm))
                case FieldAccess(receiver, label) =>
              }
          }

        }

    }

    ???
  }

}
