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

import scala.collection.mutable

object PredicatesGeneration {

  case class PContext(
      terms: Map[Var, PNode],
      types: Map[TypeName, PNode],
      nameSpaces: Map[Symbol, PContext]
  ) {
    def termVar(v: Var): PVar = terms(v).asInstanceOf[PVar]
  }

  class LibUsageCounter {
    import cats.implicits._

    var usages: Map[PConst, Int] = Map()

    def useContType(ty: GroundType)(implicit ctx: PContext): PConst = {
      ctx.types(ty.id).asInstanceOf[PConst]
    }

    def useType(ty: GType)(implicit ctx: PContext): Unit = ty match {
      case ground: GroundType =>
//        val t = ctx.types(ground.id).which(_.isType)
//        usages |+|= Map(t -> 1)
      case _ => ???
    }

    def useTerm(term: PConst): Unit = {
      assert(term.isTerm)
      usages |+|= Map(term -> 1)
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

    val libCounter = new LibUsageCounter()
    import libCounter.useContType

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
          import ctx._

          stmt match {
            case d: VarDef =>
              val leftNode = termVar(d.v)
              val rightNode = d.rhs match {
                case v1: Var =>
                  terms(v1)
                case Const(_, ty) =>
                  useContType(ty)
                case FuncCall(f, args) =>
                  PCall(terms(f), args.map(terms.apply))
              }
          }

        }

    }

    ???
  }

}
