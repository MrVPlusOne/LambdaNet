package lambdanet.translation

import lambdanet.{IdAllocator, IdEquality}
import lambdanet.translation.IR._
import PredicateGraph._
import lambdanet.types.GType

case class PredicateGraph(
    nodes: Vector[PVar],
    predicates: Vector[TyPredicate]
)

object PredicateGraph {

  /** A PNode is either a known constant ([[PConst]])
    *  or an unknown variable([[PVar]]) */
  sealed trait PNode extends PExpr {
    def isType: Boolean
    def isTerm: Boolean = !isType
  }

  class PConst private (
      protected val id: Int,
      val name: TypeName,
      val isType: Boolean,
      val signatureOpt: Option[GType]
  ) extends PNode
      with IdEquality

  class PVar private (
      protected val id: Int,
      val nameOpt: Option[VarName],
      val isType: Boolean
  ) extends PNode
      with IdEquality {
    def showDetails: String = {
      val parts = nameOpt.map(n => s"{${n.name}}").toList
      s"𝒯$id${parts.mkString}"
    }

    override def toString: String = {
      val namePart = nameOpt.map(n => s"{${n.name}}").getOrElse("")
      s"𝒯$id$namePart"
    }
  }

  object PVar {

    class PVarAllocator extends IdAllocator[PVar] {
      def newVar(nameOpt: Option[VarName], isType: Boolean): PVar = {
        useNewId(id => new PVar(id, nameOpt, isType))
      }
    }

  }

  sealed trait TyPredicate

  case class HasLibType(v: PVar, ty: PConst) extends TyPredicate

  case class SubtypeRel(sub: PNode, sup: PNode) extends TyPredicate

  case class AssignRel(lhs: PNode, rhs: PNode) extends TyPredicate

  case class UsedAsBool(n: PVar) extends TyPredicate

  case class InheritanceRel(child: PVar, parent: PNode) extends TyPredicate

  case class DefineRel(v: PVar, expr: PExpr) extends TyPredicate

  // @formatter:off
  /**
    * e :=                  [[PExpr]]
    *   | n                 [[PNode]]
    *   | (n, ..., n) => n  [[PFunc]]
    *   | n(n, ..., n)
    *   | {l: n, ..., l: n}
    *   | n.l
    */
  // @formatter:on
  sealed trait PExpr

  case class PFunc(args: Vector[PNode], returnType: PNode) extends PExpr

  case class PCall(f: PNode, args: Vector[PNode]) extends PExpr

  case class PObject(fields: Map[Symbol, PNode]) extends PExpr

  case class PAccess(obj: PNode, label: Symbol) extends PExpr
}
