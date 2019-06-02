package lambdanet.translation

import lambdanet.{Annot, IdAllocator, IdEquality}
import PredicateGraph._

case class PredicateGraph(
    nodes: Vector[PNode],
    predicates: Vector[TyPredicate]
)

object PredicateGraph {

  class PNode(
      protected val id: Int,
      val nameOpt: Option[Symbol],
      val isType: Boolean,
      val fromLib: Boolean
  ) extends PExpr {
    def isTerm: Boolean = !isType

    def showDetails: String = {
      val parts = nameOpt.map(n => s"{${n.name}}").toList
      s"𝒯$id${parts.mkString}"
    }

    override def toString: String = {
      val namePart = nameOpt.map(n => s"{${n.name}}").getOrElse("")
      s"𝒯$id$namePart"
    }

    override def equals(obj: Any): Boolean = obj match {
      case p: PNode =>
        (id, isType) == (p.id, p.isType)
      case _ => false
    }

    override def hashCode(): Int = {
      (id, isType).hashCode()
    }
  }

  class PNodeAllocator(val forLib: Boolean) extends IdAllocator[PNode] {
    def newNode(
        nameOpt: Option[Symbol],
        isType: Boolean
    ): PNode = {
      useNewId(id => new PNode(id, nameOpt, isType, forLib))
    }
  }

  sealed trait PType

  case object PAny extends PType

  case class PTyVar(node: PNode) extends PType {
    assert(node.isType)
  }

  case class PFuncType(args: Vector[PType], to: PType) extends PType

  case class PObjectType(fields: Map[Symbol, PType]) extends PType

  sealed trait TyPredicate

  case class HasLibType(v: PNode, ty: PNode) extends TyPredicate

  case class SubtypeRel(sub: PNode, sup: PNode) extends TyPredicate

  case class AssignRel(lhs: PNode, rhs: PNode) extends TyPredicate

  case class UsedAsBool(n: PNode) extends TyPredicate

  case class InheritanceRel(child: PNode, parent: PNode) extends TyPredicate

  case class DefineRel(v: PNode, expr: PExpr) extends TyPredicate

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
