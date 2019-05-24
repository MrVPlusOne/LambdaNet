package lambdanet.translation

import lambdanet.IdEquality
import lambdanet.translation.IR._
import PredicateGraph._
import lambdanet.types.GType

case class PredicateGraph(
    nodes: Vector[PVar],
    predicates: Vector[TyPredicate]
)

object PredicateGraph {

  /** A PNode is either a fixed library type([[LibraryType]])
    *  or an unknown type variable([[PVar]]) */
  sealed trait PNode extends PExpr

  class LibraryType(
      protected val id: Int,
      val name: Symbol,
      val signatureOpt: Option[GType]
  ) extends PNode
      with IdEquality

  class PVar(
      protected val id: Int,
      val nameOpt: Option[Symbol]
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

  sealed trait TyPredicate

  case class HasLibType(v: PVar, ty: LibraryType) extends TyPredicate

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
