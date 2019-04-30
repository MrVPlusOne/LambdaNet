package infer

import IR._
import PredicateGraph._
import collection.mutable

object SymbolicDecoding {

  case class TypingEnv(
      typeAssign: Map[IRType, TypeLabel]
  )

  sealed trait TypingAssumption
  case class SubtypeAssumption(sub: IRType, sup: IRType) extends TypingAssumption

  def fromPredicates(predicates: Vector[TyVarPredicate]): Vector[TypingConstraint] = {
    val initAssign = mutable.HashMap[IRType, TypeLabel]()
    val constraintsLeft = mutable.Set[TypingConstraint]()
    //      val assumptionSet = mutable.Set[TypingAssumption]()
    /** variables whose value are defined in terms of other vars */
    val dependentVars = mutable.Set[IRType]()

    predicates.foreach {
      case FreezeType(v, ty) =>
        initAssign(v) = LibraryType(ty)
      case _: HasName       =>
      case _: IsLibraryType =>
      case s: SubtypeRel =>
        constraintsLeft += s
      case AssignRel(lhs, rhs) =>
        constraintsLeft += SubtypeRel(rhs, lhs)
      case _: UsedAsBoolean =>
      case InheritanceRel(child, parent) =>
        ???
      //          assumptionSet += SubtypeAssumption(child, parent)
      case d: DefineRel =>
        initAssign(d.v) = ProjectType(d.v)
        constraintsLeft += d
    }
    ???
  }
}
