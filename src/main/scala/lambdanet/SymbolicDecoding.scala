package lambdanet

import lambdanet.translation.OldIR
import lambdanet.translation.OldIR.IRType
import lambdanet.translation.OldPredicateGraph._

import scala.collection.mutable

object SymbolicDecoding {

  case class InferEnv(
      typeAssign: Map[IRType, TypeLabel],
      dependentVars: Map[IRType, TypeExpr],
      constraints: Map[IRType, Set[TypingConstraint]]
  ) {
    @throws[TypeAssignInvalidException]
    def varChanged(dv: DecisionVar): InferEnv = {
      var currentEnv = this
      constraints(dv).foreach {
        case SubtypeRel(sub, sup) =>
          if (currentEnv.cannotBeSubtype(sub, sup))
            throw TypeAssignInvalidException(s"$sub should be a subtype of $sup")
        case DefineRel(v, rhs) =>
          rhs match {
            case VarTypeExpr(v1) =>
          }

      }
      ???
    }

    @throws[TypeAssignInvalidException]
    def assignDecisionVar(dv: DecisionVar, label: TypeLabel): InferEnv = {
      require(!dependentVars.contains(dv), s"$dv shouldn't be a dependent variable.")
      val newEnv = copy(typeAssign = typeAssign.updated(dv, label))
      label match {
        case OutOfScope => newEnv
        case _          => newEnv.varChanged(dv)
      }
    }

    def cannotBeSubtype(sub: OldIR.IRType, sup: OldIR.IRType): Boolean = {
      ???
    }
  }

  sealed trait TypingAssumption
  case class SubtypeAssumption(sub: IRType, sup: IRType) extends TypingAssumption

  def initEnv(predicates: Vector[TyVarPredicate]): InferEnv = {
    val initAssign = mutable.HashMap[IRType, TypeLabel]()
    val constraintsLeft = mutable.Set[TypingConstraint]()
    //      val assumptionSet = mutable.Set[TypingAssumption]()
    /** variables whose value are defined in terms of other vars */
    val dependentVars = mutable.HashMap[IRType, TypeExpr]()

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
        dependentVars(d.v) = d.expr
        constraintsLeft += d
    }

    ???
//    InferEnv(initAssign.toMap, dependentVars.toMap, constraintsLeft.toSet)
  }

  type DecisionVar = IRType

  case class TypeAssignInvalidException(msg: String) extends Exception

//  def decode(allVars: Vector[IRType], env: InferEnv) = {
//    var decisionVars = allVars.filterNot(env.dependentVars.contains)
//
//    def sortedOptions(dv: DecisionVar): Stream[TypeLabel] = ???
//
//    for (dv <- decisionVars) {
//      //might need to try assignments first to figure out which is best
//      val value = sortedOptions(dv).find(opt => valid(dv, opt)).get
//      assignVar(dv, value)
//    }
//  }
}
