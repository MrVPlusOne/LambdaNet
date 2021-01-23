package lambdanet.correctness

import lambdanet.translation.PredicateGraph.{PNode, PType}

trait ValidTypeGen {
  def validTypes(
    allTypes: Seq[PType],
    nodes: Set[PNode],
    assignment: Assignment
  ): Seq[PType]
}

