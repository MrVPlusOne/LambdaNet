package lambdanet.correctness

import lambdanet.translation.PredicateGraph.{PNode, PType}

trait ValidTypeGen {
  def validTypes(
      allTypes: Seq[PType],
      nodes: Set[PNode],
      assignment: Assignment
  ): Seq[PType]

  def toType(assignment: Assignment, nodeOrType: NodeOrType): PType = nodeOrType match {
    case Left(node) => assignment(node)
    case Right(typ) => typ
  }
}
