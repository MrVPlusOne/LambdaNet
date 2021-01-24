package lambdanet.correctness

import lambdanet.translation.PredicateGraph.{PAny, PNode, PType}

case class ShallowSubtype(
    subtypes: Set[(PType, PType)],
    subtypingNodes: Map[PNode, Set[Either[PNode, PNode]]]
) extends ValidTypeGen {
  def validTypes(
      allTypes: Seq[PType],
      nodes: Set[PNode],
      assignment: Assignment
  ): Seq[PType] = {
    val relatedNodes =
      nodes.flatMap(node => subtypingNodes.getOrElse(node, Set.empty))
    allTypes
      .filter(
        typ =>
          relatedNodes.forall {
            case Left(child) =>
              subtypes.contains(assignment(child), typ)
            case Right(parent) =>
              subtypes.contains(typ, assignment(parent))
          }
      )
  }
}

object ShallowSubtype {

  /**
    * Over approximate subtyping relations when the type assignment of PNodes are unknown.
    */
  def apply(checker: TypeChecker, types: Set[PType]): ShallowSubtype = {
    val assignment: Map[PNode, PType] = Map.empty.withDefaultValue(PAny)
    val subtypes =
      for {
        child <- types
        parent <- types
        if checker.defaultContext
          .checkSubtype(child, parent, assignment, Set.empty)
          .nonEmpty
      } yield child -> parent
    new ShallowSubtype(subtypes, checker.subtypingNodes)
  }
}
