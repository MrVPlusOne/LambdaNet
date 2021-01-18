package lambdanet.correctness

import lambdanet.translation.PredicateGraph._

object Heuristics {
  def accessNodesAreTheSame(
      typeUnfold: Map[PNode, PExpr]
  ): Set[Set[PNode]] = {
    typeUnfold
      .collect {
        case (node, PAccess(obj, label))
            if typeUnfold.get(obj).exists(_.isInstanceOf[PObject]) =>
          val field = typeUnfold(obj).asInstanceOf[PObject].fields(label)
          (field, node)
      }
      .groupBy(_._1)
      .map { case (field, fieldToNodes) => Set(field) ++ fieldToNodes.values }
      .toSet
  }

  def availableTypesWithAnyAssignment(
      results: TypeDistrs,
      sameNodes: Set[Set[PNode]],
      checker: TypeChecker
  ): Map[PNode, Seq[PType]] = {
    val anyAssignment: Map[PNode, PType] = Map.empty.withDefaultValue(PAny)
    (
      for {
        nodeGroup <- sameNodes
        node <- nodeGroup
      } yield node -> checker.availableTypes(
        results(node).distr.map(_._2),
        nodeGroup,
        anyAssignment
      )
    )(collection.breakOut)
  }
}
