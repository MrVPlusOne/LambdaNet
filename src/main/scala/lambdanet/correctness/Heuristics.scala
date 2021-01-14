package lambdanet.correctness

import lambdanet.translation.PredicateGraph.{PAccess, PExpr, PNode, PObject}

object Heuristics {
  def accessNodesAreTheSame(
    typeUnfold: Map[PNode, PExpr]
  ): Set[Set[PNode]] = {
    typeUnfold
      .collect {
        case (node, PAccess(obj, label)) if typeUnfold.get(obj).exists(_.isInstanceOf[PObject]) =>
          val field = typeUnfold(obj).asInstanceOf[PObject].fields(label)
          (field, node)
      }
      .groupBy(_._1)
      .map { case (field, fieldToNodes) => Set(field) ++ fieldToNodes.values }
      .toSet
  }
}
