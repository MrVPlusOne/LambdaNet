package lambdanet.correctness

import lambdanet.translation.PredicateGraph._

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

  def validTypesWithAnyAssignment(
      results: TypeDistrs,
      sameNodes: Set[Set[PNode]],
      checker: TypeChecker
  ): Map[PNode, Seq[PType]] = {
    val anyAssignment: Map[PNode, PType] = Map.empty.withDefaultValue(PAny)
    (
      for {
        nodeGroup <- sameNodes
        node <- nodeGroup
      } yield node -> checker.validTypes(
        results(node).distr.map(_._2),
        nodeGroup,
        anyAssignment
      )
    )(collection.breakOut)
  }

  def fixTypesByAccess(
      typeUnfold: Map[PNode, PExpr],
      nodeMapping: NodeMapping
  ): Assignment = {
    typeUnfold
      .collect {
        case (node, PAccess(obj, label))
            if nodeMapping
              .get(obj)
              .flatMap(_.typeOpt)
              .exists(_.isInstanceOf[PObjectType]) =>
          val fieldType =
            nodeMapping(obj).typeOpt.get.asInstanceOf[PObjectType].fields(label)
          (node, fieldType)
      }(collection.breakOut)
  }

  def fixTypesByFixType(
      fixTypes: Set[BinaryRel]
  ): Assignment = {
    fixTypes.collect {
      case BinaryRel(instance, typ, BinaryRelCat.fixType) =>
        (instance, PTyVar(typ))
    }(collection.breakOut)
  }

  def fixTypesByFixAnnotation(
      binaryRels: Set[BinaryRel],
      assignment: Assignment
  ): Assignment = {
    // todo(haoyang): Unify a node with its fixAnnotation's node
    ???
  }
}
