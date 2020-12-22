package lambdanet.correctness

import lambdanet.LibDefs
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{BinaryRel, BinaryRelCat, PNode, PType}

case class TypeChecker(
  graph: PredicateGraph,
  libDefs: LibDefs,
  binaryRels: Set[BinaryRel],
  subtypesToCheck: Set[(PNode, PNode)],
  defaultContext: PTypeContext,
  additionalSubrel: Set[(PType, PType)]
) {
  def violate(
      assignment: Map[PNode, PType],
  ): Set[(PNode, PNode)] =
    violate(defaultContext, assignment)

  def violate(
      context: PTypeContext,
      assignment: Map[PNode, PType]
  ): Set[(PNode, PNode)] = {
    assert(
      assignment.keySet == graph.nodes.filter(_.fromProject),
      "Assignment does not fully match nodes on the graph. \n" +
        s"Nodes only in assignment: ${assignment.keySet &~ graph.nodes}\n" +
        s"Nodes only in graph: ${graph.nodes &~ assignment.keySet}"
    )
    subtypesToCheck.filterNot {
      case (child, parent) => context.isSubtype(child, parent, assignment)
    }
  }
}

object TypeChecker {
  def apply(
    graph: PredicateGraph,
    libDefs: LibDefs,
    additionalSubrel: Set[(PType, PType)] = Set.empty
  ): TypeChecker = {
    val binaryRels = graph.predicates.collect {
      // inheritance is always satisfied (from definition)
      case p: BinaryRel if p.category != BinaryRelCat.inheritance => p
    }
    val subtypesToCheck: Set[(PNode, PNode)] =
      binaryRels
        .flatMap {
          case BinaryRel(lhs, rhs, category) =>
            category match {
              // use inheritance as hard constraints
              case BinaryRelCat.subtype | BinaryRelCat.assign =>
                Set((lhs, rhs))
              case BinaryRelCat.equal | BinaryRelCat.fixType |
                   BinaryRelCat.fixAnnotation =>
                Set((lhs, rhs), (rhs, lhs))
            }
        }
    val defaultContext = PTypeContext(graph, libDefs, additionalSubrel)
    TypeChecker(
      graph,
      libDefs,
      binaryRels,
      subtypesToCheck,
      defaultContext,
      additionalSubrel
    )
  }
}
