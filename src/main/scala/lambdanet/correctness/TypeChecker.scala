package lambdanet.correctness

import ammonite.ops.{RelPath, pwd, up}
import funcdiff.SimpleMath
import lambdanet.PrepareRepos.{libDefsFile, parseProject}
import lambdanet.translation.ImportsResolution.ErrorHandler
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{
  BinaryRel,
  BinaryRelCat,
  PNode,
  PType
}
import lambdanet.{LibDefs, announced}

case class TypeChecker(
    graph: PredicateGraph,
    additionalSubrel: Set[(PType, PType)] = Set.empty
) {
  def violate(
      assignment: Map[PNode, PType],
  ): Set[(PNode, PNode)] =
    violate(PTypeContext(graph, additionalSubrel), assignment)

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
    val binaryRels = graph.predicates.collect {
      // inheritance is always satisfied (from definition)
      case p: BinaryRel if p.category != BinaryRelCat.inheritance => p
    }
    val subtypesToCheck: Set[(PNode, PNode)] =
      binaryRels
        .filter {
          case BinaryRel(lhs, rhs, _) => lhs.fromProject && rhs.fromProject
        }
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
    subtypesToCheck.filterNot {
      case (child, parent) => context.isSubtype(child, parent, assignment)
    }
  }
}
