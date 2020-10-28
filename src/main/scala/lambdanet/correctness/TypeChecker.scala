package lambdanet.correctness

import ammonite.ops.{RelPath, pwd, up}
import funcdiff.SimpleMath
import lambdanet.PrepareRepos.{libDefsFile, parseProject}
import lambdanet.translation.ImportsResolution.ErrorHandler
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{BinaryRel, BinaryRelCat, PNode, PType}
import lambdanet.{LibDefs, announced}

object TypeChecker {
  def violate(
      graph: PredicateGraph,
      assignment: Map[PNode, PType],
  ): Set[BinaryRel] = {
    assert(
      assignment.keySet == graph.nodes,
      "Assignment does not fully match nodes on the graph"
    )
    val context = PTypeContext(graph)
    val binaryRels = graph.predicates.collect {
      // inheritance is always satisfied (from definition)
      case p @ BinaryRel(_, _, category) if category != BinaryRelCat.inheritance => p
    }
    binaryRels.filterNot(satisfy(_, context, assignment))
  }

  def satisfy(
      predicate: BinaryRel,
      context: PTypeContext,
      assignment: Map[PNode, PType]
  ): Boolean = {
    val lhs = predicate.lhs
    val rhs = predicate.rhs
    predicate.category match {
      // use inheritance as hard constraints
      case BinaryRelCat.subtype | BinaryRelCat.assign =>
        context.isSubtype(lhs, rhs, assignment)
      case BinaryRelCat.equal | BinaryRelCat.fixType | BinaryRelCat.fixAnnotation =>
        context.isSubtype(lhs, rhs, assignment) &&
          context.isSubtype(rhs, lhs, assignment)
    }
  }
}
