package lambdanet.correctness

import cats.Monoid
import cats.implicits._
import lambdanet.LibDefs
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{BinaryRel, BinaryRelCat, PNode, PType}

/**
  *
  * @param subtypingNodes maps a node to its set of either child nodes (left) or parent nodes (right)
  */
case class TypeChecker(
    graph: PredicateGraph,
    libDefs: LibDefs,
    binaryRels: Set[BinaryRel],
    subtypesToCheck: Set[(PNode, PNode)],
    subtypingNodes: Map[PNode, Set[Either[PNode, PNode]]],
    defaultContext: PTypeContext,
    additionalSubrel: Set[(PType, PType)]
) extends ValidTypeGen {
  def violate(
      assignment: Assignment
  ): Set[(PNode, PNode)] =
    violate(defaultContext, assignment)

  def violate(
      context: PTypeContext,
      assignment: Assignment
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

  def validTypes(
      allTypes: Seq[PType],
      nodes: Set[PNode],
      assignment: Assignment
  ): Seq[PType] = {
    val node = nodes.head
    val relatedNodes =
      nodes.flatMap(node => subtypingNodes.getOrElse(node, Set.empty))
    allTypes
      .filter(
        typ =>
          relatedNodes.forall {
            case Left(child) =>
              defaultContext.isSubtype(child, node, assignment.updated(node, typ))
            case Right(parent) =>
              defaultContext.isSubtype(node, parent, assignment.updated(node, typ))
          }
      )
  }
}

object TypeChecker {
  def apply(
      graph: PredicateGraph,
      libDefs: LibDefs,
      additionalSubrel: Set[(PType, PType)] = Set.empty
  ): TypeChecker = {
    val binaryRels = graph.predicates.collect {
      case p: BinaryRel => p
    }
    val subtypesToCheck: Set[(PNode, PNode)] =
      binaryRels
        .flatMap {
          case BinaryRel(lhs, rhs, category) =>
            category match {
              // use inheritance as hard constraints
              case BinaryRelCat.subtype | BinaryRelCat.inheritance =>
                Set((lhs, rhs))
              case BinaryRelCat.assign =>
                Set((rhs, lhs))
              case BinaryRelCat.equal | BinaryRelCat.fixType |
                  BinaryRelCat.fixAnnotation =>
                // todo: use fixType to constrain type assignment directly
                Set((lhs, rhs), (rhs, lhs))
            }
        }

    val parents: Map[PNode, Set[Either[PNode, PNode]]] =
      subtypesToCheck
        .groupBy(_._1)
        .mapValuesNow(_.map(x => Right(x._2)))
    val children: Map[PNode, Set[Either[PNode, PNode]]] =
      subtypesToCheck.groupBy(_._2).mapValuesNow(_.map(x => Left(x._1)))
    val subtypingNodes = implicitly[Monoid[Map[PNode, Set[Either[PNode, PNode]]]]].combine(parents, children)

    val defaultContext = PTypeContext(graph, libDefs, additionalSubrel)
    TypeChecker(
      graph,
      libDefs,
      binaryRels,
      subtypesToCheck,
      subtypingNodes,
      defaultContext,
      additionalSubrel
    )
  }
}
