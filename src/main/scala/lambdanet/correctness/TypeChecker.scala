package lambdanet.correctness

import cats.Monoid
import cats.implicits._
import lambdanet.LibDefs
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph._

/**
  *
  * @param typeAndNodeBounds maps a node to its set of either child nodes (left) or parent nodes (right)
  */
case class TypeChecker(
    graph: PredicateGraph,
    libDefs: LibDefs,
    binaryRels: Set[BinaryRel],
    subtypesToCheck: Set[(PNode, PNode)],
    typeAndNodeBounds: Bounds[NodeOrType],
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
      nodes.flatMap(node => typeAndNodeBounds.getOrElse(node, Set.empty))
    allTypes
      .filter { typ =>
        val newAssignment = assignment.updated(node, typ)
        relatedNodes.forall {
          case Left(child) =>
            defaultContext
              .checkSubtype(toType(newAssignment, child), typ, newAssignment, Set.empty)
              .nonEmpty
          case Right(parent) =>
            defaultContext
              .checkSubtype(typ, toType(newAssignment, parent), newAssignment, Set.empty)
              .nonEmpty
        }
      }
  }
}

object TypeChecker {
  def syntheticCallBounds(syntheticCalls: Set[PSyntheticCall]): Bounds[NodeOrType] = {
    syntheticCalls
      .flatMap {
        case PSyntheticCall(ret, _, args, signature) =>
          signature match {
            // fixme: Right now I have constraints in the form of arg >: param and ret <: to.
            //  Should I generate constraints like param <: and to >: ret as well? I do this for binaryRels.
            case Left(PFunc(params, to)) =>
              args.zip(params).map { case (arg, param) => (arg, Left(Left(param))) } :+
                (ret, Right(Left(to)))
            case Right(PFuncType(paramTypes, toType)) =>
              args.zip(paramTypes).map { case (arg, param) => (arg, Left(Right(param))) } :+
                (ret, Right(Right(toType)))
          }
      }
      .groupBy(_._1)
      .mapValuesNow(_.map(_._2))
  }

  def apply(
      graph: PredicateGraph,
      libDefs: LibDefs,
      additionalSubrel: Set[(PType, PType)] = Set.empty
  ): TypeChecker = {
    val binaryRels = graph.predicates.collect {
      case p: BinaryRel => p
    }
    val defineRels = graph.predicates.collect {
      case p: DefineRel => p
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
              case BinaryRelCat.equal | BinaryRelCat.fixType | BinaryRelCat.fixAnnotation =>
                // todo: use fixType to constrain type assignment directly
                Set((lhs, rhs), (rhs, lhs))
            }
        }

    def monoid[T: Monoid] = implicitly[Monoid[T]]

    val parents: Bounds[NodeOrType] =
      subtypesToCheck.groupBy(_._1).mapValuesNow(_.map(x => Right(Left(x._2))))
    val children: Bounds[NodeOrType] =
      subtypesToCheck.groupBy(_._2).mapValuesNow(_.map(x => Left(Left(x._1))))
    val syntheticCalls = PMethodCall
      .generate(defineRels, libDefs)
      .asInstanceOf[Set[PSyntheticCall]] ++ PFuncCall
      .generate(defineRels, libDefs)
      .asInstanceOf[Set[PSyntheticCall]]
    val callBounds = syntheticCallBounds(syntheticCalls)
    val subtypingNodes =
      monoid[Bounds[NodeOrType]].combineAll(Seq(parents, children, callBounds))

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
