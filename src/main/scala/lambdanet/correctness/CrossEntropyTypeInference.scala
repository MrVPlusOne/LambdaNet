package lambdanet.correctness

import com.typesafe.scalalogging.Logger
import lambdanet.translation.PredicateGraph.{PAny, PNode, PType}

import scala.util.Random

object CrossEntropyTypeInference {
  type Samples = IndexedSeq[Assignment]
  case class AssignmentGen(
      projectNodes: Set[PNode],
      context: PTypeContext,
      parents: Map[PNode, Set[PNode]],
      children: Map[PNode, Set[PNode]]
  ) extends ((TypeDistrs, Int) => Samples) {
    private val logger = Logger(classOf[AssignmentGen])

    def apply(param: TypeDistrs, numSamples: Int): Samples =
      IndexedSeq.fill(numSamples)(gen(param))

    def gen(param: TypeDistrs): Assignment = {
      var assignment = Map.empty[PNode, PType].withDefaultValue(PAny)
      val perm = Random.shuffle(projectNodes)
      for (node <- perm) {
        logger.debug(s"Selecting node: $node")
        val allNodeTypes = param(node).distr.map(_._2).toSet
        val parentNodes = parents
          .getOrElse(node, Set.empty)
        val childrenNodes = children.getOrElse(node, Set.empty)
        logger.debug(s"Parent nodes: ${parentNodes.mkString(", ")}")
        val parentSubtypes = parentNodes
          .foldLeft(allNodeTypes) {
            case (subtypes, parent) =>
              val remaining = subtypes.filter(typ => context.isSubtype(node, parent, assignment.updated(node, typ)))
              logger.debug(s"Types remaining after parent $parent: ${remaining.mkString(", ")}")
              remaining
          }
        logger.debug(s"Children nodes: ${childrenNodes.mkString(", ")}")
        val childrenSupertypes = childrenNodes.foldLeft(parentSubtypes) {
          case (supertypes, child) =>
            val remaining = supertypes.filter(typ => context.isSubtype(child, node, assignment.updated(node, typ)))
            logger.debug(s"Types remaining after child $child: ${remaining.mkString(", ")}")
            remaining
        }
        val availableTypes = childrenSupertypes
        assert(availableTypes.nonEmpty, s"no available type for node $node")
        val probs = availableTypes.toSeq.map(param(node).typeProb(_))
        logger.debug(s"Types and probs: ${availableTypes.toSeq.zip(probs).mkString(", ")}")
        val nodeType = Sampling.choose(
          availableTypes.toSeq,
          availableTypes.toSeq.map(param(node).typeProb(_))
        )
        logger.debug(s"Assigning $nodeType to $node\n")
        assignment = assignment.updated(node, nodeType)
      }
      assignment
    }
  }
}
