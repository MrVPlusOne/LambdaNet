package lambdanet.correctness

import cats.Foldable
import cats.implicits._
import com.typesafe.scalalogging.Logger
import funcdiff.Real
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PAny, PNode, PType}

import scala.util.Random

object CrossEntropyTypeInference {
  type Samples = Vector[Assignment]
  type Scores = Vector[Real]

  case class AssignmentGen(
      projectNodes: Set[PNode],
      context: PTypeContext,
      parents: Map[PNode, Set[PNode]],
      children: Map[PNode, Set[PNode]]
  ) extends ((TypeDistrs, Int) => Samples) {
    private val logger = Logger(classOf[AssignmentGen])

    def apply(param: TypeDistrs, numSamples: Int): Samples =
      Vector.fill(numSamples)(gen(param))

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

  case class UpdateTypeDistrs(smoothing: Double) extends ((TypeDistrs, Samples, Scores) => TypeDistrs) {
    override def apply(distrs: TypeDistrs,
                       elites: Samples,
                       _scores: Scores): TypeDistrs = {
      val n = elites.size
      val probs = Foldable[Vector].fold(elites.map(_.mapValues(x => Map(x -> 1.0 / n))))
      val totalProbs = multiply(distrs.mapValues(_.typeProb), smoothing) |+| multiply(probs, 1 - smoothing)
      totalProbs.mapValues(typeProbs => TopNDistribution(typeProbs.toVector.map(x => (x._2, x._1)).sortBy(_._1)))
    }

    def multiply(distrs: Map[PNode, Map[PType, Real]], k: Real): Map[PNode, Map[PType, Real]] =
      distrs.mapValues(_.mapValues(_ * k))
  }

  case class IsConverged(stopIters: Int) extends ((TypeDistrs, Samples, Scores, Int) => Boolean) {
    var best: Option[(Int, Real)] = None
    override def apply(_distrs: TypeDistrs,
                       _elites: Samples,
                       scores: Scores,
                       t: Int): Boolean = {
      val mean = scores.sum / scores.size
      if (best.isEmpty || best.exists { case (_, bestScore) => bestScore > mean }) {
        best = Some((t, mean))
      }
      best.exists { case (bestT, _) => t - bestT >= stopIters }
    }
  }
}