package lambdanet.correctness

import java.util.concurrent.ThreadLocalRandom

import cats.Monoid
import cats.implicits._
import com.typesafe.scalalogging.Logger
import funcdiff.Real
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PAny, PNode, PType}

import scala.util.Random.javaRandomToRandom

object CrossEntropyTypeInference {
  type Samples = Vector[Assignment]
  type Scores = Vector[Real]

  /**
    *
    * @param subtypingNodes maps a node to its set of either child nodes (left) or parent nodes (right)
    */
  case class AssignmentGen(
    projectNodes: Set[PNode],
    context: PTypeContext,
    subtypingNodes: Map[PNode, Set[Either[PNode, PNode]]],
    sameNodes: Set[Set[PNode]]
  ) extends ((TypeDistrs, Int) => Samples) {
    private val logger = Logger(classOf[AssignmentGen])

    def apply(param: TypeDistrs, numSamples: Int): Samples =
      Vector.fill(numSamples)(gen(param))

    def gen(param: TypeDistrs): Assignment = {
      val random = ThreadLocalRandom.current()
      var assignment = Map.empty[PNode, PType].withDefaultValue(PAny)
      val perm = random.shuffle(sameNodes.map(random.shuffle(_)))
      for (nodes <- perm) {
        val node = nodes.head
        logger.debug(s"Selecting node: $node")
        val allNodeTypes = param(node).distr.map(_._2).toSet
        val relatedNodes = nodes.flatMap(node => subtypingNodes.getOrElse(node, Set.empty))
        val availableTypes = relatedNodes
          .foldLeft(allNodeTypes) {
            case (types, eitherChildOrParent) =>
              val remaining = types.filter {
                eitherChildOrParent match {
                  case Left(child) =>
                    typ => context.isSubtype(child, node, assignment.updated(node, typ))
                  case Right(parent) =>
                    typ => context.isSubtype(node, parent, assignment.updated(node, typ))
                }
              }
              logger.debug(s"Types remaining after related node $eitherChildOrParent: ${remaining.mkString(", ")}")
              remaining
          }
        assert(availableTypes.nonEmpty, s"no available type for node $node")
        val probs = availableTypes.toSeq.map(param(node).typeProb(_))
        logger.debug(s"Types and probs: ${availableTypes.toSeq.zip(probs).mkString(", ")}")
        val nodeType = Sampling.choose(
          availableTypes.toSeq,
          availableTypes.toSeq.map(param(node).typeProb(_))
        )
        logger.debug(s"Assigning $nodeType to $node\n")
        assignment = assignment ++ nodes.map(node => node -> nodeType).toMap
      }
      assignment
    }
  }

  case class UpdateTypeDistrs(smoothing: Double) extends ((TypeDistrs, Samples, Scores) => TypeDistrs) {
    val monoid: Monoid[Map[PNode, Map[PType, Real]]] = implicitly

    override def apply(distrs: TypeDistrs,
                       elites: Samples,
                       _scores: Scores): TypeDistrs = {
      val n = elites.size
      val probs = elites.map(_.mapValues(x => Map(x -> 1.0 / n))).foldLeft(monoid.empty)(monoid.combine)
      val totalProbs = multiply(distrs.mapValues(_.typeProb), smoothing) |+| multiply(probs, 1 - smoothing)
      totalProbs.mapValuesNow(typeProbs => TopNDistribution(typeProbs.toVector.map(x => (x._2, x._1)).sortBy(-_._1), typeProbs))
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