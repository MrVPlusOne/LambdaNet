package lambdanet.correctness

import java.util.concurrent.ThreadLocalRandom

import cats.Monoid
import cats.implicits._
import com.typesafe.scalalogging.Logger
import funcdiff.Real
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PAny, PNode, PType}
import lambdanet.utils.Statistics

import scala.collection.mutable.ArrayBuffer
import scala.util.Random.javaRandomToRandom

object CrossEntropyTypeInference {
  type Samples = Vector[Assignment]
  type Scores = Vector[Real]

  case class AssignmentGen(
    projectNodes: Set[PNode],
    checker: ValidTypeGen,
    sameNodes: Set[Set[PNode]],
    precomputedValidTypes: Map[PNode, Seq[PType]],
    fixedAssignment: Assignment
  ) extends ((TypeDistrs, Int) => Samples) {
    private val logger = Logger(classOf[AssignmentGen])

    def apply(param: TypeDistrs, numSamples: Int): Samples =
      Vector.fill(numSamples)(gen(param))

    def gen(param: TypeDistrs): Assignment = {
      // fixme: can we pass in random seeds so that the results are reproducible?
      val random = ThreadLocalRandom.current()
      var assignment = fixedAssignment.withDefaultValue(PAny)
      val perm = random.shuffle(sameNodes.map(random.shuffle(_)))
      for (nodes <- perm) {
        // todo: When library nodes are added in PTypeContext, nodes can contain library nodes
        val node = nodes.head
        if (!assignment.contains(node)) {
          logger.debug(s"Selecting node: $node")
          val allNodeTypes = precomputedValidTypes(node)
          val validTypes = checker.validTypes(allNodeTypes, nodes, assignment)
          assert(validTypes.nonEmpty, s"no available type for node $node")
          val typesAndProbs = validTypes.map { typ =>
            val prob = param(node).typeProb(typ)
            val adjustedProb =
              if (typ == PAny) {
                prob / param(node).distr.size
              } else {
                prob
              }
            (typ, adjustedProb)
          }
          logger.debug(s"Types and probs: ${typesAndProbs.mkString(", ")}")
          val nodeType = Sampling.choose(typesAndProbs)
          logger.debug(s"Assigning $nodeType to $node\n")
          assignment = assignment ++ nodes.map(node => node -> nodeType)
        }
      }
      assignment
    }
  }

  case class UpdateTypeDistrs(smoothing: Double)
    extends ((TypeDistrs, Samples, Scores) => TypeDistrs) {
    val monoid: Monoid[Map[PNode, Map[PType, Real]]] = implicitly

    override def apply(
      distrs: TypeDistrs,
      elites: Samples,
      _scores: Scores
    ): TypeDistrs = {
      val n = elites.size
      val probs = elites.map(_.mapValues(x => Map(x -> 1.0 / n))).foldLeft(monoid.empty)(monoid.combine)
      val totalProbs = multiply(distrs.mapValues(_.typeProb), smoothing) |+| multiply(probs, 1 - smoothing)
      totalProbs.mapValuesNow(typeProbs => TopNDistribution(typeProbs.toVector.map(x => (x._2, x._1)).sortBy(-_._1), typeProbs))
    }

    def multiply(distrs: Map[PNode, Map[PType, Real]], k: Real): Map[PNode, Map[PType, Real]] =
      distrs.mapValues(_.mapValues(_ * k))
  }

  type Metric = (TypeDistrs, Samples, Scores, Int) => Unit
  type Convergence = (TypeDistrs, Samples, Scores, Int) => Boolean

  trait AverageAndStdev {
    def length: Int

    val epochs: ArrayBuffer[Int] = new ArrayBuffer[Int]()
    val mean: ArrayBuffer[Double] = new ArrayBuffer[Double]()
    val stdev: ArrayBuffer[Double] = new ArrayBuffer[Double]()

    def recordAverageAndStdev(xs: Seq[Double], t: Int): Unit = {
      val currentMean = Statistics.average(xs)
      epochs += t
      mean += currentMean
      stdev += Statistics.stdev(xs, currentMean)
    }
  }

  class SampleAccuracy(maxIters: Int, accuracy: Accuracy) extends Metric with AverageAndStdev with Serializable {
    def length: Int = maxIters

    override def apply(
      _distrs: TypeDistrs,
      elites: Samples,
      _scores: Scores,
      t: Int
    ): Unit = {
      recordAverageAndStdev(elites.map(accuracy.get), t)
      println(s"Accuracy at epoch $t: ${mean(t - 1)} +/- ${stdev(t - 1)}")
    }
  }

  class SampleScore(maxIters: Int) extends Metric with AverageAndStdev with Serializable {
    def length: Int = maxIters

    override def apply(
      _distrs: TypeDistrs,
      _elites: Samples,
      scores: Scores,
      t: Int
    ): Unit = {
      recordAverageAndStdev(scores, t)
    }
  }

  case class IsConverged(stopIters: Int) extends Convergence {
    var best: Option[(Int, Real)] = None

    override def apply(
      _distrs: TypeDistrs,
      _elites: Samples,
      scores: Scores,
      t: Int
    ): Boolean = {
      val mean = scores.sum / scores.size
      if (best.isEmpty || best.exists { case (_, bestScore) => bestScore > mean }) {
        best = Some((t, mean))
      }
      best.exists { case (bestT, _) => t - bestT >= stopIters }
    }
  }
}
