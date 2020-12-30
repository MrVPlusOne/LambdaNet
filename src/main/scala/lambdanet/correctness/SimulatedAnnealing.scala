package lambdanet.correctness

import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{PAny, PNode, PType}

import scala.util.Random

object SimulatedAnnealing {
  case class IntermediateValues(
      epochs: Seq[Int],
      ys: Seq[Double],
      bestYs: Seq[Double],
      nodeAccuracy: Seq[Double],
      proportionOfAny: Seq[Double],
      proportionOfNodesCoveredByAny: Seq[Double]
  )

  /**
    * Minimize f using simulated annealing. Record intermediate objective values.
    */
  def searchWithLog(
      g: PredicateGraph,
      proposal: Map[PNode, TopNDistribution[PType]],
      randomNeighbor: Assignment => Assignment,
      correct: Correction,
      t: Int => Double,
      numEpochs: Int,
      f: Objective,
      reboot: Boolean = false,
      accuracy: Accuracy,
      checker: TypeChecker,
      // TODO: Use this to limit reboot times,
      rebootLimit: Int = 5
  ): (Assignment, IntermediateValues) = {
    val mostLikely = proposal.mapValuesNow(_.topValue)
    var x = mostLikely
    val correctX = correct(x)
    var y = f(correctX)
    var bestX = x
    var bestY = y
    var lastBestEpoch = 0

    val ys = new Array[Double](numEpochs + 1)
    val bestYs = new Array[Double](numEpochs + 1)
    val nodeAccuracy = new Array[Double](numEpochs + 1)
    val proportionOfAny = new Array[Double](numEpochs + 1)
    val proportionOfNodesCoveredByAny = new Array[Double](numEpochs + 1)
    ys(0) = y
    bestYs(0) = bestY
    nodeAccuracy(0) = accuracy.get(x)
    val anyNodes = accuracy.truth.keySet.filter(correctX(_) == PAny)
    proportionOfAny(0) = anyNodes.size / accuracy.truth.keySet.size.toDouble
    // TODO: Is this way of calculating node coverage accurate?
    val constrainedNodes = checker.subtypesToCheck
      .flatMap { case (a, b) => Set(a, b) }
      .intersect(accuracy.truth.keySet)
    val nodesCoveredByAny = constrainedNodes.to[collection.mutable.Set]
    checker.subtypesToCheck.foreach {
      case (a, b) =>
        if (!anyNodes.contains(a) && !anyNodes.contains(b)) {
          nodesCoveredByAny -= a
          nodesCoveredByAny -= b
        }
    }
    proportionOfNodesCoveredByAny(0) = nodesCoveredByAny.size / accuracy.truth.keySet.size.toDouble

    var epoch = 1
    while (epoch <= numEpochs) {
      val nextX = randomNeighbor(x)
      val correctX = correct(nextX)
      val nextY = f(correctX)
      val deltaY = nextY - y
      if (deltaY <= 0 || Random.nextDouble() < math.exp(-deltaY / t(epoch))) {
        x = nextX
        y = nextY
      }
      if (y < bestY) {
        bestX = x
        bestY = y
        lastBestEpoch = epoch
      }
      //      println(s"epoch: $epoch, y: $y, bestY: $bestY")
      ys(epoch) = y
      bestYs(epoch) = bestY
      nodeAccuracy(epoch) = accuracy.get(correctX)

      val constrainedNodes = checker.subtypesToCheck
        .flatMap { case (a, b) => Set(a, b) }
        .intersect(accuracy.truth.keySet)
      val anyNodes = accuracy.truth.keySet.filter(correctX(_) == PAny)
      proportionOfAny(epoch) = anyNodes.size / accuracy.truth.keySet.size.toDouble
      val nodesCoveredByAny = constrainedNodes.to[collection.mutable.Set]
      checker.subtypesToCheck.foreach {
        case (a, b) =>
          if (!anyNodes.contains(a) && !anyNodes.contains(b)) {
            nodesCoveredByAny -= a
            nodesCoveredByAny -= b
          }
      }
      proportionOfNodesCoveredByAny(epoch) = nodesCoveredByAny.size / accuracy.truth.keySet.size.toDouble

      if (reboot && epoch - lastBestEpoch > numEpochs / 10) {
        x = bestX
        y = bestY
        println(s"Reboot: from $epoch to $lastBestEpoch")
        epoch = lastBestEpoch
      }
      epoch += 1
    }
    (
      correct(bestX),
      IntermediateValues(
        0 to numEpochs,
        ys,
        bestYs,
        nodeAccuracy = nodeAccuracy,
        proportionOfAny,
        proportionOfNodesCoveredByAny
      )
    )
  }

  /**
    * Minimize f using simulated annealing. Only return the corrected best assignment.
    */
  def search(
      g: PredicateGraph,
      proposal: Map[PNode, TopNDistribution[PType]],
      randomNeighbor: Assignment => Assignment,
      correct: Correction,
      t: Int => Double,
      numEpochs: Int,
      f: Objective,
      reboot: Boolean = false
  ): Assignment = {
    val mostLikely = proposal.mapValuesNow(_.topValue)
    var x = mostLikely
    var y = f(correct(x))
    var bestX = x
    var bestY = y
    var lastBestEpoch = 0
    (1 to numEpochs).foreach { epoch =>
      val nextX = randomNeighbor(x)
      val correctX = correct(nextX)
      val nextY = f(correctX)
      val deltaY = nextY - y
      if (deltaY <= 0 || Random.nextDouble() < math.exp(-deltaY / t(epoch))) {
        x = nextX
        y = nextY
      }
      if (y < bestY) {
        bestX = x
        bestY = y
        lastBestEpoch = epoch
      }
      println(s"epoch: $epoch, y: $y, bestY: $bestY")
      if (reboot && epoch - lastBestEpoch > numEpochs / 20) {
        x = bestX
        y = bestY
      }
    }
    correct(bestX)
  }
}

trait OneDifferenceRandomNeighborBase {
  def proposal: Map[PNode, TopNDistribution[PType]]

  // TODO: Pass a vector for better performance
  def changeType(x: Assignment, nodeIndex: Int): Assignment = {
    val vec: Seq[(PNode, PType)] = x.toVector
    val (node, oldType) = vec(nodeIndex)
    val distr = proposal(node).distr
    val newType = Iterator
      .continually {
        val typeIndex = Random.nextInt(distr.size)
        distr(typeIndex)._2
      }
      .dropWhile(_ == oldType)
      .next()
    vec.updated(nodeIndex, (node, newType)).toMap
  }

  def randomNeighbor(x: Assignment): Assignment = {
    val nodeIndex = Random.nextInt(x.size)
    changeType(x, nodeIndex)
  }
}

trait WeightedOneDifferenceRandomNeighborBase
    extends OneDifferenceRandomNeighborBase {
  override def changeType(x: Assignment, nodeIndex: Int): Assignment = {
    // fixme: Create a prefix-sum of type probabilities for O(log n) lookup
    val vec: Seq[(PNode, PType)] = x.toVector
    val (node, oldType) = vec(nodeIndex)
    val distr = proposal(node)
    val remainingProb = 1 - distr.typeProb(oldType)
    val cmf = Random.nextDouble() * remainingProb
    val (_, newType) = {
      var sumProb = 0.0
      distr.distr.map {
        case (prob, t) =>
          if (t != oldType) {
            sumProb += prob
          }
          (sumProb, t)
      }.dropWhile { case (sumProb, _) => sumProb < cmf }.head
    }
    vec.updated(nodeIndex, (node, newType)).toMap
  }
}

case class OneDifferenceRandomNeighbor(
    proposal: Map[PNode, TopNDistribution[PType]]
) extends OneDifferenceRandomNeighborBase

case class WeightedOneDifferenceRandomNeighbor(
    proposal: Map[PNode, TopNDistribution[PType]]
) extends WeightedOneDifferenceRandomNeighborBase

case class WrongFirstOneDifferenceRandomNeighbor(
    override val proposal: Map[PNode, TopNDistribution[PType]],
    checker: TypeChecker
) extends OneDifferenceRandomNeighborBase {
  override def randomNeighbor(x: Assignment): Assignment = {
    val badPairs = checker.violate(x).toIndexedSeq
    if (badPairs.nonEmpty && Random.nextDouble() < 0.5) {
      val pairIndex = Random.nextInt(badPairs.size)
      val node = Iterator
        .continually {
          val elementIndex = Random.nextInt(2)
          badPairs(pairIndex).productElement(elementIndex)
        }
        .dropWhile(n => n.asInstanceOf[PNode].fromLib)
        .next()
      val nodeIndex = x.toIndexedSeq.indexWhere { case (n, _) => n == node }
      changeType(x, nodeIndex)
    } else {
      super.randomNeighbor(x)
    }
  }
}

trait CorrectionBase {
  def correct(assignment: Assignment): Assignment
}

trait PatchAny {

  /**
    * Try to mark as few project nodes as `Any` to satisfy all the subtyping relations in `badPairs`.
    * Treat this as the Minimum Vertex Cover problem and use the approximation algorithm.
    */
  // TODO: make this random
  def patchAny(
      badPairs: Set[(PNode, PNode)],
      assignment: Assignment
  ): Assignment = {
    Iterator
      .iterate((badPairs, assignment)) {
        case (badPairs, assignment) =>
          val (child, parent) = badPairs.head
          val projNodes = Set(child, parent).filter(_.fromProject)
          val remain = badPairs.filterNot {
            case (a, b) => projNodes.contains(a) || projNodes.contains(b)
          }
          val markAsAny = projNodes.foldLeft(assignment) {
            case (assignment, projNode) => assignment.updated(projNode, PAny)
          }
          (remain, markAsAny)
      }
      .dropWhile { case (remain, _) => remain.nonEmpty }
      .next()
      ._2
  }
}

trait WeightedPatchAny {
  def proposal: Map[PNode, TopNDistribution[PType]]

  def nodeNLL(x: PNode): Double =
    -math.log(proposal(x).typeProb(PAny))

  def tighten(
      badPairs: Iterable[(PNode, PNode)]
  ): collection.mutable.Map[PNode, Double] = {
    val eps = 1e-6
    val gap = collection.mutable.Map.empty[PNode, Double]
    badPairs.foreach {
      case (u, v) =>
        val projNodes = Set(u, v).filter(_.fromProject)
        projNodes.foreach { u =>
          if (!gap.contains(u)) {
            gap += (u -> nodeNLL(u))
          }
        }
        val nodeGaps = projNodes.map(gap)
        if (nodeGaps.forall(_ > eps)) {
          val maxPrice = nodeGaps.min
          projNodes.zip(nodeGaps).foreach {
            case (u, uGap) =>
              gap += (u -> (uGap - maxPrice))
          }
        }
    }
    gap
  }

  /**
    * Find the set of nodes with the minimum negative log-likelihood,
    * given that it fixes bad constraints when labeled as [[PAny]].
    * Use the Pricing Method to approximately find Weighted Vertex Cover.
    * See: [[http://web.cs.iastate.edu/~cs511/handout10/Approx_VC.pdf]]
    */
  def patchAny(
      badPairs: Iterable[(PNode, PNode)],
      assignment: Assignment
  ): Assignment = {
    val gap = tighten(badPairs)
    val anyNodes = gap.filter { case (_, g) => g == 0 }.keySet
    assignment ++ anyNodes.map(x => (x, PAny)).toMap
  }
}

case object NoCorrection extends CorrectionBase {
  def correct(assignment: Assignment): Assignment = assignment
}

case class PatchAnyCorrection(
    checker: TypeChecker,
    proposal: Map[PNode, TopNDistribution[PType]]
) extends CorrectionBase
    with PatchAny {
  def correct(prediction: Assignment): Assignment = {
    val badPairs = checker.violate(prediction)
    patchAny(badPairs, prediction)
  }
}

case class WeightedPatchAnyCorrection(
    checker: TypeChecker,
    proposal: Map[PNode, TopNDistribution[PType]]
) extends CorrectionBase
    with WeightedPatchAny {
  def correct(prediction: Assignment): Assignment = {
    val badPairs = checker.violate(prediction)
    patchAny(badPairs, prediction)
  }
}

case class LocalSearchCorrection(
    checker: TypeChecker,
    proposal: Map[PNode, TopNDistribution[PType]]
) extends CorrectionBase
    with PatchAny {
  def correct(prediction: Assignment): Assignment = {
    val (badPairs, prediction1) = boundedSearch(prediction, 3)
    patchAny(badPairs, prediction1)
  }

  def boundedSearch(
      prediction: Assignment,
      remainingDepth: Int
  ): (Set[(PNode, PNode)], Assignment) = {
    val badPairs = checker.violate(prediction)
    if (remainingDepth <= 0)
      return (badPairs, prediction)
    val badNodes = badPairs.flatMap { case (a, b) => Set(a, b) }
    var (leastBadPairs, bestPrediction) = (badPairs, prediction)
    badNodes.foreach { node =>
      proposal(node).distr.map(_._2).foreach { newType =>
        if (newType != prediction(node)) {
          val (newBadPairs, newPrediction) =
            boundedSearch(prediction.updated(node, newType), remainingDepth - 1)
          if (newBadPairs.size < badPairs.size) {
            leastBadPairs = badPairs
            bestPrediction = newPrediction
          }
        }
      }
    }
    (leastBadPairs, bestPrediction)
  }
}

trait NegativeLogLikelihoodBase {
  def proposal: Map[PNode, TopNDistribution[PType]]

  def logLikelihoods(assignment: Assignment): Iterable[Double] =
    assignment.map {
      case (node, typ) =>
        val topN = proposal(node)
        val topNProb = topN.typeProb(typ)
        math.log(topNProb)
    }

  def prob(assignment: Assignment): Double =
    -logLikelihoods(assignment).sum
}

trait AverageNLLBase extends NegativeLogLikelihoodBase {
  override def prob(assignment: Assignment): Double = {
    val ll = logLikelihoods(assignment)
    -ll.sum / ll.size
  }
}

trait PenalizedAverageNLLBase extends AverageNLLBase {
  def checker: TypeChecker
  def coefficient: Double

  override def prob(assignment: Assignment): Double =
    super.prob(assignment) + coefficient * checker.violate(assignment).size
}

case class NegativeLogLikelihood(
    proposal: Map[PNode, TopNDistribution[PType]]
) extends NegativeLogLikelihoodBase

case class AverageNegativeLogLikelihood(
    proposal: Map[PNode, TopNDistribution[PType]]
) extends AverageNLLBase

case class PenalizedAverageNLL(
    proposal: Map[PNode, TopNDistribution[PType]],
    checker: TypeChecker,
    coefficient: Double
) extends PenalizedAverageNLLBase
