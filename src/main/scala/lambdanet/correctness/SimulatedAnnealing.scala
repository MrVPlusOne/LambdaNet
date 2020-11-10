package lambdanet.correctness

import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.{PAny, PNode, PType}

import scala.util.Random

object SimulatedAnnealing {
  def search(
      g: PredicateGraph,
      proposal: Map[PNode, TopNDistribution[PType]],
      randomNeighbor: Assignment => Assignment,
      correct: Correction,
      t: Int => Double,
      numEpochs: Int,
      f: Objective
  ): Assignment = {
    val mostLikely = proposal.mapValuesNow(_.topValue)
    var x = mostLikely
    var y = f(x)
    var bestX = x
    var bestY = y
    (1 to numEpochs).foreach { epoch =>
      val nextX = correct(randomNeighbor(x))
      val nextY = f(x)
      val deltaY = nextY - y
      if (deltaY >= 0 || Random.nextDouble() < math.exp(deltaY / t(epoch))) {
        x = nextX
        y = nextY
      }
      if (y < bestY) {
        bestX = x
        bestY = y
      }
    }
    bestX
  }
}

case class OneDifferenceRandomNeighbor(proposal: Map[PNode, TopNDistribution[PType]]) {
  // TODO: Pass a vector for better performance
  def randomNeighbor(x: Assignment): Assignment = {
    val nodeIndex = Random.nextInt(x.size)
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
}

case class PatchAnyCorrection(
    checker: TypeChecker,
    proposal: Map[PNode, TopNDistribution[PType]]
) {
  def correct(prediction: Assignment): Assignment = {
    val badPairs = checker.violate(prediction)
    patchAny(badPairs, prediction)
  }

  def patchAny(
    badPairs: Set[(PNode, PNode)],
    assignment: Assignment
  ): Assignment = {
    Iterator
      .iterate((badPairs, assignment)) {
        case (badPairs, assignment) =>
          val (child, parent) = badPairs.head
          val set = Set(child, parent)
          val remain = badPairs.filterNot {
            case (a, b) => set.contains(a) || set.contains(b)
          }
          val markAsAny = assignment.updated(child, PAny).updated(parent, PAny)
          (remain, markAsAny)
      }
      .dropWhile { case (remain, _) => remain.nonEmpty }
      .next()
      ._2
  }
}

case class LocalSearchCorrection(
    checker: TypeChecker,
    proposal: Map[PNode, TopNDistribution[PType]]
) {
  def correct(prediction: Assignment): Assignment = {
    val (badPairs, prediction1) = boundedSearch(prediction, 3)
    patchAny(badPairs, prediction1)
  }

  /**
    * Try to mark as few nodes as `Any` to satisfy all the subtyping relations in `badPairs`.
    * Treat this as the Minimum Vertex Cover problem and use the approximation algorithm.
    */
  def patchAny(
      badPairs: Set[(PNode, PNode)],
      assignment: Assignment
  ): Assignment = {
    Iterator
      .iterate((badPairs, assignment)) {
        case (badPairs, assignment) =>
          val (child, parent) = badPairs.head
          val set = Set(child, parent)
          val remain = badPairs.filterNot {
            case (a, b) => set.contains(a) || set.contains(b)
          }
          val markAsAny = assignment.updated(child, PAny).updated(parent, PAny)
          (remain, markAsAny)
      }
      .dropWhile { case (remain, _) => remain.nonEmpty }
      .next()
      ._2
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
      proposal(node).distr.map(_._2).foreach {
        case newType if newType != prediction(node) =>
          val (newBadPairs, newPrediction) =
            boundedSearch(prediction.updated(node, newType), remainingDepth - 1)
          if (newBadPairs.size < badPairs.size) {
            leastBadPairs = badPairs
            bestPrediction = newPrediction
          }
      }
    }
    (leastBadPairs, bestPrediction)
  }
}

case class LogLikelihood(proposal: Map[PNode, TopNDistribution[PType]]) {
  def prob(assignment: Assignment): Double =
    assignment.map {
      case (node, typ) =>
        math.log(proposal(node).distr.find(_._2 == typ).get._1)
    }.sum
}