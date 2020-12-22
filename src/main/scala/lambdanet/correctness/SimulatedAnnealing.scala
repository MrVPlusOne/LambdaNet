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
    constraintAccuracy: Seq[Double]
  )

  def diff(
      a: Assignment,
      b: Assignment
  ): Map[PNode, (PType, PType)] = {
    val set =
      for (node <- a.keySet if a(node) != b(node))
        yield (node, (a(node), b(node)))
    set.toMap
  }

  def lazySearch(
      g: PredicateGraph,
      proposal: Map[PNode, TopNDistribution[PType]],
      randomNeighbor: Assignment => Assignment,
      correct: Correction,
      t: Int => Double,
      numEpochs: Int,
      f: Objective,
      reboot: Boolean = false
  ): Iterator[((Assignment, Double), (Assignment, Double))] = {
    ???
    //    val mostLikely = proposal.mapValuesNow(_.topValue)
    //    Iterator.range(1, numEpochs).
  }

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
    // TODO: Use this to limit reboot times,
    accuracy: Accuracy,
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
    ys(0) = y
    bestYs(0) = bestY
    nodeAccuracy(0) = accuracy.get(x)

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
      if (reboot && epoch - lastBestEpoch > numEpochs / 10) {
        x = bestX
        y = bestY
        println(s"Reboot: from $epoch to $lastBestEpoch")
        epoch = lastBestEpoch
      }
      ys(epoch) = y
      bestYs(epoch) = bestY
      nodeAccuracy(epoch) = accuracy.get(x)
      epoch += 1
    }
    (
      correct(bestX),
      IntermediateValues(
        0 to numEpochs,
        ys,
        bestYs,
        nodeAccuracy = nodeAccuracy,
        constraintAccuracy = Array.empty[Double]
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

case class OneDifferenceRandomNeighbor(
    proposal: Map[PNode, TopNDistribution[PType]]
) extends OneDifferenceRandomNeighborBase

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

case class PatchAnyCorrection(
    checker: TypeChecker,
    proposal: Map[PNode, TopNDistribution[PType]]
) extends PatchAny {
  def correct(prediction: Assignment): Assignment = {
    val badPairs = checker.violate(prediction)
    patchAny(badPairs, prediction)
  }
}

case class LocalSearchCorrection(
    checker: TypeChecker,
    proposal: Map[PNode, TopNDistribution[PType]]
) extends PatchAny {
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
    assignment.flatMap {
      case (node, typ) =>
        val topN = proposal(node)
        val topNProb = topN.distrMap.get(typ)
        topNProb.map(math.log)
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

case class NegativeLogLikelihood(
  proposal: Map[PNode, TopNDistribution[PType]]
) extends NegativeLogLikelihoodBase

case class AverageNegativeLogLikelihood(
  proposal: Map[PNode, TopNDistribution[PType]]
) extends AverageNLLBase
