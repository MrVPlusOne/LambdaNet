package lambdanet.correctness

import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PNode, PType}

object Objective {
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
}