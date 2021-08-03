package lambdanet.correctness

object Objective {
  trait LikelihoodBase extends (Assignment => Double) {
    def proposal: TypeDistrs
    def prob(assignment: Assignment): Double

    def likelihoods(assignment: Assignment): Iterable[Double] =
      assignment.map {
        case (node, typ) =>
          val topN = proposal(node)
          topN.typeProb(typ)
      }

    def apply(assignment: Assignment): Double =
      prob(assignment)
  }

  trait NegativeLogLikelihoodBase extends LikelihoodBase {
    def logLikelihoods(assignment: Assignment): Iterable[Double] =
      likelihoods(assignment).map(math.log)

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

  trait HammingLossBase extends LikelihoodBase {
    def prob(assignment: Assignment): Double =
      -likelihoods(assignment).sum
  }

  case class NegativeLogLikelihood(
    proposal: TypeDistrs,
  ) extends NegativeLogLikelihoodBase

  case class AverageNegativeLogLikelihood(
    proposal: TypeDistrs,
  ) extends AverageNLLBase

  case class PenalizedAverageNLL(
    proposal: TypeDistrs,
    checker: TypeChecker,
    coefficient: Double
  ) extends PenalizedAverageNLLBase

  case class HammingLoss(
      proposal: TypeDistrs
  ) extends HammingLossBase
}