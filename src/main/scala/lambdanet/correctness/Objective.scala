package lambdanet.correctness

object Objective {
  trait NegativeLogLikelihoodBase extends (Assignment => Double) {
    def proposal: TypeDistrs

    def logLikelihoods(assignment: Assignment): Iterable[Double] =
      assignment.map {
        case (node, typ) =>
          val topN = proposal(node)
          val topNProb = topN.typeProb(typ)
          math.log(topNProb)
      }

    def prob(assignment: Assignment): Double =
      -logLikelihoods(assignment).sum

    def apply(assignment: Assignment): Double =
      prob(assignment)
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
}