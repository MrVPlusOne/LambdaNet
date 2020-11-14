package lambdanet.correctness

import ammonite.{ops => amm}
import lambdanet.translation.PredicateGraph.{BinaryRel, BinaryRelCat, DefineRel}

object SimulatedAnnealingDemo {
  def main(args: Array[String]): Unit = {
    val inputPath = amm.pwd / "data" / "tests" / "simple"
    val (graph, results) = InputUtils.loadGraphAndPredict(inputPath)

    println("Original result:")
    results foreach { case (node, value) => println(node, value) }
    val originalProb = LogLikelihood(results).prob(results.mapValuesNow(_.topValue))

    val defineRels = graph.predicates.collect {
      case p: DefineRel => p
    }
    println("defineRels:")
    defineRels.foreach { case DefineRel(v, expr) => println(v, expr) }
    val binaryRels = graph.predicates.collect {
      // inheritance is always satisfied (from definition)
      case p: BinaryRel if p.category != BinaryRelCat.inheritance => p
    }
    println("binaryRels")
    binaryRels.foreach {
      case BinaryRel(lhs, rhs, category) => println(lhs, rhs, category)
    }

    val checker = TypeChecker(graph)
    val schedule = (epoch: Int) => 20 * math.log(2) / math.log(epoch + 1)
    val correctPrediction =
      SimulatedAnnealing.search(
        graph,
        results,
        OneDifferenceRandomNeighbor(results).randomNeighbor,
        PatchAnyCorrection(checker, results).correct,
        schedule,
        numEpochs = 5000,
        f = LogLikelihood(results).prob
      )
    println("correctPrediction:")
    correctPrediction.foreach { case (node, pType) => println(node, pType) }
    val correctProb = LogLikelihood(results).prob(correctPrediction)
    println(s"log-likelihood of correct prediction: $correctProb")
    println(s"log-likelihood of original maximum-likelihood prediction: $originalProb")
  }
}
