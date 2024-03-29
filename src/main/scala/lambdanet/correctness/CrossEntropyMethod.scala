package lambdanet.correctness

import funcdiff.Real

object CrossEntropyMethod {
  case class CEResult[S, P](
      param: P,
      elites: Vector[S],
      scores: Vector[Real],
      converged: Boolean,
      iterations: Int
  )

  /**
    * @param x0 the initial parameter
    * @tparam S Sample
    * @tparam P Param
    */
  def ceMinimize[S, P](
      f: S => Real,
      x0: P,
      genSamples: (P, Int) => Vector[S],
      updateParam: (P, Vector[S], Vector[Real]) => P,
      numSamples: Int,
      numElites: Int,
      isConverged: (P, Vector[S], Vector[Real], Int) => Boolean,
      maxIters: Int = 1000,
      metrics: Seq[(P, Vector[S], Vector[Real], Int) => Unit]
  ): CEResult[S, P] = {
    var converged = false
    var t = 0
    var x = x0
    var samples = Vector.empty[S]
    var scores = Vector.empty[Real]
    while (!converged && t < maxIters) {
      t += 1
      println(t)
      samples = genSamples(x, numSamples)
      scores = samples.map(f)
      if (numElites != numSamples) {
        val eliteIds = scores.indices.sortBy(scores(_)).take(numElites)
        scores = eliteIds.map(scores(_)).toVector
        samples = eliteIds.map(samples(_)).toVector
      }
      x = updateParam(x, samples, scores)
      converged = isConverged(x, samples, scores, t)
      metrics.foreach(record => record(x, samples, scores, t))
    }
    CEResult(x, samples, scores, converged, iterations = t)
  }
}
