package lambdanet.correctness

import funcdiff.Real

object CrossEntropyMethod {
  case class CEResult[P](param: P, converged: Boolean, iterations: Int)

  /**
   *  @param x0 the initial parameter
    * @tparam S Sample
    * @tparam P Param
    */
  def ceMaximize[S, P](
      f: S => Real,
      x0: P,
      genSamples: (P, Int) => Vector[S],
      updateParam: (P, Vector[S], Vector[Real]) => P,
      numSamples: Int,
      numElites: Int,
      callback: (P, Vector[S], Vector[Real], Int) => Boolean,
      maxIters: Int = 1000,
  ): CEResult[P] = {
    var converged = false
    var t = 0
    var x = x0
    while (!converged && t < maxIters) {
      var samples = genSamples(x, numSamples)
      var scores = samples.map(f)
      if (numElites != numSamples) {
        val eliteIds = scores.indices.sortBy(scores(_)).take(numElites)
        scores = eliteIds.map(scores(_)).toVector
        samples = eliteIds.map(samples(_)).toVector
      }
      x = updateParam(x, samples, scores)
      converged = callback(x, samples, scores, t)
      t += 1
    }
    CEResult(x, converged, iterations = t)
  }
}
