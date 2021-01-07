package lambdanet.correctness

import org.scalatest.WordSpec

class SamplingTest extends WordSpec {
  "choosing from discrete distribution" should {
    "pass an simple example" in {
      val items = Seq(1, 2, 3, 4)
      val probs = Seq(0.1, 0.4, 0.3, 0.1)
      assert(Sampling.choose(items, probs, 0.11 / 0.9) == 2)
      assert(Sampling.choose(items, probs, 0.1 / 0.9) == 1)
      assert(Sampling.choose(items, probs, 1) == 4)
    }
  }
}
