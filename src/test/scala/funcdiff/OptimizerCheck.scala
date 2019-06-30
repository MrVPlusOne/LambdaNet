package funcdiff

import botkop.{numsca => ns}
import funcdiff.Optimizer._

class OptimizerCheck extends TestUtils {
  val optimizers =
    Seq[Optimizer](SGD(learningRate = 0.25), Adam(learningRate = 0.1))

  "All Optimizers" should "be able to solve this linear regression problem" in {

    for (optimizer <- optimizers) {
      val pc = ParamCollection()

      ns.rand.setSeed(1)

      val data = ns.randn(3, 3)
      val w1 = ns.randn(3, 3)
      val b1 = ns.randn(3, 1)
      val y1 = w1 * data + b1

      val errors = for (i <- 0 until 500) yield {
        val w = pc.getVar(SymbolPath.empty / 'w)(ns.randn(3, 3))
        val b = pc.getVar(SymbolPath.empty / 'b)(ns.randn(3, 1))

        val y = w * data + b
        val loss = mean(square(y - y1))
        optimizer.minimize(loss, pc.allParams)
        loss.value
      }

      val finalError = errors.last.squeeze()
      println(s"Optimizer $optimizer final error: $finalError")
      assert(
        finalError < 1e-3,
        s"Optimizer $optimizer failed. Final error: $finalError",
      )
    }
  }

  "All Optimizers" should "be able to solve this linear layer regression problem" in {
    for (optimizer <- optimizers) {
      val pc: ParamCollection = ParamCollection()
      ns.rand.setSeed(1)
      val factory = new LayerFactory(SymbolPath.empty / 'testNet, pc)

      val data = ns.randn(3, 3)
      val w1 = ns.randn(3, 3)
      val b1 = ns.randn(3, 1)
      val y1 = w1 * data + b1

      val errors = for (i <- 0 until 500) yield {
        if (i == 10) {
          factory.linear('newLinear, nOut = 23)(data)
        }

        val y = factory.linear('linear1, nOut = data.shape(1))(data)
        val loss = mean(square(y - y1))
        optimizer.minimize(loss, pc.allParams)
        loss.value
      }

      val finalError = errors.last.squeeze()
      println(s"Optimizer $optimizer final error: $finalError")
      assert(
        finalError < 1e-3,
        s"Optimizer $optimizer failed. Final error: $finalError",
      )
    }
  }

}
