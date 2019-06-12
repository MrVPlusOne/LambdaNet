package funcdiff

import botkop.numsca.{NumscaRange, Tensor}
import botkop.{numsca => ns}
import com.typesafe.scalalogging.LazyLogging
import org.nd4j.linalg.api.buffer.DataBuffer
import org.nd4j.linalg.factory.Nd4j
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

trait TestUtils extends FlatSpec with Matchers with LazyLogging {

  ns.rand.setSeed(1)
  Nd4j.setDataType(DataBuffer.Type.DOUBLE)

  def relError(x: Tensor, y: Tensor): Double =
    ns.max(ns.abs(x - y) / ns.maximum(ns.abs(x) + ns.abs(y), 1e-8)).squeeze()

  def relError(dx: Gradient, dy: Gradient): Double = {
    val x = dx.toTensor()
    val y = dy.toTensor()
    ns.max(ns.abs(x - y) / ns.maximum(ns.abs(x) + ns.abs(y), 1e-8)).squeeze()
  }

  /**
    * Evaluate a numeric gradient for a function that accepts an array and returns an array.
    */
  def evalNumericalGradientArray(
      f: Tensor => Tensor,
      x: Tensor,
      df: Tensor,
      h: Double = 1e-5
  ): Tensor = {
    val grad = ns.zeros(x.shape)

    val it = ns.nditer(x)
    while (it.hasNext) {
      val ix = it.next

      val oldVal = x(ix).squeeze()

      x(ix) := oldVal + h
      val pos = f(x).copy()
      x(ix) := oldVal - h
      val neg = f(x).copy()
      x(ix) := oldVal

      val g = ns.sum((pos - neg) * df) / (2.0 * h)

      grad(ix) := g
    }
    grad
  }

  /**
  a naive implementation of numerical gradient of f at x
    - f should be a function that takes a single argument
    - x is the point (array) to evaluate the gradient at
    */
  def evalNumericalGradient(
      f: (Tensor) => Double,
      x: Tensor,
      h: Double = 0.00001
  ): Tensor = {
    val grad = ns.zeros(x.shape)
    val it = ns.nditer(x)
    while (it.hasNext) {
      val ix = it.next

      val oldVal = x(ix).squeeze()

      x(ix) := oldVal + h
      val pos = f(x)

      x(ix) := oldVal - h
      val neg = f(x)

      x(ix) := oldVal

      val g = (pos - neg) / (2.0 * h)
      grad(ix) := g
    }
    grad
  }

  val rand = new Random()

  def nOpGradientCheck(
      f: Seq[CompNode] => CompNode,
      x: IS[CompNode],
      name: String
  ): Unit = {

    val out = f(x)
    println(s"[$name]: out = $out")

    val zeroGrad = ZeroGradient(out.shape)
    val denseGrad = DenseGradient(ns.abs(ns.randn(out.shape)))
    val inflatedGrad = {
      val ranges = out.shape.ints.map { size =>
        val l = rand.nextInt(size)
        val r = rand.nextInt(size)
        NumscaRange(l, Some(math.max(r, size)))
      }
      InflatedGradient(denseGrad.value(ranges: _*), ranges.toList, out.shape)
    }

    for (dOut <- Seq(zeroGrad, denseGrad, inflatedGrad)) {
      val testName = s"$name(${dOut.getClass.getSimpleName})"

      val s = dOut.shape
      val t = ns.rand(s)
      assert(
        relError((dOut * t).toTensor(), dOut.toTensor() * t) < 1e-5,
        s"gradient $dOut failed to passed consistency test"
      )

      println(s"[$testName]: dOut = $dOut")

      val gradients = CompNode.backprop(List(out), List(dOut))

      x.zipWithIndex.foreach {
        case (n, i) =>
          def fi(t: Tensor): Tensor = {
            val newArgs = x.updated(i, const(t))
            f(newArgs).value
          }

          val numerical =
            evalNumericalGradientArray(fi, x(i).value, dOut.toTensor())
          println(s"[$testName]: numerical[$i] = $numerical")

          gradients.get(n) match {
            case Some(calculated) =>
              val calcTensor = calculated.toTensor()
              println(s"[$testName]: calculated[$i] = ${calculated}")
              val diError = relError(calcTensor, numerical)
              println(s"[$testName]: Difference: ${calcTensor - numerical}")
              assert(
                diError < 1e-5,
                s"\n[$testName]: $out failed to pass numerical gradient check"
              )
            case None =>
              assert(
                numerical.data.forall(x => math.abs(x) < 1e-5),
                s"*** $out failed to pass numerical gradient check. The gradient is expected to be zero."
              )
          }
      }
    }
  }

  def layerGradCheck(
      f: ParamCollection => CompNode,
      pc: ParamCollection,
      name: String
  ): Unit = {

    val out = f(pc)
    println(s"Test Layer: $name")

    val dOut = DenseGradient(ns.ones(out.shape))
    val gradients = out.backpropForParams(None)

    pc.allParams.foreach { p =>
      def fi(t: Tensor): Tensor = {
        p.node = paramNode(t, p.path)
        f(pc).value
      }

      val numerical =
        evalNumericalGradientArray(fi, p.node.value, dOut.toTensor())
      println(s"numerical[$p] = $numerical")
      gradients.get(p.path) match {
        case Some(calculated) =>
          val calcTensor = calculated.toTensor()
          println(s"calculated[$p] = ${calculated}")
          val diError = relError(calcTensor, numerical)
          println(s"Difference: ${calcTensor - numerical}")
          assert(
            diError < 1e-5,
            s"*** $out failed to pass numerical gradient check"
          )
        case None =>
          assert(
            numerical.data.forall(x => math.abs(x) < 1e-5),
            s"*** $out failed to pass numerical gradient check. The gradient is expected to be zero."
          )
      }
    }
  }

}
