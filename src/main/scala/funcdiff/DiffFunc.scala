package funcdiff

import botkop.numsca._
import TensorExtension._
import botkop.numsca
import botkop.{numsca => ns}

trait DiffFunc {
  def name: String

  def args: IS[CompNode]

  def value: Tensor

  def backProp(grad: Gradient): IS[Gradient]
}

object DiffFunc {

  case class ConstFunc(value: Tensor) extends DiffFunc {
    def name: String = {
      s"const{shape=${value.shape}}"
    }

    def args: IS[CompNode] = IS()

    def backProp(grad: Gradient): IS[Gradient] = IS()
  }

  // ============== Unary Functions ===============

  trait UnaryFunc extends DiffFunc {
    def x1: CompNode

    def args: IS[CompNode] = IS(x1)

    def backProp(grad: Gradient): IS[Gradient] = {
      IS(backprop1(grad))
    }

    def backprop1(grad: Gradient): Gradient
  }

  case class Sqrt(x1: CompNode) extends UnaryFunc {
    x1.value.requirePositive()

    val value: Tensor = numsca.sqrt(x1.value)

    def backprop1(grad: Gradient): Gradient = {
      grad / (value * 2)
    }

    def name: String = "sqrt"
  }

  case class Exp(x1: CompNode) extends UnaryFunc {
    val value: Tensor = numsca.exp(x1.value)

    def backprop1(grad: Gradient): Gradient = grad * value

    def name: String = "exp"
  }

  case class Negate(x1: CompNode) extends UnaryFunc {
    val value: Tensor = -x1.value

    def backprop1(grad: Gradient): Gradient = -grad

    def name: String = "negate"

  }

  case class Sigmoid(x1: CompNode) extends UnaryFunc {
    val value: Tensor = numsca.sigmoid(x1.value)

    def backprop1(grad: Gradient): Gradient = {
      grad * value * (1 - value)
    }

    def name: String = "sigmoid"
  }

  case class Tanh(x1: CompNode) extends UnaryFunc {
    val value: Tensor = numsca.tanh(x1.value)

    def backprop1(grad: Gradient): Gradient = {
      grad * (1 - numsca.square(value))
    }

    def name: String = "tanh"
  }

  case class Mean(x1: CompNode) extends UnaryFunc {
    require(x1.shape.product > 0)
    val value: Tensor = numsca.mean(x1.value)

    def backprop1(grad: Gradient): Gradient = {
      val n = x1.shape.product
      require(n > 0)
      (grad / n).broadcast(x1.shape)
    }

    def name: String = "mean"
  }

  case class MeanByAxis(x1: CompNode, axis: Int) extends UnaryFunc {
    require(x1.shape.product > 0)
    val value: Tensor = numsca.mean(x1.value, axis)

    def backprop1(grad: Gradient): Gradient = {
      val n = x1.shape(axis)
      require(n > 0)
      (grad / n).broadcast(x1.shape)
    }

    def name: String = s"mean{axis=$axis}"
  }

  case class Sum(x1: CompNode) extends UnaryFunc {
    val value = Tensor(ns.sum(x1.value))

    def backprop1(grad: Gradient): Gradient = {
      grad.broadcast(x1.shape)
    }

    def name: String = "sum"
  }

  case class SumByAxis(x1: CompNode, axis: Int) extends UnaryFunc {
    val value: Tensor = ns.sum(x1.value, axis)

    def backprop1(grad: Gradient): Gradient = {
      grad.broadcast(x1.shape)
    }

    def name: String = s"sum{axis=$axis}"
  }

  case class Threshold(x1: CompNode, threshold: Double) extends UnaryFunc {
    val value: Tensor = ns.maximum(x1.value, threshold)

    def backprop1(grad: Gradient): Gradient = {
      grad * (x1.value > threshold)
    }

    def name: String = s"threshold{t=$threshold}"
  }

  case class LeakyRelu(x1: CompNode, slope: Double) extends UnaryFunc {
    val value: Tensor = ns.maximum(x1.value, 0.0) + ns.minimum(x1.value, 0.0) * slope

    def backprop1(grad: Gradient): Gradient = {
      grad * ((x1.value > 0) + (x1.value < 0) * slope)
    }

    def name: String = s"leakyRelu{slope=$slope}"
  }

  case class Slice(x1: CompNode, ranges: Seq[NumscaRange]) extends UnaryFunc {
    require(
      x1.shape.sizes.zip(ranges).forall { case (s, r) => r.to.forall { _ <= s } },
      s"slice out of range. x1 shape: ${x1.shape}, ranges: ${showRanges(ranges)}"
    )
    val value = x1.value.apply(ranges: _*)

    def backprop1(grad: Gradient): Gradient = {
      grad.putInRanges(ranges, x1.shape)
    }

    def name: String = s"slice{ranges=${TensorExtension.showRanges(ranges)}}"
  }

  case class Transpose(x1: CompNode) extends UnaryFunc {
    require(x1.shape.rank == 2, "only matrices can be transposed")
    val value: Tensor = x1.value.transpose()

    def backprop1(grad: Gradient): Gradient = grad.transpose

    def name: String = "transpose"
  }

  case class PowerConst(x1: CompNode, power: Double) extends UnaryFunc {
    val value: Tensor = numsca.power(x1.value, power)

    private lazy val dCache = power * numsca.power(x1.value, power - 1)

    def backprop1(grad: Gradient): Gradient = {
      grad * dCache
    }

    def name: String = s"power{p=$power}"
  }

  /** softmax along the last dimension */
  case class Softmax(x1: CompNode) extends UnaryFunc {
    val value: Tensor = ns.softmax(x1.value)

    def backprop1(grad: Gradient): Gradient = {
      val y = value
      val dy = grad.toTensor()
      val dx = dy * y
      val s = ns.sum(dx, axis = dx.shape.rank - 1)
      dx -= y * s
      DenseGradient(dx)
    }

    def name: String = "softmax"
  }

  case class Log(x1: CompNode) extends UnaryFunc {
    x1.value.requirePositive()

    val value: Tensor = ns.log(x1.value)

    def backprop1(grad: Gradient): Gradient = {
      grad / x1.value
    }

    def name: String = "log"
  }

  case class Abs(x1: CompNode) extends UnaryFunc {
    x1.value.requireNonZero()

    val value: Tensor = ns.abs(x1.value)

    def backprop1(grad: Gradient): Gradient = {
      grad * (value / x1.value)
    }

    def name: String = "abs"
  }

  // ============== Binary Functions ==============

  trait BinaryFunc extends DiffFunc {
    def x1: CompNode

    def x2: CompNode

    def args: IS[CompNode] = IS(x1, x2)

    def backProp(grad: Gradient): IS[Gradient] = {
      val (g1, g2) = backProp2(grad)
      IS(g1, g2)
    }

    def backProp2(grad: Gradient): (Gradient, Gradient)
  }

  case class Plus(x1: CompNode, x2: CompNode) extends BinaryFunc {
    val value: Tensor = x1.value + x2.value

    def backProp2(grad: Gradient): (Gradient, Gradient) = {
      (grad.unbroadcast(x1.shape), grad.unbroadcast(x2.shape))
    }

    def name: String = "plus"
  }

  case class Minus(x1: CompNode, x2: CompNode) extends BinaryFunc {
    val value: Tensor = x1.value - x2.value

    def backProp2(grad: Gradient): (Gradient, Gradient) = {
      (grad.unbroadcast(x1.shape), -grad.unbroadcast(x2.shape))
    }

    def name: String = "minus"
  }

  case class Divide(x1: CompNode, x2: CompNode) extends BinaryFunc {
    x2.value.requireNonZero()
    val value: Tensor = x1.value / x2.value

    def backProp2(grad: Gradient): (Gradient, Gradient) = {
      val dx1 = (grad / x2.value).unbroadcast(x1.shape)
      val dx2 = (-grad * x1.value / (x2.value ** 2)).unbroadcast(x2.shape)
      (dx1, dx2)
    }

    def name: String = "divide"
  }

  case class Times(x1: CompNode, x2: CompNode) extends BinaryFunc {
    val value: Tensor = x1.value * x2.value

    def backProp2(grad: Gradient): (Gradient, Gradient) = {
      val dx1 = (grad * x2.value).unbroadcast(x1.shape)
      val dx2 = (grad * x1.value).unbroadcast(x2.shape)
      (dx1, dx2)
    }

    def name: String = "times"
  }

  case class Concat(x1: CompNode, x2: CompNode, axis: Int) extends BinaryFunc {

    val value: Tensor = numsca.concatenate(Seq(x1.value, x2.value), axis)

    def backProp2(grad: Gradient): (Gradient, Gradient) = {
      grad.splitAlongAxis(axis, x1.shape(axis).toInt)
    }

    def name: String = s"concat{axis=$axis}"

  }

  /** Matrix multiplication */
  case class Dot(x1: CompNode, x2: CompNode) extends BinaryFunc {
    val value: Tensor = x1.value dot x2.value

    def backProp2(grad: Gradient): (Gradient, Gradient) = {
      val gradTensor = grad.toTensor()
      DenseGradient(gradTensor.dot(x2.value.T)) ->
        DenseGradient(x1.value.T.dot(gradTensor))
    }

    def name: String = "dot"
  }

  case class MaxBinary(x1: CompNode, x2: CompNode) extends BinaryFunc {
    val value: Tensor = ns.maximum(x1.value, x2.value)

    def backProp2(grad: Gradient): (Gradient, Gradient) = {
      (grad * (x1.value >= x2.value)).unbroadcast(x1.shape) ->
        (grad * (x1.value <= x2.value)).unbroadcast(x2.shape)
    }

    def name: String = "maxBinary"
  }

  // ================ N-nary functions ==================
  case class Total(args: IS[CompNode]) extends DiffFunc {
    val value: Tensor = args.map(_.value).reduce(_ + _)

    def name: String = "total"

    def backProp(grad: Gradient): IS[Gradient] = {
      args.map { x =>
        grad.unbroadcast(x.shape)
      }
    }
  }

  case class ConcatN(args: IS[CompNode], axis: Int) extends DiffFunc {
    require(args.nonEmpty)

    val value: Tensor = ns.concatenate(args.map(_.value), axis)

    val name: String = s"concatN{axis=$axis}"

    def backProp(grad: Gradient): IS[Gradient] = {
      val denseGrad = grad match {
        case grad: InflatedGradient => grad.toDense
        case _                      => grad
      }

      val ranges = value.shape.ints.map { size =>
        0 until size
      }
      var idx = 0
      val subRegions = args.map { arg =>
        val s = arg.shape(axis).toInt
        idx += s
        ranges.updated(axis, (idx - s) until idx)
      }

      subRegions.map { rgs =>
        denseGrad.subGradient(rgs)
      }
    }
  }

  // ================ Loss functions ======================
  case class CrossEntropyOnSoftmax(logits: CompNode, targets: CompNode)
      extends UnaryFunc {
    require(
      targets.shape == logits.shape,
      s"Targets shape (${targets.shape}) is different from logits (${logits.shape})."
    )
    require(logits.shape.rank == 2, "Logits should be of rank 2.")

    def x1: CompNode = logits

    val (y, value) = {
      val baseline = ns.max(logits.value, axis = 1)
      val x = logits.value - baseline
      val denum = ns.sum(ns.exp(x), axis = 1)
      val y = ns.exp(x) / denum
      y -> ns.sum((ns.log(denum) - x) * targets.value, axis = 1)
    }

    def backprop1(grad: Gradient): Gradient = {
      grad * (y - targets.value)
    }

    def name: String = {
      "CrossEntropyOnSoftmax"
    }
  }

}
