package funcdiff

import botkop.numsca._
import TensorExtension._
import botkop.numsca
import botkop.{numsca => ns}

trait DiffFunc {
  def name: String

  def shortName: String = name

  def args: IS[CompNode]

  def value: Tensor

  def backProp(grad: Gradient): IS[Gradient]

}

private[funcdiff] object DiffFunc {

  case class ConstFunc(value: Tensor) extends DiffFunc {
    def name: String = {
      s"const{shape=${value.shape}}"
    }

    override def shortName: String = "const"

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

  case class Reshape(newShape: Shape, x1: CompNode) extends UnaryFunc {
    def name = s"reshape{newShape=$newShape}"

    override def shortName = "reshape"

    val value = x1.value.reshape(newShape)

    def backprop1(grad: Gradient): Gradient = {
      grad.reshape(x1.shape)
    }
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
    require(x1.shape.elements > 0)
    val value: Tensor = numsca.mean(x1.value)

    def backprop1(grad: Gradient): Gradient = {
      val n = x1.shape.elements
      require(n > 0)
      (grad / n).broadcast(x1.shape)
    }

    def name: String = "mean"
  }

  case class MeanByAxis(x1: CompNode, axis: Int, keepDim: Boolean = true)
      extends UnaryFunc {
    require(x1.shape.elements > 0)
    val value: Tensor = numsca.meanAxis(x1.value, axis, keepDim)

    def backprop1(grad: Gradient): Gradient = {
      val n = x1.shape(axis)
      require(n > 0)
      (grad / n).broadcast(x1.shape)
    }

    def name: String = s"mean{axis=$axis}"

    override def shortName: String = "meanByAxis"

  }

  case class Sum(x1: CompNode) extends UnaryFunc {
    val value = Tensor(ns.sum(x1.value))

    def backprop1(grad: Gradient): Gradient = {
      grad.broadcast(x1.shape)
    }

    def name: String = "sum"
  }

  case class SumByAxis(x1: CompNode, axis: Int, keepDim: Boolean = true)
      extends UnaryFunc {
    val value: Tensor = ns.sumAxis(x1.value, axis, keepDim)

    def backprop1(grad: Gradient): Gradient = {
      grad.broadcast(x1.shape)
    }

    def name: String = s"sum{axis=$axis}"

    override def shortName: String = "sumByAxis"

  }

  case class Threshold(x1: CompNode, threshold: Double) extends UnaryFunc {
    val value: Tensor = ns.maximum(x1.value, threshold)

    def backprop1(grad: Gradient): Gradient = {
      grad * (x1.value > threshold).boolToFloating
    }

    def name: String = s"threshold{t=$threshold}"

    override def shortName: String = "threshold"
  }

  case class LeakyRelu(x1: CompNode, slope: Double) extends UnaryFunc {
    val value
        : Tensor = ns.maximum(x1.value, 0.0) + ns.minimum(x1.value, 0.0) * slope

    def backprop1(grad: Gradient): Gradient = {
      grad * ((x1.value > 0).boolToFloating + (x1.value < 0).boolToFloating * slope)
    }

    def name: String = s"leakyRelu{slope=$slope}"

    override def shortName: String = "leakyRelu"

  }

  case class Slice(x1: CompNode, ranges: Seq[NumscaRange]) extends UnaryFunc {
    require(
      x1.shape.sizes.zip(ranges).forall {
        case (s, r) => r.to.forall { _ <= s }
      },
      s"slice out of range. num1 shape: ${x1.shape}, ranges: ${showRanges(ranges)}"
    )
    val value = x1.value.apply(ranges: _*)

    def backprop1(grad: Gradient): Gradient = {
      grad.putInRanges(ranges, x1.shape)
    }

    def name: String = s"slice{ranges=${TensorExtension.showRanges(ranges)}}"

    override def shortName: String = "slice"
  }

  case class GetRow(x1: CompNode, i: Int, keepDim: Boolean = true) extends UnaryFunc {
    val value = Tensor(x1.value.array.getRow(i, keepDim))

    def name: String = s"getRow{i=$i, keepDim=$keepDim}"

    def backprop1(grad: Gradient): Gradient = {
      grad.putInRanges(List(i, :>), x1.shape)
    }

    override def shortName: String = "getRow"
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

    override def shortName: String = "power"
  }

  /** softmax along the last dimension */
  case class Softmax(x1: CompNode) extends UnaryFunc {
    val value: Tensor = ns.softmax(x1.value)

    def backprop1(grad: Gradient): Gradient = {
      val y = value
      val dy = grad.toTensor()
      val dx = dy * y
      val s = ns.sumAxis(dx, axis = dx.shape.rank - 1)
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

  case class Divide(x1: CompNode, x2: CompNode) extends BinaryFunc {
    x2.value.requireNonZero(halfZeroTolerance)
    val value: Tensor = x1.value / x2.value

    def backProp2(grad: Gradient): (Gradient, Gradient) = {
      val dx1 = (grad / x2.value).unbroadcast(x1.shape)
      val dx2 = (-grad * x1.value / (x2.value * x2.value)).unbroadcast(x2.shape)
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

    val value: Tensor = numsca.concat(Seq(x1.value, x2.value), axis)

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
      (grad * (1.0 - (x1.value < x2.value).boolToFloating))
        .unbroadcast(x1.shape) ->
        (grad * (x1.value < x2.value).boolToFloating).unbroadcast(x2.shape)
    }

    def name: String = "maxBinary"
  }

  // ================ N-nary functions ==================
  case class PlusN(args: IS[CompNode]) extends DiffFunc {
    require(args.nonEmpty)
    private val shape = args.head.shape
    require(args.forall(_.shape == shape))

    val value: Tensor = {
      val acc = ns.zeros(shape)
      args.foreach(a => acc.array.addi(a.value.array))
      acc
    }

    def name: String = "plusN"

    def backProp(grad: Gradient): IS[Gradient] = {
      args.map { _ =>
        grad
      }
    }
  }

  /**
   * @param fromRows Whether all arguments are row vectors and have the same shape.
   */
  case class ConcatN(args: IS[CompNode], axis: Int, fromRows: Boolean)
      extends DiffFunc {

    val value: Tensor =
      if (fromRows) ns.fromRows(args.map(_.value), axis)
      else ns.concat(args.map(_.value), axis)

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

  case class StackRows(args: IS[CompNode]) extends DiffFunc {
    if (checkNaN) {
      args.forall(_.shape(0) == 1)
    }

    def name: String = "stackRows"

    def value: Tensor = {
      val current = Thread.currentThread().getContextClassLoader
      Thread.currentThread().setContextClassLoader(this.getClass.getClassLoader)
      val v = ns.stackTensorRows(args.map(_.value))
      Thread.currentThread().setContextClassLoader(current)
      v
    }

    def backProp(grad: Gradient): IS[Gradient] = {
      grad match {
        case _: ZeroGradient =>
          val g = ZeroGradient(args.head.shape)
          IS.fill(args.length)(g)
        case _ =>
          grad.toTensor().rows.map(DenseGradient)
      }
    }
  }

  // ================ Loss functions ======================
  case class CrossEntropyOnSoftmax(logits: CompNode, targets: Tensor)
      extends UnaryFunc {
    require(
      targets.shape == logits.shape,
      s"Targets shape (${targets.shape}) is different from logits (${logits.shape})."
    )
    require(logits.shape.rank == 2, "Logits should be of rank 2.")

    def x1: CompNode = logits

    val (y, value) = {
      val baseline = ns.maxAxis(logits.value, axis = 1)
      val x = logits.value - baseline
      val denum = ns.sumAxis(ns.exp(x), axis = 1)
      val y = ns.exp(x) / denum
      y -> ns.sumAxis(((-x) + ns.log(denum)) * targets, axis = 1)
    }

    def backprop1(grad: Gradient): Gradient = {
      (y - targets) * grad
    }

    def name: String = {
      "CrossEntropyOnSoftmax"
    }
  }

  case class CrossEntropyOnSigmoid(logits: CompNode, targets: Tensor)
      extends UnaryFunc {
    require(targets.shape(1) == 1)
    require(logits.shape(1) == 1)

    def x1: CompNode = logits

    val value = {
      val l = ns.log(ns.exp(-logits.value) + 1)
      l * targets + (-targets + 1) * (logits.value + l)
    }

    def backprop1(grad: Gradient): Gradient = {
      (ns.sigmoid(logits.value) - targets) * grad
    }

    def name: String = {
      "CrossEntropyOnSigmoid"
    }
  }

}
