package funcdiff

import botkop.numsca
import numsca._
import TensorExtension._

/**
  * The gradient of a scalar w.r.t a tensor
  */
sealed trait Gradient {
  def shape(): Shape

  def unary_- : Gradient

  /** only true when this is ZeroGradient */
  def isZero: Boolean = false

  def nonZero: Boolean = !isZero

  def deepCopy: Gradient

  def *(d: Real): Gradient

  def *(t: Tensor): Gradient

  def timesBy(t: Tensor): Gradient

  def /(t: Tensor): Gradient

  def /(d: Real): Gradient = {
    require(math.abs(d) > TensorExtension.zeroTolerance)
    this * (1.0 / d)
  }

  def unbroadcast(oldShape: Shape): Gradient

  def broadcast(newShape: Shape): Gradient

  def addToTensor(tensor: Tensor): Unit

  /** Create a sparse gradient tensor whose only non-empty part is this tensor */
  def putInRanges(ranges: Seq[NumscaRange], shape: Shape): Gradient

  /** split gradient tensor along an axis into two */
  def splitAlongAxis(axis: Int, splitAt: Int): (Gradient, Gradient)

  def subGradient(subRegion: Seq[Range]): Gradient

  def toTensor(): Tensor = {
    val r = numsca.zeros(shape())
    addToTensor(r)
    r
  }

  def toDouble: Double = {
    toTensor().squeeze()
  }

  def transpose: Gradient

  def clip(min: Real, max: Real): Gradient

  def clipNorm(maxNorm: Real): Gradient
}

object Gradient {
  def transposeShape(shape: Shape): Shape = {
    require(shape.rank == 2)
    Shape(Vector(shape(1), shape(0)))
  }
}

/**
  * Useful when the target scalar does not depend on the variable
  */
case class ZeroGradient(shape: Shape) extends Gradient {

  override def isZero: Boolean = true

  def unary_- : Gradient = this

  def deepCopy: Gradient = this

  def +(g: Gradient): Gradient = g

  def *(d: Real): Gradient = this

  def unbroadcast(oldShape: Shape): Gradient = ZeroGradient(oldShape)

  def broadcast(newShape: Shape): Gradient = ZeroGradient(newShape)

  def addToTensor(tensor: Tensor): Unit = ()

  def /(t: Tensor): Gradient = this

  def *(t: Tensor): Gradient = this

  def timesBy(tensor: Tensor): Gradient = this

  def splitAlongAxis(axis: Int, splitAt: Int): (Gradient, Gradient) = {
    (
      ZeroGradient(shape.updated(axis, splitAt)),
      ZeroGradient(shape.updated(axis, shape(axis) - splitAt))
    )
  }

  def putInRanges(ranges: Seq[NumscaRange], shape: Shape): Gradient =
    ZeroGradient(shape)

  def transpose: Gradient = ZeroGradient(Gradient.transposeShape(shape))

  def clip(min: Real, max: Real): Gradient = this

  def subGradient(subRegion: Seq[Range]): Gradient = {
    ZeroGradient(Shape(subRegion.map { r =>
      (r.end - r.start).toLong
    }.toVector))
  }

  def clipNorm(maxNorm: Real): Gradient = this
}

case class DenseGradient(value: Tensor) extends Gradient {
  def shape(): Shape = value.shape

  def unary_- : Gradient = DenseGradient(-value)

  def deepCopy: Gradient = DenseGradient(value.copy())

  def *(d: Real): Gradient = DenseGradient(value * d)

  def unbroadcast(oldShape: Shape): Gradient =
    DenseGradient(value.unbroadcast(oldShape))

  def broadcast(newShape: Shape): Gradient =
    DenseGradient(value.broadcast(newShape))

  def addToTensor(tensor: Tensor): Unit = tensor += value

  def /(t: Tensor): Gradient = {
    if (TensorExtension.checkNaN)
      t.requireNonZero()
    DenseGradient(value / t)
  }

  def *(t: Tensor): Gradient = DenseGradient(value * t)

  def timesBy(t: Tensor): Gradient = DenseGradient(t * value)

  def splitAlongAxis(axis: Int, splitAt: Int): (Gradient, Gradient) = {
    val (g1, g2) = value.splitAlongAxis(axis, splitAt)
    DenseGradient(g1) -> DenseGradient(g2)
  }

  def putInRanges(ranges: Seq[NumscaRange], shape: Shape): Gradient = {
    InflatedGradient(value, ranges.toList, shape)
  }

  def transpose: Gradient = {
    DenseGradient(value.transpose())
  }

  def clip(min: Real, max: Real): Gradient = {
    copy(value = value.clip(min, max))
  }

  def subGradient(subRegion: Seq[Range]): Gradient = {
    val ranges = subRegion.map { r =>
      r.start :> r.end
    }
    DenseGradient(value(ranges: _*))
  }

  def clipNorm(maxNorm: Real): Gradient = copy(value = value.clipNorm(maxNorm))
}

/**
  * A sparse gradient tensor with only one non-empty core
  */
case class InflatedGradient(
    core: Tensor,
    ranges: List[NumscaRange],
    shape: Shape
) extends Gradient {
  require(
    TensorExtension.shapeConsistentWithRanges(core.shape, ranges),
    s"core shape: ${core.shape}, ranges: ${TensorExtension.showRanges(ranges)}"
  )

  override def toString: String = {
    s"InflatedGradient($core, ${ranges.map(_.prettyPrint).mkString("[", ",", "]")}, shape=$shape)"
  }

  def deepCopy: Gradient = this.copy(core = core.copy())

  def toDense: DenseGradient = {
    val t = numsca.zeros(shape)
    t(ranges: _*) += core
    DenseGradient(t)
  }

  def unary_- : Gradient = this.copy(core = -core)

  def *(d: Real): Gradient = this.copy(core = core * d)

  def transformCore(t: Tensor, op: (Tensor, Tensor) => Tensor): Gradient = {
    val (axes1, axes2) = TensorExtension.broadcastAxesWhenMerge(shape, t.shape)
    val tRanges = ranges.toArray
    val coreRanges = ranges.toArray
    val newShape = shape.sizes.toArray

    axes1.foreach { axis =>
      newShape(axis) = t.shape(axis)
      tRanges(axis) = :>
      coreRanges(axis) = :>
    }
    axes2.foreach { axis =>
      tRanges(axis) = 0 :> 1
    }

    InflatedGradient(
      core = op(core, t(tRanges: _*)),
      coreRanges.toList,
      Shape(newShape.toVector)
    )
  }

  def *(t: Tensor): Gradient = {
    transformCore(t, _ * _)
  }

  def timesBy(t: Tensor): Gradient = transformCore(t, (a, b) => b * a)

  def /(t: Tensor): Gradient = {
    transformCore(t, _ / _)
  }

  def unbroadcast(oldShape: Shape): Gradient = {
    val axes = TensorExtension.broadcastAxes(oldShape, shape)
    val newRanges = axes.foldLeft(ranges) { (rs, axis) =>
      rs.updated(axis, 0 :> 1)
    }
    InflatedGradient(core.sumAlongAxes(axes), newRanges, oldShape)
  }

  def broadcast(newShape: Shape): Gradient = {
    val axes = TensorExtension.broadcastAxes(shape, newShape)
    val newRanges = axes.foldLeft(ranges) { (rs, axis) =>
      rs.updated(axis, :>)
    }
    val newCoreShape = {
      val s = core.shape.sizes.toArray
      axes.foreach { axis =>
        s(axis) = newShape(axis)
      }
      Shape(s.toVector)
    }

    InflatedGradient(core.broadcast(newCoreShape), newRanges, newShape)
  }

  def addToTensor(tensor: Tensor): Unit = {
    tensor(ranges: _*) += core
  }

  def putInRanges(newRanges: Seq[NumscaRange], newShape: Shape): Gradient = {
    val offsetRanges = ranges.zip(newRanges).map {
      case (r, newR) =>
        r.offset(newR.from)
    }
    InflatedGradient(core, offsetRanges, newShape)
  }

  def splitAlongAxis(axis: Int, splitAt: Int): (Gradient, Gradient) = {
    val range = ranges(axis)

    val lShape = shape.updated(axis, splitAt)
    val rShape = shape.updated(axis, shape(axis) - splitAt)
    if (range.from < splitAt && splitAt < range.to.getOrElse(shape(axis))) {
      val (l, r) = core.splitAlongAxis(axis, splitAt - range.from)
      val lRange = ranges.updated(axis, range.from :> splitAt)
      val rRange = ranges.updated(axis, NumscaRange(0, range.to.map {
        _ - splitAt
      }))
      InflatedGradient(l, lRange, lShape) -> InflatedGradient(r, rRange, rShape)
    } else {
      if (range.from >= splitAt) {
        val rRange = ranges.updated(axis, range.offset(-splitAt))
        ZeroGradient(lShape) -> InflatedGradient(core, rRange, rShape)
      } else {
        InflatedGradient(core, ranges, lShape) -> ZeroGradient(rShape)
      }
    }
  }

  def transpose: Gradient = {
    InflatedGradient(
      core.transpose(),
      List(ranges(1), ranges.head),
      Gradient.transposeShape(shape)
    )
  }

  def clip(min: Real, max: Real): Gradient = {
    copy(core = core.clip(min, max))
  }

  def subGradient(subRegion: Seq[Range]): Gradient = {
    throw new Exception(
      "subGradient on sparse gradient not supported. Turn the gradient into dense instead."
    )
  }

  def clipNorm(maxNorm: Real): Gradient = copy(core = core.clipNorm(maxNorm))
}

/** Mutable buffer used to accumulate gradients */
class GradientBuilder(
    private var value: Gradient,
    private var needCopy: Boolean
) {

  def add(grad: Gradient): Unit = this synchronized {
    value match {
      case _: ZeroGradient =>
        value = grad
        needCopy = true
      case dense: DenseGradient =>
        grad match {
          case _: ZeroGradient => ()
          case i: InflatedGradient =>
            if (needCopy) {
              val c = dense.value.copy()
              c(i.ranges: _*) += i.core
              value = DenseGradient(c)
              needCopy = false
            } else {
              dense.value(i.ranges: _*) += i.core
            }
          case d: DenseGradient =>
            if (needCopy) {
              val c = dense.value.copy()
              c += d.value
              value = DenseGradient(c)
              needCopy = false
            } else {
              dense.value += d.value
            }
        }
      case inflated: InflatedGradient =>
        grad match {
          case _: ZeroGradient => ()
          case _ =>
            value = inflated.toDense
            needCopy = false
            add(grad)
        }
    }
    this
  }

  def retrieve: Gradient = {
    needCopy = true
    value
  }
}
