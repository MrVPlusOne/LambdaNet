package funcdiff

import botkop.numsca
import numsca._
import TensorExtension._

/**
  * The gradient of a scalar w.r.t a tensor
  */
sealed trait Gradient{
  def shape: Array[Int]

  def unary_- : Gradient

  /** only true when this is ZeroGradient */
  def isZero: Boolean = false

  def nonZero: Boolean = !isZero

  def deepCopy: Gradient

  def * (d: Real): Gradient

  def * (t: Tensor): Gradient

  def / (t: Tensor): Gradient

  def / (d: Real): Gradient = {
    require(math.abs(d) > TensorExtension.zeroTolerance)
    this * (1.0/d)
  }

  def unbroadcast(oldShape: Array[Int]): Gradient

  def broadcast(newShape: Array[Int]): Gradient

  def addToTensor(tensor: Tensor): Unit

  /** Create a sparse gradient tensor whose only non-empty part is this tensor */
  def putInRanges(ranges: Seq[NumscaRange], shape: Array[Int]): Gradient

  /** split gradient tensor along an axis into two */
  def splitAlongAxis(axis: Int, splitAt: Int): (Gradient, Gradient)

  def toTensor(shape: Array[Int]): Tensor = {
    val r = numsca.zeros(shape)
    addToTensor(r)
    r
  }

  def toDouble: Double = {
    toTensor(Array(1)).squeeze()
  }

  def transpose: Gradient

  def clip(min: Real, max: Real): Gradient
}

object Gradient{
  def transposeShape(shape: Array[Int]): Array[Int] = {
    require(shape.length == 2)
    Array(shape(1), shape(0))
  }
}

/**
  * Useful when the target scalar does not depend on the variable
  */
case class ZeroGradient(shape: Array[Int]) extends Gradient{

  override def isZero: Boolean = true

  def unary_- : Gradient = this

  def deepCopy: Gradient = this

  def +(g: Gradient): Gradient = g

  def *(d: Real): Gradient = this

  def unbroadcast(oldShape: Array[Int]): Gradient = ZeroGradient(oldShape)

  def broadcast(newShape: Array[Int]): Gradient = ZeroGradient(newShape)

  def addToTensor(tensor: Tensor): Unit = ()

  def /(t: Tensor): Gradient = this

  def *(t: Tensor): Gradient = this

  def splitAlongAxis(axis: Int, splitAt: Int): (Gradient, Gradient) = {
    (ZeroGradient(shape.updated(axis, splitAt)),
      ZeroGradient(shape.updated(axis, shape(axis) - splitAt)))
  }

  def putInRanges(ranges: Seq[NumscaRange], shape: Array[Int]): Gradient = ZeroGradient(shape)

  def transpose: Gradient = ZeroGradient(Gradient.transposeShape(shape))

  def clip(min: Real, max: Real): Gradient = this
}

case class DenseGradient(value: Tensor) extends Gradient {
  def shape: Array[Int] = value.shape

  def unary_- : Gradient = DenseGradient(-value)

  def deepCopy: Gradient = DenseGradient(value.copy())

  def *(d: Real): Gradient = DenseGradient(value * d)

  def unbroadcast(oldShape: Array[Int]): Gradient = DenseGradient(value.unbroadcast(oldShape))

  def broadcast(newShape: Array[Int]): Gradient = DenseGradient(value.broadcast(newShape))

  def addToTensor(tensor: Tensor): Unit = tensor += value

  def /(t: Tensor): Gradient = {
    if(TensorExtension.checkNaN)
      t.requireNonZero()
    DenseGradient(value / t)
  }

  def *(t: Tensor): Gradient = DenseGradient(value * t)

  def splitAlongAxis(axis: Int, splitAt: Int): (Gradient, Gradient) = {
    val (g1, g2) = value.splitAlongAxis(axis, splitAt)
    DenseGradient(g1) -> DenseGradient(g2)
  }

  def putInRanges(ranges: Seq[NumscaRange], shape: Array[Int]): Gradient = {
    InflatedGradient(value, ranges.toList, shape)
  }

  def transpose: Gradient = {
    DenseGradient(value.transpose())
  }

  def clip(min: Real, max: Real): Gradient = {
    copy(value = value.clip(min, max))
  }
}

/**
  * A sparse gradient tensor with only one non-empty core
  */
case class InflatedGradient(core: Tensor, ranges: List[NumscaRange], shape: Array[Int]) extends Gradient {
  require(TensorExtension.shapeConsistentWithRanges(core.shape, ranges),
    s"core shape: ${showShape(core.shape)}, ranges: ${TensorExtension.showRanges(ranges)}")

  override def toString: String = {
    s"InflatedGradient($core, ${ranges.map(_.prettyPrint).mkString("[",",","]")}, shape=${shape.mkString("{",",","}")})"
  }

  def deepCopy: Gradient = this.copy(core=core.copy())

  def toDense: DenseGradient = {
    val t = numsca.zeros(shape)
    t(ranges :_*) += core
    DenseGradient(t)
  }

  def unary_- : Gradient = this.copy(core = -core)

  def *(d: Real): Gradient = this.copy(core = core * d)

  def transformCore(t: Tensor, op: (Tensor, Tensor) => Tensor): Gradient = {
    val (axes1, axes2) = TensorExtension.broadcastAxesWhenMerge(shape, t.shape)
    val tRanges = ranges.toArray
    val coreRanges = ranges.toArray
    val newShape = shape.clone()

    axes1.foreach{ axis =>
      newShape(axis) = t.shape(axis)
      tRanges(axis) = :>
      coreRanges(axis) = :>
    }
    axes2.foreach{ axis =>
      tRanges(axis) = 0 :> 1
    }

    InflatedGradient(core = op(core, t(tRanges :_*)), coreRanges.toList, newShape)
  }

  def *(t: Tensor): Gradient = {
    transformCore(t, _ * _)
  }

  def /(t: Tensor): Gradient = {
    transformCore(t, _ / _)
  }

  def unbroadcast(oldShape: Array[Int]): Gradient = {
    val axes = TensorExtension.broadcastAxes(oldShape, shape)
    val newRanges = axes.foldLeft(ranges){ (rs, axis) => rs.updated(axis, 0:>1)}
    InflatedGradient(core.sumAlongAxes(axes), newRanges, oldShape)
  }

  def broadcast(newShape: Array[Int]): Gradient = {
    val axes = TensorExtension.broadcastAxes(shape, newShape)
    val newRanges = axes.foldLeft(ranges){ (rs, axis) => rs.updated(axis, :>)}
    val newCoreShape = {
      val s = core.shape.clone()
      axes.foreach{ axis =>
        s(axis) = newShape(axis)
      }
      s
    }

    InflatedGradient(core.broadcast(newCoreShape), newRanges, newShape)
  }

  def addToTensor(tensor: Tensor): Unit = {
    tensor(ranges :_*) += core
  }

  def putInRanges(newRanges: Seq[NumscaRange], newShape: Array[Int]): Gradient = {
    val offsetRanges = ranges.zip(newRanges).map{ case (r, newR) =>
        r.offset(newR.from)
    }
    InflatedGradient(core, offsetRanges, newShape)
  }

  def splitAlongAxis(axis: Int, splitAt: Int): (Gradient, Gradient) = {
    val range = ranges(axis)

    val lShape = shape.updated(axis, splitAt)
    val rShape = shape.updated(axis, shape(axis) - splitAt)
    if(range.from < splitAt && splitAt < range.to.getOrElse(shape(axis))){
      val (l, r) = core.splitAlongAxis(axis, splitAt - range.from)
      val lRange = ranges.updated(axis, range.from :> splitAt)
      val rRange = ranges.updated(axis, NumscaRange(0, range.to.map{_  - splitAt}))
      InflatedGradient(l, lRange, lShape) -> InflatedGradient(r, rRange, rShape)
    } else {
      if(range.from >= splitAt) {
        val rRange = ranges.updated(axis, range.offset(-splitAt))
        ZeroGradient(lShape) -> InflatedGradient(core, rRange, rShape)
      } else {
        InflatedGradient(core, ranges, lShape) -> ZeroGradient(rShape)
      }
    }
  }

  def transpose: Gradient = {
    InflatedGradient(core.transpose(), List(ranges(1), ranges.head), Gradient.transposeShape(shape))
  }

  def clip(min: Real, max: Real): Gradient = {
    copy(core = core.clip(min, max))
  }
}

/** Mutable buffer used to accumulate gradients */
class GradientBuilder (private var value: Gradient, private var needCopy: Boolean){

  def add(grad: Gradient): Unit = {
    value match {
      case _: ZeroGradient =>
        value = grad
        needCopy = true
      case dense: DenseGradient =>
        grad match {
          case _: ZeroGradient => ()
          case i: InflatedGradient =>
            if(needCopy){
              val c = dense.value.copy()
              c(i.ranges :_*) += i.core
              value = DenseGradient(c)
              needCopy = false
            } else {
              dense.value(i.ranges :_*) += i.core
            }
          case d: DenseGradient =>
            if(needCopy){
              val c = dense.value.copy()
              c += d.value
              value = DenseGradient(c)
              needCopy = false
            } else{
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
  }

  def retrieve: Gradient = {
    needCopy = true
    value
  }
}