package botkop

import funcdiff.DebugTime
import org.nd4j.linalg.api.iter.NdIndexIterator
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.api.ops.impl.indexaccum.{IMax, IMin}
import org.nd4j.linalg.api.ops.impl.transforms.custom.{
  GreaterThanOrEqual,
  LessThanOrEqual,
}
import org.nd4j.linalg.api.ops.random.impl.Choice
import org.nd4j.linalg.api.rng
import org.nd4j.linalg.factory.{NDArrayFactory, Nd4j}
import org.nd4j.linalg.factory.Nd4j.PadMode
import org.nd4j.linalg.indexing.{INDArrayIndex, NDArrayIndex}
import org.nd4j.linalg.ops.transforms.Transforms

import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scala.util.Random

package object numsca {

  implicit def selectionToTensor(ts: TensorSelection): Tensor =
    ts.asTensor

  implicit class NumscaDoubleOps(d: Double) {
    def +(t: Tensor): Tensor = t + d
    def -(t: Tensor): Tensor = -t + d
    def *(t: Tensor): Tensor = t * d
    def /(t: Tensor): Tensor = numsca.power(t, -1) * d
  }

  case class NumscaRange(from: Long, to: Option[Long])

  def :>(end: Long) = NumscaRange(0, Some(end))
  def :> = NumscaRange(0, None)

  implicit class NumscaInt(i: Long) {
    def :>(end: Long) = NumscaRange(i, Some(end))
    def :> = NumscaRange(i, None)
  }

  implicit def intToNumscaRange(i: Int): NumscaRange =
    NumscaRange(i, Some(i + 1))

  implicit def longToNumscaRange(i: Long): NumscaRange =
    NumscaRange(i, Some(i + 1))

  def rand: rng.Random = Nd4j.getRandom

  def array(ds: Double*) = Tensor(ds: _*)
  def zeros(shape: Long*): Tensor = new Tensor(Nd4j.zeros(shape: _*))
  def zeros(shape: Shape): Tensor = zeros(shape.sizes: _*)
  def zerosLike(t: Tensor): Tensor = zeros(t.shape)

  def ones(shape: Long*): Tensor = {
    if (shape.length == 1)
      // probably a bug in nd4j
      new Tensor(Nd4j.ones(1L +: shape: _*))
    else
      new Tensor(Nd4j.ones(shape: _*))
  }
  def ones(shape: Shape): Tensor = ones(shape.sizes: _*)

  def full(shape: Shape, value: Double): Tensor = zeros(shape) + value

  def randn(shape: Long*): Tensor = new Tensor(Nd4j.randn(shape.toArray: _*))
  def randn(shape: Shape): Tensor = randn(shape.sizes: _*)

  def rand(shape: Long*): Tensor = new Tensor(Nd4j.rand(shape.toArray))
  def rand(shape: Shape): Tensor = rand(shape.sizes: _*)

  def randint(low: Int, shape: Shape): Tensor = {
    val data = Array.fill(shape.ints.product)(Random.nextInt(low).toDouble)
    Tensor(data).reshape(shape)
  }

  def uniform(
      low: Double = 0.0,
      high: Double = 1.0,
      shape: Array[Int],
  ): Tensor =
    (new Tensor(Nd4j.randn(shape)) - low) / (high - low)

  def copy(t: Tensor): Tensor = t.copy()

  def abs(t: Tensor): Tensor = new Tensor(Transforms.abs(t.array))

  def maximum(t: Tensor, d: Double): Tensor = t.maximum(d)
  def maximum(a: Tensor, b: Tensor): Tensor = a.maximum(b)
  def minimum(t: Tensor, d: Double): Tensor = t.minimum(d)
  def minimum(a: Tensor, b: Tensor): Tensor = a.minimum(b)

  type Axis = Int
  case class axisOps(op: (INDArray, Axis) => INDArray) {
    def apply(t: Tensor, axis: Int, keepDim: Boolean = true): Tensor = {
      val x1 = new Tensor(op(t.array, axis))
      if (keepDim) {
        val s1 = {
          val a = if (axis < 0) t.shape.rank + axis else axis
          t.shape.sizes.updated(a, 1L)
        }
        x1.reshape(Shape(s1))
      } else x1
    }
  }

  def max(t: Tensor): Tensor = new Tensor(Nd4j.max(t.array))
  val maxAxis = axisOps(Nd4j.max)
  def min(t: Tensor): Tensor = new Tensor(Nd4j.min(t.array))
  val minAxis = axisOps(Nd4j.min)

  def sum(t: Tensor): Double = Nd4j.sum(t.array).getDouble(0L)
  val sumAxis = axisOps(Nd4j.sum)

  def prod(t: Tensor): Double = Nd4j.prod(t.array).getDouble(0L)
  val prodAxis = axisOps(Nd4j.prod)

  def arange(end: Double): Tensor = new Tensor(Nd4j.arange(end))
  def arange(start: Double, end: Double): Tensor =
    new Tensor(Nd4j.arange(start, end))

  def sigmoid(t: Tensor): Tensor = new Tensor(Transforms.sigmoid(t.array))
  def softmax(t: Tensor): Tensor = new Tensor(Transforms.softmax(t.array))
  def relu(t: Tensor): Tensor = new Tensor(Transforms.relu(t.array))
  def tanh(t: Tensor): Tensor = new Tensor(Transforms.tanh(t.array))
  def log(t: Tensor): Tensor = new Tensor(Transforms.log(t.array))
  def power(t: Tensor, pow: Double): Tensor =
    new Tensor(Transforms.pow(t.array, pow))
  def exp(t: Tensor): Tensor = new Tensor(Transforms.exp(t.array))
  def sqrt(t: Tensor): Tensor = new Tensor(Transforms.sqrt(t.array))
  def square(t: Tensor): Tensor = power(t, 2)

  def nditer(t: Tensor): Iterator[Array[Long]] = nditer(t.shape)
  def nditer(shape: Shape): Iterator[Array[Long]] =
    new NdIndexIterator(shape.sizes: _*).asScala

  def argmax(t: Tensor): Tensor =
    new Tensor(Nd4j.getExecutioner.exec(new IMax(t.array)))
  val argmaxAxis =
    axisOps((x, a) => Nd4j.getExecutioner.exec(new IMax(x, a)))
  def argmin(t: Tensor, axis: Int): Tensor =
    new Tensor(Nd4j.getExecutioner.exec(new IMin(t.array, axis)))

  val argminAxis =
    axisOps((x, a) => Nd4j.getExecutioner.exec(new IMin(x, a)))

  def round(t: Tensor): Tensor = new Tensor(Transforms.round(t.array))
  def ceil(t: Tensor): Tensor = new Tensor(Transforms.ceil(t.array))
  def floor(t: Tensor): Tensor = new Tensor(Transforms.floor(t.array))

  def mean(t: Tensor): Tensor = new Tensor(Nd4j.mean(t.array))
  val meanAxis = axisOps(Nd4j.mean)

  // really do not understand how they calculate the variance and std in nd4j
  def variance(t: Tensor): Tensor = mean((t - mean(t)) ** 2)
  def variance(t: Tensor, axis: Int, keepDim: Boolean = true): Tensor =
    meanAxis((t - meanAxis(t, axis, keepDim)) ** 2, axis, keepDim)
  def std(t: Tensor): Tensor = sqrt(variance(t))
  def std(t: Tensor, axis: Int, keepDim: Boolean = true): Tensor =
    sqrt(variance(t, axis, keepDim))

  /*
  def variance(t: Tensor): Tensor = new Tensor(Nd4j.`var`(t.array))
  def variance(t: Tensor, axis: Int): Tensor =
    new Tensor(Nd4j.`var`(t.array, axis))
  def std(t: Tensor): Tensor = new Tensor(Nd4j.std(t.array))
  def std(t: Tensor, axis: Int): Tensor = new Tensor(Nd4j.std(t.array, axis))
   */

  def multiply(a: Tensor, b: Tensor): Tensor = a * b
  def dot(a: Tensor, b: Tensor): Tensor = a dot b

  def pad(x: Tensor, padWidth: Array[Array[Int]], mode: PadMode): Tensor = {
    val a = Nd4j.pad(x.array, padWidth, mode)
    new Tensor(a)
  }

  def clip(t: Tensor, min: Double, max: Double): Tensor = t.clip(min, max)

  /** only works for 2D Tensors for performance reason */
  def concat(ts: Seq[Tensor], axis: Int): Tensor = {
    require(ts.nonEmpty)
    val shapes = ts.map(_.shape)
    val sumAxis = shapes.map(s => s(axis)).sum
    val first = ts.head
    val newShape = if (axis == 0) {
      Array(sumAxis, first.shape(1))
    } else {
      assert(axis == 1)
      Array(first.shape(0), sumAxis)
    }
    val ordering = if (axis == 0) NDArrayFactory.C else NDArrayFactory.FORTRAN
    val newArray =
      Nd4j.createUninitialized(first.array.dataType(), newShape, ordering)
    var pos = 0L
    ts.foreach { t =>
      val nextPos = pos + t.shape(axis)
      val inter = NDArrayIndex.interval(pos, nextPos)
      if (axis == 0)
        newArray.put(Array(inter, NDArrayIndex.all()), t.array)
      else newArray.put(Array(NDArrayIndex.all(), inter), t.array)
      pos = nextPos
    }

//    val newArray = Nd4j.concat(axis, ts.map(_.array): _*)
    new Tensor(newArray)
  }

  def fromRows(rows: Seq[Tensor], axis: Int): Tensor = {
    require(axis == 0 || axis == 1)
    val first = rows.head
    assert(first.shape(0) == 1, s"shape = ${first.shape}")
    val columns = first.shape(1)
    val n = rows.length
//    val data = Nd4j.createUninitialized(Array(columns * n), NDArrayFactory.C)
//    var pos = 0L
//    rows.foreach { r =>
//      val nextPos = pos + columns
//      data.put(Array(NDArrayIndex.interval(pos, nextPos)), r.array)
//      pos = nextPos
//    }
    val data = Nd4j.create(rows.toArray.flatMap(_.data), NDArrayFactory.C)
    val r =
      if (axis == 0) data.reshape(n, columns) else data.reshape(1, n * columns)
    new Tensor(r)
  }

  def reshape(x: Tensor, shape: Shape): Tensor = x.reshape(shape)

  def transpose(x: Tensor): Tensor = x.transpose()
  def transpose(x: Tensor, axes: Array[Int]): Tensor = x.transpose(axes: _*)

  def arrayEqual(t1: Tensor, t2: Tensor): Boolean =
    numsca.prod((t1 == t2).boolToFloating) == 1

  def choice(a: Tensor, p: Tensor, size: Option[Array[Int]] = None): Tensor = {
    val z = Nd4j.zeros(a.shape.ints: _*)
    Nd4j.getExecutioner.exec(new Choice(a.array, p.array, z))
    if (size.isEmpty) {
      new Tensor(z.getScalar(0L))
    } else {
      new Tensor(z.getScalar(size.get: _*))
    }
  }

  // ops between 2 tensors, with broadcasting
  object Ops {

    def binOp(
        op: (INDArray, INDArray) => INDArray,
    )(t1: Tensor, t2: Tensor): Tensor = {
      new Tensor(op(t1.array, t2.array))
    }

    def max(t1: Tensor, t2: Tensor): Tensor = {
      binOp(Transforms.max)(t1, t2)
    }

    def min(t1: Tensor, t2: Tensor): Tensor = {
      binOp(Transforms.min)(t1, t2)
    }

    def prepareShapesForBroadcast(sa: Seq[INDArray]): Seq[INDArray] = {
      val maxRank = sa.map(_.rank()).max
      sa.map { a =>
        val diff = maxRank - a.rank()
        val extShape = Array.fill(diff)(1L)
        a.reshape(extShape ++ a.shape(): _*)
      }
    }

    def broadcastArrays(sa: Seq[INDArray]): Seq[INDArray] = {
      val xa = prepareShapesForBroadcast(sa)
      val rank = xa.head.rank()
      val finalShape: Array[Long] =
        xa.map(_.shape()).foldLeft(Array.fill(rank)(0L)) {
          case (shp, acc) =>
            shp.zip(acc).map { case (a, b) => math.max(a, b) }
        }
      xa.map(a => a.broadcast(finalShape: _*))
    }

    /** make two tensors have the same shape, broadcast them if necessary */
    @deprecated("because of bad performance")
    def broadcast2(t1: Tensor, t2: Tensor): Seq[INDArray] = {

      val rank = t1.shape.rank.max(t2.shape.rank)
      val s1 = Vector.fill(rank - t1.shape.rank)(1L) ++ t1.shape.sizes
      val s2 = Vector.fill(rank - t2.shape.rank)(1L) ++ t2.shape.sizes
      val newShape = s1.zip(s2).map {
        case (a, b) =>
          if (a != b) {
            if (a == 1L || b == 1L)
              a.max(b)
            else {
              throw new Error(s"incompatible broadcasting: a = $a, b = $b")
            }
          } else a
      }
      Seq(
        if (t1.shape.sizes != newShape)
          t1.array.reshape(s1: _*).broadcast(newShape: _*)
        else t1.array,
        if (t2.shape.sizes != newShape)
          t2.array.reshape(s2: _*).broadcast(newShape: _*)
        else t2.array,
      )
    }

//    def tbc(ts: Tensor*): Seq[INDArray] =
//      broadcastArrays(ts.map(_.array))

  }

}
