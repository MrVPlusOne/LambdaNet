package botkop.numsca

import funcdiff.{Gradient, Real}
import org.nd4j.linalg.api.buffer.DataType
import org.nd4j.linalg.api.iter.NdIndexIterator
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.{Broadcast, Nd4j}
import org.nd4j.linalg.indexing.{INDArrayIndex, NDArrayIndex}
import org.nd4j.linalg.ops.transforms.Transforms

import scala.language.{implicitConversions, postfixOps}

class Tensor(val array: INDArray) extends Serializable {

  val shape: Shape = Shape.fromArray(array.shape())

  lazy val data: Array[Double] = array.dup.data.asDouble

  def newTensor(array: INDArray): Tensor = new Tensor(array)

  /** Total number of elements in this tensor */
  def elements: Long = shape.elements

  def copy(): Tensor = new Tensor(array.dup())

  def reshape(newShape: Shape) = newTensor(array.reshape(newShape.sizes: _*))
  def reshape(newShape: Int*) = newTensor(array.reshape(newShape.toArray))
  def shapeLike(t: Tensor): Tensor = reshape(t.shape)

  def transpose() = new Tensor(array.transpose())
  def T: Tensor = transpose()
  def transpose(axes: Int*): Tensor = {
    require(axes.sorted == shape.sizes.indices, "invalid axes")
    val newShape = Shape(axes.map(a => shape(a)).toVector)
    reshape(newShape)
  }

  def round: Tensor =
    Tensor(data.map(math.round(_).toDouble)).reshape(this.shape)

  def dot(other: Tensor) = Tensor(array mmul other.array)

  def boolToFloating: Tensor = {
    new Tensor(array.castTo(Tensor.floatingDataType))
  }

  def unary_- : Tensor = Tensor(array.neg)
  def +(d: Double): Tensor = Tensor(array add d)
  def -(d: Double): Tensor = Tensor(array sub d)
  def *(d: Double): Tensor = Tensor(array mul d)
  def **(d: Double): Tensor = power(this, d)
  def /(d: Double): Tensor = Tensor(array div d)
  def %(d: Double): Tensor = Tensor(array fmod d)

  def +=(d: Double): Unit = array addi d
  def -=(d: Double): Unit = array subi d
  def *=(d: Double): Unit = array muli d
  def **=(d: Double): Unit = array.assign(Transforms.pow(array, d))
  def /=(d: Double): Unit = array divi d
  def %=(d: Double): Unit = array fmodi d

  def >(d: Double): Tensor = new Tensor(array gt d)
  def >=(d: Double): Tensor = new Tensor(array gte d)
  def <(d: Double): Tensor = new Tensor(array lt d)
  def <=(d: Double): Tensor = new Tensor(array lte d)
  def ==(d: Double): Tensor = new Tensor(array eq d)
  def !=(d: Double): Tensor = new Tensor(array neq d)

  def *(grad: Gradient) = grad.timesBy(this)

  @inline
  def selectOp(
      scalarOp: (INDArray, Double) => INDArray,
      rowOp: (INDArray, INDArray) => INDArray,
      colOp: (INDArray, INDArray) => INDArray,
      tensorOp: (INDArray, INDArray) => INDArray
  )(other: Tensor): Tensor = {
    val v1 = array
    val v2 = other.array
    if (rank == 2) {
      if (other.elements == 1) return Tensor(scalarOp(v1, other.squeeze()))
      else {
        assert(other.rank == 2)
        if (other.shape(0) == 1) return Tensor(rowOp(v1, v2))
        else if (other.shape(1) == 1) return Tensor(colOp(v1, v2))
      }
    }

    Tensor(tensorOp(v1, v2))
  }

  def +(other: Tensor): Tensor =
    selectOp(_ add _, _ addRowVector _, _ addColumnVector _, _ add _)(other)

  def -(other: Tensor): Tensor = this + (-other)
  def *(other: Tensor): Tensor =
    selectOp(_ mul _, _ mulRowVector _, _ mulColumnVector _, _ mul _)(other)
  def /(other: Tensor): Tensor =
    selectOp(_ div _, _ divRowVector _, _ divColumnVector _, _ div _)(other)

  def >(other: Tensor): Tensor = Ops.binOp(_.gt(_))(this, other)
//  def >=(other: Tensor): Tensor = Ops.gte(this, other)
  def <(other: Tensor): Tensor = Ops.binOp(_.lt(_))(this, other)
//  def <=(other: Tensor): Tensor = Ops.lte(this, other)
  def ==(other: Tensor): Tensor = Ops.binOp(_.eq(_))(this, other)
  def !=(other: Tensor): Tensor = Ops.binOp(_.neq(_))(this, other)

  def forall: Boolean = array.all()

  def any: Boolean = array.any()

  /**
    * broadcast argument tensor with shape of this tensor
    */
  private def bc(t: Tensor): INDArray = {
    if (t.shape == this.shape) t.array
    else {
      val rankDiff = this.shape.rank - t.shape.rank
      if (rankDiff > 0) {
        val fill = Vector.fill(rankDiff)(1L)
        val s1 = t.shape.sizes ++ fill
        t.array.reshape(s1: _*).broadcast(shape.sizes: _*)
      } else {
        t.array.broadcast(shape.sizes: _*)
      }
    }
  }

  def +=(t: Tensor): Unit = array addi bc(t)
  def -=(t: Tensor): Unit = array subi bc(t)
  def *=(t: Tensor): Unit = array muli bc(t)
  def /=(t: Tensor): Unit = array divi bc(t)
  def %=(t: Tensor): Unit = array fmodi bc(t)

  def :=(t: Tensor): Unit = array assign t.array
  def :=(d: Double): Unit = array assign d

  def maximum(other: Tensor): Tensor = Ops.max(this, other)
  def maximum(d: Double): Tensor = new Tensor(Transforms.max(this.array, d))
  def minimum(other: Tensor): Tensor = Ops.min(this, other)
  def minimum(d: Double): Tensor = new Tensor(Transforms.min(this.array, d))

  def ravel(): Tensor = newTensor(array.ravel())
  def ravel(c: Char): Tensor = newTensor(array.ravel(c))

  def slice(i: Int): Tensor = newTensor(array.slice(i))
  def slice(i: Int, dim: Int): Tensor = newTensor(array.slice(i, dim))

  def squeeze(): Double = {
    require(shape.sizes.forall(_ == 1))
    array.getDouble(0L)
  }
  def squeeze(index: Int*): Double = array.getDouble(index: _*)
  def squeeze(index: Array[Int]): Double = squeeze(index: _*)

  /**
    * returns a view
    */
  def access(index: Long*): Tensor = {
    val ix = index.map(NDArrayIndex.point)
    newTensor(array.get(ix: _*))
  }

  def apply(index: Array[Long]): Tensor = access(index: _*)

  private def handleNegIndex(i: Long, shapeIndex: Int): Long =
    if (i < 0) shape(shapeIndex) + i else i

  /**
    * returns a view
    */
  def apply(ranges: NumscaRange*): Tensor = {

    val indexes: Seq[INDArrayIndex] = ranges.zipWithIndex.map {
      case (nr, i) =>
        nr.to match {
          case None if nr.from == 0 =>
            NDArrayIndex.all()
          case None =>
            NDArrayIndex.interval(handleNegIndex(nr.from, i), shape(i))
          case Some(n) =>
            NDArrayIndex.interval(
              handleNegIndex(nr.from, i),
              handleNegIndex(n, i)
            )
        }
    }
    newTensor(array.get(indexes: _*))
  }

  def apply(selection: Tensor*): TensorSelection = {
    val (indexes, newShape) = selectIndexes(selection)
    TensorSelection(this, indexes, newShape)
  }

  private def selectIndexes(
      selection: Seq[Tensor]
  ): (Array[Array[Long]], Option[Shape]) = {
    if (selection.length == 1) {
      if (rank == 2 && shape.head == 1) {
        (indexByTensor(selection.head), Some(selection.head.shape))
      } else {
        throw new NotImplementedError()
      }
    } else {
      (multiIndex(selection), None)
    }
  }

  private def multiIndex(selection: Seq[Tensor]): Array[Array[Long]] = {
    require(
      selection.forall(s => s.shape.head == 1),
      s"shapes must be [1, n] (was: ${selection.map(_.shape)}"
    )

    // broadcast selection to same shape
    val ts: Seq[INDArray] = Ops.broadcastArrays(selection.map(_.array))

    val rank = ts.head.shape()(1)
    require(
      ts.forall(s => s.shape()(1) == rank),
      s"shapes must be of rank $rank (was ${ts.map(_.shape().toList)}"
    )

    (0 until rank.toInt).map { r =>
      ts.map(s => s.getInt(0, r).toLong).toArray
    }.toArray
  }

  private def indexByTensor(t: Tensor): Array[Array[Long]] = {
    t.array.data().asInt().map(i => Array(0L, i))
  }

  def sameShape(other: Tensor): Boolean = shape == other.shape
  def sameElements(other: Tensor): Boolean = data sameElements other.data

  def rank: Int = array.rank()

  def clip(min: Double, max: Double): Tensor =
    Tensor(data.map { x =>
      if (x < min) min
      else if (x > max)
        max
      else x
    }).reshape(shape)

  def clipNorm(maxNorm: Real): Tensor = {
    val maxNorm2 = maxNorm * maxNorm
    val norm2 = sum(square(this))
    if (norm2 > maxNorm2) {
      val ratio = math.sqrt(maxNorm2 / norm2)
      this * ratio
    } else this
  }

  override def toString: String = array.toString
}

case class Shape(sizes: Vector[Long]) {
  def apply(i: Int): Long = sizes(i)

  def head: Long = sizes.head

  lazy val ints: Vector[Int] = sizes.map(_.toInt)

  override def toString: String = sizes.mkString("Shape(", ",", ")")

  def rank: Int = sizes.length

  def updated(i: Int, value: Long): Shape = Shape(sizes.updated(i, value))

  def elements: Long = sizes.product
}

object Shape {
  def fromArray(a: Array[Long]) = Shape(a.toVector)

  def make(sizes: Long*) = Shape(sizes.toVector)
}

object Tensor {

  private var _floatingDataType: DataType = DataType.DOUBLE
  def floatingDataType: DataType = _floatingDataType
  def floatingDataType_=(dataType: DataType): Unit = {
    Nd4j.setDefaultDataTypes(dataType, dataType)
    _floatingDataType = dataType
  }

  type Size = Long

  def apply(data: Array[Double]): Tensor = {
    val array = Nd4j.create(data)
    new Tensor(array)
  }

  def apply(data: Array[Float]): Tensor = {
    val array = Nd4j.create(data)
    new Tensor(array)
  }

  def apply(array: INDArray) = new Tensor(array)

  def apply(data: Double*): Tensor = Tensor(data.toArray)

}

case class TensorSelection(
    t: Tensor,
    indexes: Array[Array[Long]],
    shape: Option[Shape]
) {

  def asTensor: Tensor = {
    val newData = indexes.map(ix => t.array.getDouble(ix: _*))
    if (shape.isDefined)
      Tensor(newData).reshape(shape.get)
    else
      Tensor(newData)
  }

  def :=(d: Double): Unit = indexes.foreach(t.apply(_) := d)
  def +=(d: Double): Unit = indexes.foreach(t.apply(_) += d)
  def -=(d: Double): Unit = indexes.foreach(t.apply(_) -= d)
  def *=(d: Double): Unit = indexes.foreach(t.apply(_) *= d)
  def /=(d: Double): Unit = indexes.foreach(t.apply(_) /= d)
  def %=(d: Double): Unit = indexes.foreach(t.apply(_) %= d)
  def **=(d: Double): Unit = indexes.foreach(t.apply(_) **= d)

}
