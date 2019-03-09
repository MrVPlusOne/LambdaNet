package botkop.numsca

import org.nd4j.linalg.api.iter.NdIndexIterator
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.indexing.{INDArrayIndex, NDArrayIndex}
import org.nd4j.linalg.ops.transforms.Transforms

import scala.collection.JavaConverters._
import scala.language.{implicitConversions, postfixOps}

class Tensor(val array: INDArray, val isBoolean: Boolean = false)
    extends Serializable {

  def data: Array[Double] = array.dup.data.asDouble

  def copy(): Tensor = new Tensor(array.dup())

  val shape: Shape = Shape.fromArray(array.shape())
  def reshape(newShape: Shape) = new Tensor(array.reshape(newShape.sizes: _*))
  def reshape(newShape: Int*) = new Tensor(array.reshape(newShape.map(_.toLong): _*))
  def shapeLike(t: Tensor): Tensor = reshape(t.shape)

  def transpose() = new Tensor(array.transpose())
  def T: Tensor = transpose()
  def transpose(axes: Array[Int]): Tensor = {
    require(axes.sorted sameElements shape.sizes.indices, "invalid axes")
    val newShape = Shape(axes.map(a => shape(a)).toVector)
    reshape(newShape)
  }
  def transpose(axes: Int*): Tensor = transpose(axes.toArray)

  def round: Tensor =
    Tensor(data.map(math.round(_).toDouble)).reshape(this.shape)

  def dot(other: Tensor) = new Tensor(array mmul other.array)

  def unary_- : Tensor = new Tensor(array mul -1)
  def +(d: Double): Tensor = new Tensor(array add d)
  def -(d: Double): Tensor = new Tensor(array sub d)
  def *(d: Double): Tensor = new Tensor(array mul d)
  def **(d: Double): Tensor = power(this, d)
  def /(d: Double): Tensor = new Tensor(array div d)
  def %(d: Double): Tensor = new Tensor(array fmod d)

  def +=(d: Double): Unit = array addi d
  def -=(d: Double): Unit = array subi d
  def *=(d: Double): Unit = array muli d
  def **=(d: Double): Unit = array.assign(Transforms.pow(array, d))
  def /=(d: Double): Unit = array divi d
  def %=(d: Double): Unit = array fmodi d

  def >(d: Double): Tensor = new Tensor(array gt d, true)
  def >=(d: Double): Tensor = new Tensor(array gte d, true)
  def <(d: Double): Tensor = new Tensor(array lt d, true)
  def <=(d: Double): Tensor = new Tensor(array lte d, true)
  def ==(d: Double): Tensor = new Tensor(array eq d, true)
  def !=(d: Double): Tensor = new Tensor(array neq d, true)

  def +(other: Tensor): Tensor = Ops.add(this, other)
  def -(other: Tensor): Tensor = Ops.sub(this, other)
  def *(other: Tensor): Tensor = Ops.mul(this, other)
  // def **(other: Tensor): Tensor = Ops.pow(this, other)
  def /(other: Tensor): Tensor = Ops.div(this, other)
  def %(other: Tensor): Tensor = Ops.mod(this, other)
  def >(other: Tensor): Tensor = Ops.gt(this, other)
  def >=(other: Tensor): Tensor = Ops.gte(this, other)
  def <(other: Tensor): Tensor = Ops.lt(this, other)
  def <=(other: Tensor): Tensor = Ops.lte(this, other)
  def ==(other: Tensor): Tensor = Ops.eq(this, other)
  def !=(other: Tensor): Tensor = Ops.neq(this, other)

  def &&(other: Tensor): Tensor = {
    require(this.isBoolean && other.isBoolean)
    new Tensor(this.array.mul(other.array), true)
  }

  def ||(other: Tensor): Tensor = {
    require(this.isBoolean && other.isBoolean)
    new Tensor(Transforms.max(this.array.add(other.array), 1.0), true)
  }

  /**
    * broadcast argument tensor with shape of this tensor
    */
  private def bc(t: Tensor): INDArray = t.array.broadcast(shape.sizes: _*)

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

  def ravel(): Tensor = new Tensor(array.ravel())
  def ravel(c: Char): Tensor = new Tensor(array.ravel(c))

  def slice(i: Int): Tensor = new Tensor(array.slice(i))
  def slice(i: Int, dim: Int): Tensor = new Tensor(array.slice(i, dim))

  def squeeze(): Double = {
    require(shape.sizes.forall(_ == 1))
    array.getDouble(0L)
  }
  def squeeze(index: Int*): Double = array.getDouble(index: _*)
  def squeeze(index: Array[Int]): Double = squeeze(index: _*)

  /**
    * returns a view
    */
  def apply(index: Long*): Tensor = {
    val ix = index.map(NDArrayIndex.point)
    new Tensor(array.get(ix: _*))
  }

  def apply(indices: Array[Long]): Tensor = this.apply(indices:_*)

  private def handleNegIndex(i: Long, shapeIndex: Int): Long =
    if (i < 0) shape(shapeIndex) + i else i

  /**
    * returns a view
    */
  def apply(ranges: NumscaRange*)(implicit dummy: Int = 0): Tensor = {

    val indexes: Seq[INDArrayIndex] = ranges.zipWithIndex.map {
      case (nr, i) =>
        nr.to match {
          case None if nr.from == 0 =>
            NDArrayIndex.all()
          case None =>
            NDArrayIndex.interval(handleNegIndex(nr.from, i), shape(i))
          case Some(n) =>
            NDArrayIndex.interval(handleNegIndex(nr.from, i),
                                  handleNegIndex(n, i))
        }
    }
    new Tensor(array.get(indexes: _*))
  }

  def apply(selection: Tensor*): TensorSelection = {
    val (indexes, newShape) = selectIndexes(selection)
    TensorSelection(this, indexes, newShape)
  }

  private def selectIndexes(
      selection: Seq[Tensor]): (Array[Array[Long]], Option[Shape]) = {
    if (selection.length == 1) {
      if (selection.head.isBoolean) {
        (indexByBooleanTensor(selection.head), None)
      } else if (rank == 2 && shape.head == 1) {
        (indexByTensor(selection.head), Some(selection.head.shape))
      } else {
        throw new NotImplementedError()
      }
    } else {
      (multiIndex(selection), None)
    }
  }

  private def multiIndex(selection: Seq[Tensor]): Array[Array[Long]] = {
    require(selection.forall(s => s.shape.head == 1),
            s"shapes must be [1, n] (was: ${selection.map(_.shape)}")

    // broadcast selection to same shape
    val ts: Seq[INDArray] = Ops.tbc(selection: _*)

    val rank = ts.head.shape()(1)
    require(ts.forall(s => s.shape()(1) == rank),
            s"shapes must be of rank $rank (was ${ts.map(_.shape().toList)}")

    (0 until rank.toInt).map { r =>
      ts.map(s => s.getInt(0, r).toLong).toArray
    }.toArray
  }

  private def indexByBooleanTensor(t: Tensor): Array[Array[Long]] = {
    require(t.isBoolean)
    require(t sameShape this)

    new NdIndexIterator(t.shape.sizes: _*).asScala.collect {
      case ii: Array[Long] if t.array.getDouble(ii: _*) != 0 => ii
    } toArray
  }

  private def indexByTensor(t: Tensor): Array[Array[Long]] = {
    t.array.data().asInt().map(i => Array(0L, i))
  }

  def sameShape(other: Tensor): Boolean = shape == other.shape
  def sameElements(other: Tensor): Boolean = data sameElements other.data

  def rank: Int = array.rank()

  def clip(min: Double, max: Double): Tensor =
    Tensor(array.data().asDouble().map { x =>
      if (x < min) min
      else if (x > max)
        max
      else x
    }).reshape(shape)

  override def toString: String = array.toString
}

case class Shape(sizes: Vector[Long]){
  def apply(i: Int): Long = sizes(i)

  def head: Long = sizes.head

  lazy val ints: Vector[Int] = sizes.map(_.toInt)

  override def toString: String = sizes.mkString("Shape(", ",", ")")

  def length: Int = sizes.length

  def updated(i: Int, value: Long): Shape = Shape(sizes.updated(i,value))

  def product: Long = sizes.product
}

object Shape{
  def fromArray(a: Array[Long]) = Shape(a.toVector)

  def make(sizes: Long*) = Shape(sizes.toVector)
}

object Tensor {

  type Size = Long

  def apply(data: Array[Double]): Tensor = {
    val array = Nd4j.create(data)
    new Tensor(array)
  }

  def apply(data: Array[Float]): Tensor = {
    val array = Nd4j.create(data)
    new Tensor(array)
  }

  def apply(data: Double*): Tensor = Tensor(data.toArray)

}

case class TensorSelection(t: Tensor,
                           indexes: Array[Array[Long]],
                           shape: Option[Shape]) {

  def asTensor: Tensor = {
    val newData = indexes.map(ix => t.array.getDouble(ix: _*))
    if (shape.isDefined)
      Tensor(newData).reshape(shape.get)
    else
      Tensor(newData)
  }

  def :=(d: Double): Unit = indexes.foreach(t(_) := d)
  def +=(d: Double): Unit = indexes.foreach(t(_) += d)
  def -=(d: Double): Unit = indexes.foreach(t(_) -= d)
  def *=(d: Double): Unit = indexes.foreach(t(_) *= d)
  def /=(d: Double): Unit = indexes.foreach(t(_) /= d)
  def %=(d: Double): Unit = indexes.foreach(t(_) %= d)
  def **=(d: Double): Unit = indexes.foreach(t(_) **= d)

}
