package funcdiff

import botkop.numsca
import botkop.numsca.{NumscaRange, Shape, Tensor}
import botkop.{numsca => ns}
import org.nd4j.linalg.api.ops.impl.indexaccum.{IMax, IMin}
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.ops.transforms.Transforms
import Tensor.Size
import org.nd4j.linalg.indexing.BooleanIndexing
import org.nd4j.linalg.indexing.conditions.IsNaN

object TensorExtension {

  def mamFormat(d: Double): String = {
    d.toString.replace("E", "*10^")
  }

  def mamFormat(t: Tensor): String = {
    if (t.elements == 1) mamFormat(t.squeeze())
    else {
      t.dataSlow
        .map(mamFormat)
        .mkString("{", ",", "}") // fixme: support high-dimensional tensors
    }
  }

  /** set this to true to turn on NaN checking */
  var checkNaN = true
  val zeroTolerance = 1e-14
  val halfZeroTolerance = 1e-7
  val epsilon = 1e-12

  // ==== basic operations ====
  /** calculates which axes have been broadcasted */
  def broadcastAxes(oldShape: Shape, newShape: Shape): Seq[Int] = {
    val s1 = oldShape.sizes
    val s2 = newShape.sizes
    for (i <- s1.indices;
         l1 = s1(i);
         l2 = s2(i) if l1 != l2) yield {
      assert(l1 == 1, s"oldShape along axis $i is invalid! l1 = $l1, l2 = $l2")
      i
    }
  }

  /** calculates which axes are broadcasted when these two shapes are broadcasted to make equal */
  def broadcastAxesWhenMerge(
      shape1: Shape,
      shape2: Shape
  ): (Seq[Int], Seq[Int]) = {
    require(shape1.rank == shape2.rank, s"shape 1: $shape1, shape 2: $shape2")
    var axes1, axes2 = List[Int]()
    for (i <- 0 until shape1.rank;
         l1 = shape1(i);
         l2 = shape2(i)) yield {
      if (l1 < l2) {
        assert(
          l1 == 1,
          s"shapes along axis $i are not compatible! l1 = $l1, l2 = $l2"
        )
        axes1 = i :: axes1
      } else if (l1 > l2) {
        assert(
          l2 == 1,
          s"shapes along axis $i are not compatible! l1 = $l1, l2 = $l2"
        )
        axes2 = i :: axes2
      }
    }
    (axes1, axes2)
  }

  def shapeConsistentWithRanges(
      shape: Shape,
      ranges: Seq[NumscaRange]
  ): Boolean = {
    shape.ints.zip(ranges).forall {
      case (s, range) =>
        range.to match {
          case Some(to) =>
            (to - range.from) == s
          case None => true
        }
    }
  }

  def rangesToShape(totalShape: Shape, ranges: Seq[NumscaRange]): Shape = {
    Shape(ranges.zipWithIndex.map {
      case (nr, i) =>
        val to = nr.to.getOrElse(totalShape(i))
        to - nr.from
    }.toVector)
  }

  implicit class TensorWrapper(data: Tensor) {
    def unbroadcast(oldShape: Shape): Tensor = {
      if (oldShape.elements == 1) Tensor(ns.sum(data)).reshape(oldShape)
      else {
        val axes = broadcastAxes(oldShape, data.shape)
        sumAlongAxes(axes)
      }
    }

    def sumAlongAxes(axes: Seq[Int]): Tensor = {
      axes.foldLeft(data) { case (t, axis) => ns.sumAxis(t, axis) }
    }

    def broadcast(newShape: Shape): Tensor = {
      new Tensor(data.array.broadcast(newShape.sizes: _*))
    }

    def splitAlongAxis(axis: Int, splitAt: Long): (Tensor, Tensor) = {
      import ns._

      require(axis >= 0 && axis < data.shape.rank)
      require(
        splitAt > 0 && splitAt < data.shape(axis),
        s"split at $splitAt along axis $axis would create empty matrix from $data"
      )
      val left = data(data.shape.sizes.indices.map { dim =>
        if (dim == axis) 0 :> splitAt
        else :>
      }: _*)

      val right = data(data.shape.sizes.indices.map { dim =>
        if (dim == axis) splitAt :> data.shape(dim).toInt
        else :>
      }: _*)

      (left, right)
    }

    def requireNonZero(tolerance: Double = zeroTolerance): Unit = {
      if (checkNaN) {
        val minV = ns.min(ns.abs(data)).squeeze()
        require(
          minV >= tolerance,
          s"Tensor contains zero element: min abs = $minV"
        )
      }
    }

    def requirePositive(
        info: String = "",
        tolerance: Double = zeroTolerance
    ): Unit = {
      if (checkNaN) {
        require(
          (data > tolerance).forall,
          s"Tensor contains negative element: $data. $info"
        )
      }
    }

    def hasNaN: Boolean =
      BooleanIndexing.or(
        data.value.array,
        new IsNaN()
      )
  }

  def oneHot(labels: Seq[Int], categories: Int): Tensor = {
    val t = numsca.zeros(labels.length, categories)
    labels.zipWithIndex.foreach {
      case (l, i) =>
        t(i, l) := 1
    }

    t
  }

  def normL2(tensor: Tensor, sumAlongDim: Int = -1): Tensor = {
    numsca.sqrt(numsca.sumAxis(numsca.square(tensor), sumAlongDim))
  }

  /** Generates a random point on the surface of an N-dimensional sphere.
    * @param resultDim the dimension N */
  def randomUnitVec(resultDim: Size): Tensor = {
    val x = numsca.randn(resultDim)
    val n = normL2(x, sumAlongDim = 0)
    if (n.squeeze() < TensorExtension.zeroTolerance) {
      randomUnitVec(resultDim) // try again
    } else {
      x / n
    }
  }

  def sin(t: Tensor): Tensor = new Tensor(Transforms.sin(t.array))
  def cos(t: Tensor): Tensor = new Tensor(Transforms.cos(t.array))

  def matrix(rows: Seq[Array[Float]]): Tensor = {
    val ndArray = Nd4j.create(rows.toArray)
    new Tensor(ndArray)
  }

  def tensor(data: Array[Double], shape: Array[Int]): Tensor = {
    new Tensor(Nd4j.create(data, shape))
  }

  //fixme: use more efficient implementation
  def conv2D(x: Tensor, kernel: Tensor): Tensor = {
    import botkop.numsca._

    val Vector(h, w) = x.shape.ints
    val Vector(kh, kw) = kernel.shape.ints
    val rh = h - kh + 1
    val rw = w - kw + 1
    val data = new Array[Double](rh * rw)
    for {
      r <- 0 until rh
      c <- 0 until rw
      region = x(r :> r + kh, c :> c + kw)
    } {
      data(r * rw + c) = numsca.sum(region * kernel)
    }
    new Tensor(Nd4j.create(data, Array(rh, rw), 'c'))
  }

  implicit class RangeWrapper(range: NumscaRange) {
    def contains(index: Int): Boolean = {
      index >= range.from && (range.to match {
        case Some(t) => index < t
        case None    => true
      })
    }

    def offset(amount: Long): NumscaRange = {
      NumscaRange(range.from + amount, range.to.map(_ + amount))
    }

    def prettyPrint: String = {
      s"${range.from}:>${range.to.getOrElse("")}"
    }
  }

  def showRanges(ranges: Seq[NumscaRange]): String = {
    ranges.map(_.prettyPrint).mkString("[", ",", "]")
  }
}
