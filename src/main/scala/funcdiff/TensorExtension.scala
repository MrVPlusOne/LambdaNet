package funcdiff

import botkop.numsca
import botkop.numsca.{NumscaRange, Tensor}
import botkop.{numsca => ns}
import org.nd4j.linalg.api.ops.impl.indexaccum.{IMax, IMin}
import org.nd4j.linalg.factory.Nd4j


object TensorExtension {

  /** set this to true to turn on NaN checking */
  var checkNaN = true
  var zeroTolerance = 1e-8

  // ==== basic operations ====
  /** fix the implementation in numsca */
  def argmax(t: Tensor, axis: Int): Tensor =
    new Tensor(Nd4j.getExecutioner.exec(new IMax(t.array), axis))

  /** fix the implementation in numsca */
  def argmin(t: Tensor, axis: Int): Tensor =
    new Tensor(Nd4j.getExecutioner.exec(new IMin(t.array), axis))


  /** calculates which axes have been broadcasted */
  def broadcastAxes(oldShape: Array[Int], newShape: Array[Int]): Seq[Int] = {
    for(i <- oldShape.indices;
        l1 = oldShape(i);
        l2 = newShape(i) if l1 != l2) yield {
      assert(l1 == 1, s"oldShape along axis $i is invalid! l1 = $l1, l2 = $l2")
      i
    }
  }

  /** calculates which axes are broadcasted when these two shapes are broadcasted to make equal */
  def broadcastAxesWhenMerge(shape1: Array[Int], shape2: Array[Int]): (Seq[Int], Seq[Int]) = {
    require(shape1.length == shape2.length)
    var axes1, axes2 = List[Int]()
    for(i <- shape1.indices;
        l1 = shape1(i);
        l2 = shape2(i)) yield {
      if(l1 < l2){
        assert(l1 == 1, s"shapes along axis $i are not compatible! l1 = $l1, l2 = $l2")
        axes1 = i :: axes1
      }else if (l1 > l2){
        assert(l2 == 1, s"shapes along axis $i are not compatible! l1 = $l1, l2 = $l2")
        axes2 = i :: axes2
      }
    }
    (axes1, axes2)
  }

  def shapeConsistentWithRanges(shape: Array[Int], ranges: Seq[NumscaRange]): Boolean = {
    shape.zip(ranges).forall{ case (s, range) =>
      range.to match {
        case Some(to) =>
          (to - range.from) == s
        case None => true
      }
    }
  }

  def rangesToShape(totalShape: Array[Int], ranges: Seq[NumscaRange]): Array[Int] = {
    ranges.zipWithIndex.map{ case(nr, i) =>
      val to = nr.to.getOrElse(totalShape(i))
      to - nr.from
    }.toArray
  }

  implicit class TensorWrapper(data: Tensor){
    def unbroadcast(oldShape: Array[Int]): Tensor = {
      val axes = broadcastAxes(oldShape, data.shape)
      sumAlongAxes(axes)
    }

    def sumAlongAxes(axes: Seq[Int]): Tensor = {
      axes.foldLeft(data) { case (t, axis) => ns.sum(t, axis)}
    }

    def broadcast(newShape: Array[Int]): Tensor = {
      new Tensor(data.array.broadcast(newShape :_*))
    }

    def splitAlongAxis(axis: Int, splitAt: Int): (Tensor, Tensor) = {
      import ns._

      require(axis >= 0 && axis < data.shape.length)
      require(splitAt > 0 && splitAt < data.shape(axis), s"split at $splitAt along axis $axis would create empty matrix from $data")
      val left = data(data.shape.indices.map{ dim =>
        if(dim == axis) 0 :> splitAt
        else :>
      } :_*)

      val right = data(data.shape.indices.map{ dim =>
        if(dim == axis) splitAt :> data.shape(dim)
        else :>
      } :_*)

      (left, right)
    }

    def requireNonZero(tolerance: Double = zeroTolerance): Unit = {
      if(checkNaN) {
        require(data.data.forall(x => math.abs(x) >= tolerance), s"Tensor contains zero element: $data")
      }
    }

    def requirePositive(info: String = "", tolerance: Double = zeroTolerance): Unit = {
      if(checkNaN) {
        require(ns.sum(data > tolerance) == data.shape.product, s"Tensor contains negative element: $data. $info")
      }
    }

    def assertNoNaN(): Unit ={
      if(checkNaN) {
        assert(data.data.forall(!_.isNaN))
      }
    }
  }

  def oneHot(labels: Seq[Int], categories: Int): Tensor = {
    val t = numsca.zeros(labels.length, categories)
    labels.zipWithIndex.foreach{ case (l, i) =>
      t(i, l) := 1
    }

    t
  }

  def normL2(tensor: Tensor, sumAlongDim: Int = -1): Tensor = {
    numsca.sqrt(numsca.sum(numsca.square(tensor), sumAlongDim))
  }

  /** Generates a random point on the surface of an N-dimensional sphere.
    * @param resultDim the dimension N */
  def randomUnitVec(resultDim: Int): Tensor = {
    val x = numsca.randn(resultDim)
    val n = normL2(x)
    if(n.squeeze() < TensorExtension.zeroTolerance){
      randomUnitVec(resultDim)  // try again
    }else {
      x / n
    }
  }

  def matrix(rows: Seq[Array[Float]]): Tensor = {
    val ndArray = Nd4j.create(rows.toArray)
    new Tensor(ndArray)
  }

  def tensor(data: Array[Double], shape: Array[Int]): Tensor ={
    new Tensor(Nd4j.create(data, shape))
  }

  //fixme: use more efficient implementation
  def conv2D(x: Tensor, kernel: Tensor): Tensor = {
    import botkop.numsca._

    val Array(h, w) = x.shape
    val Array(kh, kw) = kernel.shape
    val rh = h - kh + 1
    val rw = w - kw + 1
    val data = new Array[Double](rh*rw)
    for(
      r <- 0 until rh;
      c <- 0 until rw;
      region = x(r :> r+kh, c :> c+kw)
    ) {
      data(r*rw + c) = sum(region * kernel)
    }
    new Tensor(Nd4j.create(data, Array(rh, rw), 'c'))
  }

  implicit class RangeWrapper(range: NumscaRange){
    def contains(index: Int): Boolean = {
      index >= range.from && (range.to match {
        case Some(t) => index < t
        case None => true
      })
    }

    def offset(amount: Int): NumscaRange = {
      NumscaRange(range.from + amount, range.to.map(_ + amount))
    }

    def prettyPrint: String = {
      s"${range.from}:>${range.to.getOrElse("")}"
    }
  }

  def showRanges(ranges: Seq[NumscaRange]): String = {
    ranges.map(_.prettyPrint).mkString("[",",","]")
  }

  def showShape(shape: Array[Int]): String = {
    shape.mkString("[",",","]")
  }
}
