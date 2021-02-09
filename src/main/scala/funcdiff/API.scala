package funcdiff

import botkop.numsca
import botkop.numsca._
import DiffFunc._

import scala.language.{implicitConversions, reflectiveCalls}
import TensorExtension.epsilon

trait APITrait {

  var debugOpTime = true

  implicit def symbol2Path(symbol: Symbol): SymbolPath =
    SymbolPath(symbol)

  def const(value: Tensor): CompNode = {
    new CompNode(ConstFunc(value))
  }

  def paramNode(value: Tensor, path: SymbolPath): ParamNode = {
    new ParamNode(value, path)
  }

  def funcNode(func: => DiffFunc): CompNode = {
    if (debugOpTime) {
      DebugTime.logTime(func.shortName) { new CompNode(func) }
    } else {
      new CompNode(func)
    }
  }

  def exp(x1: CompNode): CompNode = funcNode(Exp(x1))

  def sqrt(x1: CompNode): CompNode = funcNode(Sqrt(x1))

  def sigmoid(x1: CompNode): CompNode = funcNode(Sigmoid(x1))

  def softPlus(x1: CompNode): CompNode = log(exp(x1) + 1.0)

  def tanh(x1: CompNode): CompNode = funcNode(Tanh(x1))

  def mean(x1: CompNode): CompNode = funcNode(Mean(x1))

  def mean(x1: CompNode, axis: Int): CompNode = funcNode(MeanByAxis(x1, axis))

  def square(x1: CompNode): CompNode = x1 * x1

  def negate(x1: CompNode): CompNode = funcNode(Negate(x1))

  def relu(x1: CompNode): CompNode = funcNode(Threshold(x1, 0))

  def leakyRelu(x1: CompNode, slope: Double = 0.01): CompNode =
    relu(x1) - relu(-x1) * slope
//    funcNode(LeakyRelu(x1, slope))

  def sum(x1: CompNode): CompNode = funcNode(Sum(x1))

  def sum(x1: CompNode, axis: Int, keepDim: Boolean = true): CompNode =
    funcNode(SumByAxis(x1, axis, keepDim))

  def softmax(x1: CompNode): CompNode = funcNode(Softmax(x1))

  def log(x1: CompNode): CompNode = funcNode(Log(x1))

  def abs(x1: CompNode): CompNode = funcNode(Abs(x1))

  def max(x1: CompNode, x2: CompNode): CompNode = funcNode(MaxBinary(x1, x2))

  def plusN(xs: IS[CompNode]): CompNode = funcNode(PlusN(xs))

  def meanN(xs: IS[CompNode]): CompNode = plusN(xs) / xs.length

  def totalSafe(xs: IS[CompNode], whenEmpty: => CompNode) =
    if (xs.isEmpty) whenEmpty else plusN(xs)

  def concatN(axis: Int, fromRows: Boolean = false)(
      xs: IS[CompNode]
  ): CompNode = {
    if (xs.length == 1) xs.head
    else funcNode(ConcatN(xs, axis, fromRows))
  }

  def stackRows(xs: IS[CompNode]) = funcNode(StackRows(xs))

  def concatTupledRows(rows: IS[(CompNode, CompNode)]): CompNode = {
    val (l, r) = rows.unzip
    stackRows(l).concat(stackRows(r), axis = 1)
  }

  def crossEntropy(prediction: CompNode, targets: Tensor): CompNode =
    -sum(log(prediction + epsilon) * targets, axis = 1)

  def crossEntropyOnSoftmax(logits: CompNode, targets: Tensor): CompNode =
    funcNode(CrossEntropyOnSoftmax(logits, targets))

  def crossEntropyOnSigmoid(logits: CompNode, targets: Tensor): CompNode = {
    funcNode(CrossEntropyOnSigmoid(logits, targets))
  }

  def crossEntropyOnSoftmaxIneff(logits: CompNode, targets: Tensor): CompNode =
    -sum(log(softmax(logits) + epsilon) * targets, axis = 1)

  def normSquared(x1: CompNode): CompNode =
    sum(square(x1), axis = 1)

  def cosineSimilarity(
      x1: CompNode,
      x2: CompNode
  ): CompNode = {
    x1.dot(x2.t) / sqrt(normSquared(x1).dot(normSquared(x2).t) + epsilon)
  }

  def correctWrongSets(
      probabilities: Tensor,
      targets: Seq[Int],
      predictionGroupSize: Int
  ): (Set[Int], Set[Int]) = {
    require(probabilities.shape(0) / predictionGroupSize == targets.length)

    var correct = Set[Int]()
    var wrong = Set[Int]()

    for (i <- targets.indices) {
      val groupSum = numsca.sumAxis(
        probabilities(
          i * predictionGroupSize :> (i + 1) * predictionGroupSize,
          :>
        ),
        axis = 0
      )
      val prediction = argmaxAxis(groupSum, axis = 1).squeeze()
      if (prediction == targets(i)) {
        correct += i
      } else {
        wrong += i
      }
    }

    (correct, wrong)
  }

  def accuracy(
      logits: Tensor,
      targets: Seq[Int],
      predictionGroupSize: Int = 1
  ): (Double, (Set[Int], Set[Int])) = {
    val effectiveTargets = targets
      .grouped(predictionGroupSize)
      .map { group =>
        assert(group.forall(p => p == group.head))
        group.head
      }
      .toSeq

    val (correct, wrong) =
      correctWrongSets(
        numsca.softmax(logits),
        effectiveTargets,
        predictionGroupSize
      )
    (correct.size.toDouble / (correct.size + wrong.size), (correct, wrong))
  }

  implicit def doubleToNode(d: Double): CompNode = const(Tensor(d))

  implicit def intToNode(i: Int): CompNode = const(Tensor(i))

//  implicit def paramToNode(p: Param): CompNode = const(p.currentValue, name = p.path.toString, param = Some(p))

  implicit def tensorToNode(tensor: Tensor): CompNode = const(tensor)

  implicit class ExtendedFunctions(x1: CompNode) {

    def unary_- : CompNode = funcNode(Negate(x1))

    def t: CompNode = funcNode(Transpose(x1))

    def /(x2: CompNode): CompNode = funcNode(Divide(x1, x2))

    def +(x2: CompNode): CompNode = funcNode(Plus(x1, x2))

    def -(x2: CompNode): CompNode = this + (-x2)

    def *(x2: CompNode): CompNode = funcNode(Times(x1, x2))

    def ^(p: Double): CompNode = funcNode(PowerConst(x1, p))

    def concat(x2: CompNode, axis: Int): CompNode =
      funcNode(Concat(x1, x2, axis))

    def slice(ranges: NumscaRange*): CompNode = funcNode(Slice(x1, ranges))

    def dot(x2: CompNode): CompNode = funcNode(Dot(x1, x2))

    def ~>[B <: CompNode](f: CompNode => B): B = f(x1)

    def rows: Vector[CompNode] = {
      //todo: use more efficient implementation
      val numOfRows = x1.shape(0)
      (0 until numOfRows.toInt).map { r =>
        x1.slice(r, :>)
      }.toVector
    }
  }

//  /** convenient import for NumscaRange */
//  def :>(end: Int) = NumscaRange(0, Some(end))
//
//  /** convenient import for NumscaRange */
//  def :> = NumscaRange(0, None)
//
//  /** convenient import for NumscaRange */
//  implicit class NumscaInt(i: Long) {
//    def :>(end: Long) = NumscaRange(i, Some(end))
//    def :> = NumscaRange(i, None)
//  }

  def printShape(withShape: { def shape: Shape }, name: String): Unit = {
    println(s"$name shape: ${withShape.shape}")
  }
}
