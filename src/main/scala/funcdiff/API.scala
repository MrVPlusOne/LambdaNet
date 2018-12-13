package funcdiff

import botkop.numsca
import botkop.numsca._
import DiffFunc._

import scala.language.{implicitConversions, reflectiveCalls}

object API {

  def const(value: Tensor): CompNode = {
    new CompNode(ConstFunc(value))
  }

  def paramNode(value: Tensor, path: SymbolPath): ParamNode = {
    new ParamNode(value, path)
  }

  def funcNode(func: DiffFunc, name: String = "func"): CompNode = {
    new CompNode(func)
  }

  def exp(x1: CompNode): CompNode = funcNode(Exp(x1))

  def sqrt(x1: CompNode): CompNode = funcNode(Sqrt(x1))

  def sigmoid(x1: CompNode): CompNode = funcNode(Sigmoid(x1))

  def tanh(x1: CompNode): CompNode =  funcNode(Tanh(x1))

  def mean(x1: CompNode): CompNode = funcNode(Mean(x1))

  def mean(x1: CompNode, axis: Int): CompNode = funcNode(MeanByAxis(x1, axis))

  def square(x1: CompNode): CompNode = x1 * x1

  def negate(x1: CompNode): CompNode = funcNode(Negate(x1))

  def relu(x1: CompNode): CompNode = funcNode(Threshold(x1, 0))

  def leakyRelu(x1: CompNode, slope: Double = 0.01): CompNode = funcNode(LeakyRelu(x1, slope))

  def sum(x1: CompNode): CompNode = funcNode(Sum(x1))

  def sum(x1: CompNode, axis: Int): CompNode = funcNode(SumByAxis(x1, axis))

  def softmax(x1: CompNode): CompNode = funcNode(Softmax(x1))

  def log(x1: CompNode): CompNode = funcNode(Log(x1))

  def abs(x1: CompNode): CompNode = funcNode(Abs(x1))

  def max(x1: CompNode, x2: CompNode): CompNode = funcNode(MaxBinary(x1, x2))

  def total(xs: IS[CompNode]): CompNode = funcNode(Total(xs))

  def concatN(xs: IS[CompNode], axis: Int): CompNode = funcNode(ConcatN(xs, axis))

  def crossEntropy(prediction: CompNode, targets: Tensor): CompNode = -sum(log(prediction + 1e-7) * targets, axis = 1)

  def crossEntropyOnSoftmax(logits: CompNode, targets: Tensor): CompNode = funcNode(CrossEntropyOnSoftmax(logits, targets))

  def crossEntropyOnSoftmaxIneff(logits: CompNode, targets: Tensor): CompNode = -sum(log(softmax(logits) + 1e-7) * targets, axis = 1)

  def correctWrongSets(probabilities: Tensor, targets: Seq[Int], predictionGroupSize: Int): (Set[Int], Set[Int]) = {
    require(probabilities.shape(0) / predictionGroupSize == targets.length)

    var correct = Set[Int]()
    var wrong = Set[Int]()

    for(i <- targets.indices) {
      val groupSum = numsca.sum(probabilities(i * predictionGroupSize :> (i + 1) * predictionGroupSize, :>), axis = 0)
      val prediction = TensorExtension.argmax(groupSum, axis = 1).squeeze()
      if (prediction == targets(i)) {
        correct += i
      } else {
        wrong += i
      }
    }

    (correct, wrong)
  }

  def accuracy(logits: Tensor, targets: Seq[Int], predictionGroupSize: Int = 1)
      :(Double, (Set[Int], Set[Int])) = {
    val effectiveTargets = targets.grouped(predictionGroupSize).map{ group =>
      assert(group.forall(p => p == group.head))
      group.head
    }.toSeq

    val (correct, wrong) = correctWrongSets(numsca.softmax(logits), effectiveTargets, predictionGroupSize)
    (correct.size.toDouble / (correct.size + wrong.size), (correct, wrong))
  }

  implicit def doubleToNode(d: Double): CompNode = const(Tensor(d))

  implicit def intToNode(i: Int): CompNode = const(Tensor(i))

//  implicit def paramToNode(p: Param): CompNode = const(p.currentValue, name = p.path.toString, param = Some(p))

  implicit def tensorToNode(tensor: Tensor): CompNode = const(tensor)

  implicit class ExtendedFunctions(x1: CompNode){

    def unary_- : CompNode = funcNode(Negate(x1))

    def t: CompNode = funcNode(Transpose(x1))

    def / (x2: CompNode): CompNode = funcNode(Divide(x1,x2))

    def + (x2: CompNode): CompNode = funcNode(Plus(x1,x2))

    def - (x2: CompNode): CompNode = funcNode(Minus(x1, x2))

    def * (x2: CompNode): CompNode = funcNode(Times(x1,x2))

    def ^ (p: Double): CompNode = funcNode(PowerConst(x1, p))

    def concat(x2: CompNode, axis: Int): CompNode = funcNode(Concat(x1, x2, axis))

    def slice(ranges: NumscaRange*): CompNode = funcNode(Slice(x1, ranges))

    def dot(x2: CompNode): CompNode = funcNode(Dot(x1,x2))

    def ~> [B <: CompNode](f: CompNode => B): B = f(x1)

    def repeat(number: Int, axis: Int): CompNode = {
      if(number == 1) x1
      else concatN(Vector.fill(number)(x1), axis)
    }
  }

  /** convenient import for NumscaRange */
  def :>(end: Int) = NumscaRange(0, Some(end))
  /** convenient import for NumscaRange */
  def :> = NumscaRange(0, None)

  /** convenient import for NumscaRange */
  implicit class NumscaInt(i: Int) {
    def :>(end: Int) = NumscaRange(i, Some(end))
    def :> = NumscaRange(i, None)
  }

  def printShape(withShape: {def shape: Array[Int]}, name: String): Unit = {
    val shapeString = TensorExtension.showShape(withShape.shape)
    println(s"$name shape: $shapeString")
  }

  def showShape(shape: Array[Int]): String = {
    TensorExtension.showShape(shape)
  }
}
