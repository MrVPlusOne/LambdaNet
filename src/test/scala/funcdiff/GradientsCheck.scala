package funcdiff

import botkop.numsca.Tensor
import botkop.{numsca => ns}
import org.nd4j.linalg.api.buffer.{DataBuffer, DataType}
import org.nd4j.linalg.factory.Nd4j
import org.scalactic.{Equality, TolerantNumerics}
import ns._

import scala.concurrent.ExecutionContext
import scala.language.postfixOps
import scala.util.Random

class GradientScalarCheck extends TestUtils {
  implicit val m: GraphMode = ModeTraining

  "Divide" should "throw error when divided by zero" in {
    val x = const(Tensor(6))
    val y = const(Tensor(0))
    TensorExtension.checkNaN = true
    assertThrows[IllegalArgumentException] {
      x / y
    }
  }

  "Backprop" should "calculate correct scalar gradient()ient" in {

    val x = const(Tensor(-2))
    val y = const(Tensor(5))
    val z = const(Tensor(-4))

    val q = x + y

    val f = q * z

    val gradients = CompNode.backprop(f)

    gradients(x).toDouble shouldBe -4
    gradients(y).toDouble shouldBe -4
    gradients(z).toDouble shouldBe 3
    gradients(q).toDouble shouldBe -4
    gradients(f).toDouble shouldBe 1
  }

  it should "do sigmoid backProp" in {
    val w0: CompNode = 2
    val x0: CompNode = -1
    val w1: CompNode = -3
    val x1: CompNode = -2
    val w2: CompNode = -3

    // forward pass
    val dot = w0 * x0 + w1 * x1 + w2

    val out = (1: CompNode) / (exp(-dot) + 1.0)

    val gradients = CompNode.backprop(out)

    implicit val doubleEquality: Equality[Double] =
      TolerantNumerics.tolerantDoubleEquality(0.01)

    assert(gradients(w0).toDouble === -0.2)
    assert(gradients(x0).toDouble === 0.39)
    assert(gradients(w1).toDouble === -0.39)
    assert(gradients(x1).toDouble === -0.59)
    assert(gradients(w2).toDouble === 0.2)
  }

  it should "derive multiplication with a constant" in {
    val x: CompNode = 3
    val y = x * 3
    val gradients = y.backprop
    assert(gradients(x).toDouble == 3)
  }

  it should "derive multiplication with itself" in {
    val x: CompNode = 3
    val y = x * x
    val gradients = y.backprop
    assert(gradients(x).toDouble == 6)
  }

  it should "derive the mean" in {
    val x = const(ns.ones(2, 2))
    val y = x + 2
    val z = y * y * 3
    val out = mean(z)
    val gradients = out.backprop
    assert(ns.arrayEqual(gradients(x).toTensor(), ns.full(x.shape, 4.5)))
  }

  it should "do crazy stuff" in {
    val x = const(ns.ones(3, 1))
    val y = x * 2

    def acc(v: CompNode): CompNode =
      if (ns.sum(v.value) < 100) acc(v * 2) else v

    val z = acc(y)
    val gradients = CompNode.backprop(
      List(z),
      List(DenseGradient(Tensor(0.1, 1.0, 0.0001).reshape(3, 1)))
    )
    assert(
      ns.arrayEqual(
        gradients(x).toTensor(),
        Tensor(6.4, 64, 0.0064).reshape(3, 1)
      )
    )
  }
}

class GradientMatrixTest extends TestUtils {
  Nd4j.setDefaultDataTypes(DataType.DOUBLE, DataType.DOUBLE)
  ns.rand.setSeed(231)
  implicit val m: GraphMode = ModeTraining

  val gruModule = {
    val pc = ParamCollection()
    val lf = LayerFactory(SymbolPath.empty, pc)
    lf.gru('GRU) _
  }

  def gateLike(x: CompNode, y: CompNode): CompNode = {
    val s = x.shape(1) / 2
    val updateGate = x.slice(:>, 0 :> s)
    val restGate = x.slice(:>, s :>)

    val state: CompNode = ns.ones(restGate.shape)

    state * restGate * ((-updateGate) + (1: CompNode))
  }

  def sharedWeights(x: CompNode, y: CompNode): CompNode = {
    (x * y + x) * y + x
  }

  val broadCastingOps = Seq[(CompNode, CompNode) => CompNode](
    _ + _,
    _ - _,
    _ * _,
    _ / _,
    (x1, x2) => x1.concat(x2, axis = 0),
    (x1, x2) => x1.t.concat(x2.t, axis = 1),
    (x1, x2) => concatN(axis = 1)(Vector(x1.t, x2.t, x1.t)),
    (x1, x2) => x1.concat(x2, axis = 0).slice(1 :> 3, 3 :>),
    (x1, x2) => x1.concat(x2, axis = 0).slice(4 :> 5, 3 :>),
    (x1, x2) => x1.concat(x2, axis = 0).slice(1 :> 5, 3 :> 6),
    max,
    (x, y) => plusN(IS(x, y)),
    (x, y) => gruModule(gruModule(x, y), y),
    gateLike,
    sharedWeights
  )

  "Binary broadcasting operators" should "pass numerical gradient checks" in {
    broadCastingOps.zipWithIndex.foreach {
      case (op, i) =>
        val a = const(ns.randn(4, 6))
        val b = const(ns.randn(4, 6))

        nOpGradientCheck(f = {
          case Seq(n1, n2) => op(n1, n2)
        }, IS(a, b), s"Binary case $i A")
    }
  }

  "Dot" should "pass numerical gradient checks" in {
    val a = const(ns.randn(4, 6))
    val b = const(ns.randn(6, 3))

    nOpGradientCheck(f = {
      case Seq(n1, n2) => n1 dot n2
    }, IS(a, b), s"Dot case")
  }

  "Concat[axis=1]" should "pass numerical gradient checks" in {
    val a = const(ns.randn(4, 6))
    val b = const(ns.randn(4, 7))

    nOpGradientCheck(f = {
      case Seq(n1, n2) => n1.concat(n2, axis = 1)
    }, IS(a, b), s"Concat case")
  }

  val unaryTestInputShape = Seq(4L, 6L)

  val unaryOps = {
    val t = ns.softmax(ns.randn(unaryTestInputShape: _*))

    Seq[(String, CompNode => CompNode)](
      "exp" -> exp,
      "negate" -> negate,
      "mean" -> mean,
      "._t" -> (x => x.t),
      "x => x ^ 3.0" -> (x => x ^ 3.0),
      "x => mean(x, 1)" -> (x => mean(x, 1)),
      "x => mean(x, 0)" -> (x => mean(x, 0)),
      "sum" -> sum,
      "x => sum(x,0)" -> (x => sum(x, 0)),
      "x => sum(x,1)" -> (x => sum(x, 1)),
      "relu" -> relu,
      "leakyRelu" -> (x => leakyRelu(x, 0.1)),
      "softmax" -> softmax,
      "x => log(abs(x) + 0.1)" -> (x => log(abs(x) + 0.1)),
      "sigmoid" -> sigmoid,
      "tanh" -> tanh,
      "_.slice(:>, :>)" -> (_.slice(:>, :>)),
      "_.slice(1,1)" -> (_.slice(1, 1)),
      "_.slice(2:>3, 0:>5)" -> (_.slice(2 :> 3, 0 :> 5)),
      "_.slice(1:>2, :>)" -> (_.slice(1 :> 2, :>)),
      "_.slice(1:>3, 1:>).slice(1:>2, 1:>3)" -> (_.slice(1 :> 3, 1 :>)
        .slice(1 :> 2, 1 :> 3)),
      "_.reshape(Shape.make(24))" -> (_.reshape(Shape.make(24))),
      "_.reshape(Shape.make(2,3,4))" -> (_.reshape(Shape.make(2,3,4))),
      "square" -> square,
      "x => sqrt(abs(x) + 0.1)" -> (x => sqrt(abs(x) + 0.1)),
      "x => total(IS(x,x,x,x))" -> (x => plusN(IS(x, x, x, x))),
      "x => concatN(IS(x,x,x,x), axis = 0)" -> (
          x =>
            concatN(axis = 0)(IS(x, x, x, x))
        ),
      "x => concatN(IS(x,x,x,x), axis = 1)" -> (
          x =>
            concatN(axis = 1)(IS(x, x, x, x))
        ),
      "crossEntropyOnSoftmax(x, t)" -> (x => crossEntropyOnSoftmax(x, t))
    )
  }

  "Unary operators" should "pass numerical gradient checks" in {

    unaryOps.foreach {
      case (name, op) =>
        println(s"In $name")
        val a = const(ns.randn(unaryTestInputShape: _*))
        nOpGradientCheck(f = {
          case Seq(x1) => op(x1)
        }, IS(a), name)

    }
  }

  "Softmax with masked logits" should "has the same value" in {
    val x = ns.randn(20, 10)
    val mask = (ns.randn(20, 10) > 0).boolToFloating

    val original = {
      val y = ns.softmax(x) * mask
      y / ns.sumAxis(y, 1)
    }
    val masked = {
      val y = ns.softmax(x * mask) * mask
      y / ns.sumAxis(y, 1)
    }
    logger.debug(s"masked: $masked")
    logger.debug(s"original: $original")
    logger.debug(s"Difference: ${masked - original}")
    assert(relError(masked, original) < 1e-3)
  }

  "Linear layer" should "pass gradient check for its params" in {
    def construct(pc: ParamCollection) = {
      ns.rand.setSeed(1)

      val factory = new LayerFactory(SymbolPath.empty / 'testNet, pc)

      val data = ns.randn(3, 3)
      val w1 = ns.randn(3, 3)
      val b1 = ns.randn(3, 1)
      val y1 = w1 * data + b1

      val y = factory.linear('linear1, nOut = data.shape(1))(data)
      mean(square(y - y1))
    }

    val pc = ParamCollection()
    layerGradCheck(construct, pc, "Linear Layer")
  }

  "GRU applied multiple times" should "pass gradient check for its params" in {
    def construct(collection: ParamCollection) = {
      ns.rand.setSeed(1)

      val nState = 5
      val inputs = (0 until nState).map { i =>
        TensorExtension.oneHot(Seq(i), nState)
      }

      val targets = inputs.scanLeft(ns.zeros(1, nState))(_ + _).tail

      val factory = LayerFactory(SymbolPath.empty, collection)
      val initState = collection.getVar(SymbolPath.empty / 'initState) {
        ns.rand(nState).reshape(1, nState)
      }

      val states = inputs.zip(targets).scanLeft(initState: CompNode) {
        case (s, (input, t)) =>
          factory.gru('TestGRU)(s, input)
      }
      sum(plusN(states.zip(targets).map {
        case (s, t) => square(s - t): CompNode
      }))
    }

    val pc = ParamCollection()
    layerGradCheck(construct, pc, "GRU sum bits")
  }

  "randomUnitVec" should "have unit norm" in {
    for (_ <- 0 until 100) {
      val n = TensorExtension.normL2(TensorExtension.randomUnitVec(10))
      assert(relError(n, Tensor(1.0)) < 1e5, s"norm = $n")
    }
  }

  "accuracy" should "pass examples" in {
    accuracy(Tensor(1, 0, 1, 0, 0, 1).reshape(-1, 2), Seq(0, 0, 1))._1 shouldBe 1.0
    accuracy(Tensor(1, 0, 1, 0, 0, 1).reshape(-1, 2), Seq(1, 1, 0))._1 shouldBe 0.0
    accuracy(Tensor(1, 0, 1, 0, 0, 1, 0, 1).reshape(-1, 2), Seq(0, 0, 0, 0))._1 shouldBe 0.5
  }

  "Parallel version of Backprop" should "be consistent with old version" in {
    import org.scalatest.concurrent.ScalaFutures._

    val x = const(ns.randn(20, 30))
    val y = const(ns.randn(20, 30))

    val z = square(x * x + ((x - y) * 10) * exp(x * 3 + x * y))

    val (dx, dy) = {
      val grads = z.backprop
      grads(x) -> grads(y)
    }
    val (dx1, dy1) = {
      val ctx = ExecutionContext.global
      val grads = z.backpropParallel(ctx).futureValue

      grads(x) -> grads(y)
    }

    assert(relError(dx, dx1) < 1e-4)
    assert(relError(dy, dy1) < 1e-4)
  }

  "Parallel version of Backprop" should "be faster" in {
    import scala.concurrent._
    import duration._

    val random = new Random(1)
    val x = const(ns.randn(50, 50))

    def branch(depth: Int): CompNode = {
      if (depth == 0) {
        if (random.nextDouble() < 0.2) x else const(ns.randn(50, 50))
      } else sigmoid(branch(depth - 1) + branch(depth - 1))
    }

    val z = branch(11)

    val (g1, t1) = SimpleMath.measureTime(z.backprop.apply(x))
    println("T1 = %e".format(t1.toDouble))

    implicit val ctx = ExecutionContext.global
    val (g2, t2) = SimpleMath.measureTime(
      Await.result(z.backpropParallel.map(_.apply(x)), 10 seconds)
    )
    println("T2 = %e".format(t2.toDouble))

    assert(relError(g1, g2) < 1e-4)
    assert(t2 < t1)
  }
}
