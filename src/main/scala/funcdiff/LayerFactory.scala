package funcdiff

import botkop.numsca.Tensor
import botkop.{numsca => ns}
import botkop.numsca.Tensor.{Size}
import ns._
import funcdiff.LayerFactory.WeightsInitializer
import funcdiff.ParameterAttribute.NeedRegularization
import scala.language.postfixOps

object LayerFactory {
  type WeightsInitializer = (Size, Size) => Tensor

  def xavier(nIn: Size, nOut: Size): Tensor = {
    ns.randn(nIn, nOut) * math.sqrt(2.0 / (nIn + nOut))
  }
}

@SerialVersionUID(0)
case class LayerFactory(
    nameSpace: SymbolPath,
    paramCollection: ParamCollection
) {

  def getVar(
      relativePath: SymbolPath,
      attributes: Set[ParameterAttribute] = Set()
  )(init: => Tensor): ParamNode =
    paramCollection.getVar(nameSpace ++ relativePath, attributes)(init)

  def getConst(relativePath: SymbolPath)(init: => Tensor): Tensor =
    paramCollection.getConst(nameSpace ++ relativePath)(init)

  def withPrefix[A](prefix: SymbolPath)(f: SymbolPath => A): A = {
    val p = nameSpace ++ prefix
    f(p)
  }

  def linear(
      name: SymbolPath,
      nOut: Size,
      useBias: Boolean = true,
      initializer: WeightsInitializer = LayerFactory.xavier
  )(input: CompNode)(implicit mode: GraphMode): CompNode = withPrefix(name) { prefix =>
    val nIn = input.shape(1)
    val w = getVar(prefix / 'w, attributes = Set(NeedRegularization)) {
      initializer(nIn, nOut)
    }
    if (useBias) {
      val b = getVar(prefix / 'b)(ns.zeros(1, nOut))
      input.dot(w) + b
    } else {
      input.dot(w)
    }
  }

  def dropout(keepProb: Double)(
      input: CompNode
  )(implicit mode: GraphMode): CompNode = {
    require(keepProb > 0 && keepProb < 1)
    val mask = (ns.rand(input.shape) < keepProb).boolToFloating / keepProb
    input * const(mask)
  }

  def batchNorm(
      name: SymbolPath,
      inTraining: Boolean,
      momentum: Double = 0.9,
      eps: Double = 1e-5
  )(input: CompNode)(implicit mode: GraphMode): CompNode = withPrefix(name) { prefix =>
    val inputMean = mean(input, axis = 0)
    val inputVariance = mean(square(input - inputMean), axis = 0)

    val runningMean = getConst(prefix / 'runningMean) { inputMean.value }
    val runningVariance = getConst(prefix / 'runningVariance) {
      inputVariance.value
    }

    val beta = getVar(prefix / 'beta) { ns.zeros(1, input.shape(1)) }
    val gamma = getVar(prefix / 'gamma) { ns.ones(1, input.shape(1)) }

    if (inTraining) {
      runningMean := (runningMean * momentum) + (inputMean.value * (1 - momentum))
      runningVariance := (runningVariance * momentum) + (inputVariance.value * (1 - momentum))

      val xModified = (input - inputMean) / sqrt(inputVariance + eps)
      xModified * gamma + beta
    } else {
      (input - runningMean) / sqrt(runningVariance + eps) * gamma + beta
    }
  }

  /**
    * Gated recurrent unit: [https://en.wikipedia.org/wiki/Gated_recurrent_unit]
    */
  def gru(
      name: SymbolPath,
      initializer: WeightsInitializer = LayerFactory.xavier
  )(state: CompNode, input: CompNode)(implicit mode: GraphMode): CompNode =
    withPrefix(name) { prefix =>
      val inputSize = input.shape(1)
      val stateSize = state.shape(1)

      val Wg = paramCollection
        .getVar(prefix / 'Wg, attributes = Set(NeedRegularization)) {
          initializer(inputSize, 2 * stateSize)
        }
      val Ug = paramCollection
        .getVar(prefix / 'Ug, attributes = Set(NeedRegularization)) {
          initializer(stateSize, 2 * stateSize)
        }
      val bg = paramCollection.getVar(prefix / 'bg) {
        ns.zeros(1, 2 * stateSize)
      }

      val gates = sigmoid(input.dot(Wg) + state.dot(Ug) + bg)
      val updateGate = gates.slice(:>, 0 :> stateSize)
      val restGate = gates.slice(:>, stateSize :>)

      val Wh = paramCollection
        .getVar(prefix / 'Wh, attributes = Set(NeedRegularization)) {
          initializer(inputSize, stateSize)
        }
      val Uh = paramCollection
        .getVar(prefix / 'Uh, attributes = Set(NeedRegularization)) {
          initializer(stateSize, stateSize)
        }
      val bh = paramCollection.getVar(prefix / 'bh) {
        ns.zeros(1, stateSize)
      }

      val hHat = tanh(input.dot(Wh) + (state * restGate).dot(Uh) + bh)
      updateGate * hHat + state * (-updateGate + 1)
    }

  /**
    * Long short-term memory unit: [https://en.wikipedia.org/wiki/Long_short-term_memory]
    */
  def lstm(
      name: SymbolPath,
      initializer: WeightsInitializer = LayerFactory.xavier
  )(
      hAndC: (CompNode, CompNode),
      input: CompNode
  )(implicit mode: GraphMode): (CompNode, CompNode) = withPrefix(name) { prefix =>
    val (h, c) = hAndC

    val inputSize = input.shape(1)
    val stateSize = h.shape(1)
    require(c.shape(1) == stateSize)

    val Wg = paramCollection
      .getVar(prefix / 'Wg, attributes = Set(NeedRegularization)) {
        initializer(inputSize, 3 * stateSize)
      }
    val Ug = paramCollection
      .getVar(prefix / 'Ug, attributes = Set(NeedRegularization)) {
        initializer(stateSize, 3 * stateSize)
      }
    val bg = paramCollection.getVar(prefix / 'bg) {
      ns.zeros(1, 3 * stateSize)
    }

    val gates = sigmoid(input.dot(Wg) + h.dot(Ug) + bg)
    val forgetGate = gates.slice(:>, 0 :> stateSize)
    val inputGate = gates.slice(:>, stateSize :> 2 * stateSize)
    val outputGate = gates.slice(:>, 2 * stateSize :>)

    val Wc = paramCollection
      .getVar(prefix / 'Wc, attributes = Set(NeedRegularization)) {
        initializer(inputSize, stateSize)
      }
    val Uc = paramCollection
      .getVar(prefix / 'Uc, attributes = Set(NeedRegularization)) {
        initializer(stateSize, stateSize)
      }
    val bc = paramCollection.getVar(prefix / 'bc) {
      ns.zeros(1, stateSize)
    }

    val c1 = forgetGate * c + inputGate * tanh(input.dot(Wc) + h.dot(Uc) + bc)
    val h1 = outputGate * tanh(c1)

    (h1, c1)
  }

  trait RnnUnit {
    def iterate(state: CompNode, input: CompNode): CompNode
  }

  /**
    * Run an RNN unit both forwardly and backwardly over the inputs, and then combine the
    * intermediate states
    */
  def bidirectionalGru(
      name: SymbolPath,
      stateShape: Shape,
      combiner: (CompNode, CompNode) => CompNode
  )(inputs: IS[CompNode])(implicit mode: GraphMode): IS[CompNode] = withPrefix(name) {
    prefix =>
      val leftInit: CompNode = getVar(prefix / 'leftInit) {
        ns.randn(stateShape)
      }
      val states1 = inputs.scanLeft(leftInit)(gru(name / 'leftRNN))
      val rightInit: CompNode = getVar(prefix / 'rightInit) {
        ns.randn(stateShape)
      }
      val states2 =
        inputs.reverse.scanLeft(rightInit)(gru(name / 'rightRNN)).reverse
      states1.zip(states2).map {
        case (l, r) => combiner(l, r)
      }
  }

  /** performs weighted-sum over ys using dot-product attention */
  def attentionLayer(
      name: SymbolPath,
      transformKey: Boolean = false,
      transformValue: Boolean = false
  )(xKey: CompNode, ys: IS[(CompNode, CompNode)])(implicit mode: GraphMode): CompNode =
    withPrefix(name) { _ =>
      require(ys.nonEmpty)
      val keyDim = xKey.shape(1)
      val valueDim = ys.head._2.shape(1)
      val sqrtN = math.sqrt(keyDim)
      val weightLogits = {
        val originalKeys = stackRows(ys.map(_._1))
        val keys =
          if (transformKey)
            linear(name / 'keyTransform2, keyDim, useBias = false)(originalKeys)
          else originalKeys
        keys.dot(xKey.t).t
      }
      val aWeights = softmax(weightLogits / sqrtN)

      assert(aWeights.shape.head == 1)
      val yOrigin = stackRows(ys.map(_._2))
      val yMat =
        if (transformValue)
          relu(linear(name / 'valueTransform, valueDim)(yOrigin))
        else yOrigin
      aWeights.dot(yMat)
    }

  //  def cnn2D(name: Symbol, kernelSize: (Int, Int))(input: CompNode): CompNode = withPrefix(name) { prefix =>
  //    val w = getVar(prefix / 'kernel, attributes = Set(NeedRegularization)){ ns.randn(kernelSize._1, kernelSize._2) }
  //    if(useBias){
  //      val b = getVar(prefix / 'b)(ns.zeros(1, nOut))
  //      input.dot(w) + b
  //    } else {
  //      input.dot(w)
  //    }
  //  }

}
