package funcdiff

import botkop.numsca.Tensor
import botkop.{numsca => ns}
import API._
import funcdiff.LayerFactory.WeightsInitializer
import funcdiff.ParameterAttribute.NeedRegularization

object LayerFactory {
  type WeightsInitializer = (Int, Int) => Tensor

  def xavier(nIn: Int, nOut: Int): Tensor = {
    ns.randn(nIn, nOut) * math.sqrt(2.0 / (nIn + nOut))
  }
}

case class LayerFactory(nameSpace: SymbolPath, params: ParamCollection) {
  import params._

  def getVar(
    relativePath: SymbolPath,
    attributes: Set[ParameterAttribute] = Set()
  )(init: => Tensor): ParamNode =
    params.getVar(nameSpace ++ relativePath, attributes)(init)

  def getConst(relativePath: SymbolPath)(init: => Tensor): Tensor =
    params.getConst(nameSpace ++ relativePath)(init)

  def withPrefix[A](prefix: SymbolPath)(f: SymbolPath => A): A = {
    val p = nameSpace ++ prefix
    f(p)
  }

  def linear(
    name: SymbolPath,
    nOut: Int,
    useBias: Boolean = true,
    initializer: WeightsInitializer = LayerFactory.xavier
  )(input: CompNode): CompNode = withPrefix(name) { prefix =>
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

  def dropout(keepProb: Double, inTraining: Boolean)(input: CompNode): CompNode = {
    if (keepProb == 1 || !inTraining) input
    else {
      require(keepProb > 0 && keepProb < 1)
      val mask = (ns.rand(input.shape) < keepProb) / keepProb
      input * const(mask)
    }
  }

  def batchNorm(
    name: SymbolPath,
    inTraining: Boolean,
    momentum: Double = 0.9,
    eps: Double = 1e-5
  )(input: CompNode): CompNode = withPrefix(name) { prefix =>
    val inputMean = mean(input, axis = 0)
    val inputVariance = mean(square(input - inputMean), axis = 0)

    val runningMean = getConst(prefix / 'runningMean) { inputMean.value }
    val runningVariance = getConst(prefix / 'runningVariance) { inputVariance.value }

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
  )(state: CompNode, input: CompNode): CompNode = withPrefix(name) { prefix =>
    val inputSize = input.shape(1)
    val stateSize = state.shape(1)

    val Wg = params.getVar(prefix / 'Wg, attributes = Set(NeedRegularization)) {
      initializer(inputSize, 2 * stateSize)
    }
    val Ug = params.getVar(prefix / 'Ug, attributes = Set(NeedRegularization)) {
      initializer(stateSize, 2 * stateSize)
    }
    val bg = params.getVar(prefix / 'bg) {
      ns.zeros(1, 2 * stateSize)
    }

    val gates = sigmoid(input.dot(Wg) + state.dot(Ug) + bg)
    val updateGate = gates.slice(:>, 0 :> stateSize)
    val restGate = gates.slice(:>, stateSize :>)

    val Wh = params.getVar(prefix / 'Wh, attributes = Set(NeedRegularization)) {
      initializer(inputSize, stateSize)
    }
    val Uh = params.getVar(prefix / 'Uh, attributes = Set(NeedRegularization)) {
      initializer(stateSize, stateSize)
    }
    val bh = params.getVar(prefix / 'bh) {
      ns.zeros(1, stateSize)
    }

    val hHat = tanh(input.dot(Wh) + (state * restGate).dot(Uh) + bh)
    updateGate * hHat + state * ((1: CompNode) - updateGate)
  }

  /**
    * Long short-term memory unit: [https://en.wikipedia.org/wiki/Long_short-term_memory]
    */
  def lstm(name: SymbolPath, initializer: WeightsInitializer = LayerFactory.xavier)(
    hAndC: (CompNode, CompNode),
    input: CompNode
  ): (CompNode, CompNode) = withPrefix(name) { prefix =>
    val (h, c) = hAndC

    val inputSize = input.shape(1)
    val stateSize = h.shape(1)
    require(c.shape(1) == stateSize)

    val Wg = params.getVar(prefix / 'Wg, attributes = Set(NeedRegularization)) {
      initializer(inputSize, 3 * stateSize)
    }
    val Ug = params.getVar(prefix / 'Ug, attributes = Set(NeedRegularization)) {
      initializer(stateSize, 3 * stateSize)
    }
    val bg = params.getVar(prefix / 'bg) {
      ns.zeros(1, 3 * stateSize)
    }

    val gates = sigmoid(input.dot(Wg) + h.dot(Ug) + bg)
    val forgetGate = gates.slice(:>, 0 :> stateSize)
    val inputGate = gates.slice(:>, stateSize :> 2 * stateSize)
    val outputGate = gates.slice(:>, 2 * stateSize :>)

    val Wc = params.getVar(prefix / 'Wc, attributes = Set(NeedRegularization)) {
      initializer(inputSize, stateSize)
    }
    val Uc = params.getVar(prefix / 'Uc, attributes = Set(NeedRegularization)) {
      initializer(stateSize, stateSize)
    }
    val bc = params.getVar(prefix / 'bc) {
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
    stateShape: Array[Int],
    combiner: (CompNode, CompNode) => CompNode
  )(inputs: IS[CompNode]): IS[CompNode] = withPrefix(name) { prefix =>
    val leftInit: CompNode = getVar(prefix / 'leftInit) {
      ns.randn(stateShape)
    }
    val states1 = inputs.scanLeft(leftInit)(gru(name / 'leftRNN))
    val rightInit: CompNode = getVar(prefix / 'rightInit) {
      ns.randn(stateShape)
    }
    val states2 = inputs.reverse.scanLeft(rightInit)(gru(name / 'rightRNN)).reverse
    states1.zip(states2).map {
      case (l, r) => combiner(l, r)
    }
  }

  /** performs weighted-sum over ys using dot-product attention */
  def attentionLayer(
    name: SymbolPath,
    attentionDim: Int
  )(xKey: CompNode, ys: IS[(CompNode, CompNode)]): CompNode =
    withPrefix(name) { _ =>
      val sqrtN = math.sqrt(xKey.shape(1))
      val weightLogits = concatN(ys.map(_._1), axis = 0).dot(xKey.t).t
      val aWeights = softmax(weightLogits / sqrtN)
      if (aWeights.shape.head == 1) {
        val yMat = concatN(ys.map(_._2), axis = 0)
        aWeights.dot(yMat)
      } else {
        total(ys.indices.map { i =>
          aWeights.slice(:>, i :> (i + 1)) * ys(i)._2
        })
      }
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
