package funcdiff

import botkop.numsca.Tensor
import botkop.{numsca => ns}
import API._
import funcdiff.Optimizers.Adam.Momentum
import funcdiff.ParameterAttribute.NeedRegularization
import collection.mutable

import scala.concurrent.ExecutionContext

trait Optimizer extends Serializable {

  /** Given gradients, how much each gradient should change when maximizing the objective? */
  def parameterChangeAmount(param: Param, grad: Gradient): Gradient

  var warnEmptyUpdates = true

  var printUpdates = false

  def maximize(objective: CompNode,
               params: Seq[Param],
               weightDecay: Option[Double] = None,
               gradientTransform: Gradient => Gradient = identity,
               backPropInParallel: Option[ExecutionContext] = None): Unit = {

    if(warnEmptyUpdates && params.isEmpty){
      println("Warning: optimizer's param list is empty.")
    }

    import collection.mutable
    val newlyCreated = mutable.HashSet[ParamNode]()

    for (decay <- weightDecay;
         p <- params if p.attributes.contains(NeedRegularization)){
      val v = p.node.value * (1.0-decay)
      p.node = paramNode(v, p.path)
      newlyCreated += p.node
    }

    val paramMap = params.map(p => p.path -> p).toMap
    val gradients = objective.backpropForParams(backPropInParallel)
    for((path, g0) <- gradients;
        p <- paramMap.get(path);
        g = parameterChangeAmount(p, gradientTransform(g0))){

      if(newlyCreated contains p.node){
        g.addToTensor(p.node.value)
      } else {
        val newValue = p.node.value.copy()
        g.addToTensor(newValue)
        p.node = paramNode(newValue, p.path)
      }

      if (printUpdates) {
        println(s"update param $p")
      }
    }
  }

  def minimize(objective: CompNode,
               params: Seq[Param],
               weightDecay: Option[Double] = None,
               gradientTransform: Gradient => Gradient = identity,
               backPropInParallel: Option[ExecutionContext] = None): Unit = {
    maximize(-objective, params, weightDecay, gradientTransform, backPropInParallel)
  }
}

object Optimizers {
  case class SGD(learningRate: Double) extends Optimizer {

    def parameterChangeAmount(param: Param, grad: Gradient): Gradient = {
      grad * learningRate
    }

  }

  object Adam{
    @SerialVersionUID(0)
    case class Momentum(m: Tensor, v: Tensor, var t: Int = 0)

  }


  @SerialVersionUID(0)
  case class Adam(learningRate: Double,
                  beta1: Double = 0.9,
                  beta2: Double = 0.999,
                  epsilon: Double = 1e-8,
                  momenta: mutable.HashMap[SymbolPath, Momentum] = mutable.HashMap()
                 ) extends Optimizer {

    def parameterChangeAmount(p: Param, g: Gradient): Gradient = {
      val mem@ Momentum(m, v, _) = momenta.getOrElse(p.path, Momentum(ns.zeros(g.shape), ns.zeros(g.shape)))
      momenta(p.path) = mem
      mem.t += 1
      m *= beta1
      (g * (1-beta1)).addToTensor(m)

      val mt = m / (1 - math.pow(beta1, mem.t))

      v *= beta2

      val squareG = g match {
        case zg: ZeroGradient => zg
        case DenseGradient(core) => DenseGradient(ns.square(core))
        case ig: InflatedGradient => ig.copy(core = ns.square(ig.core))
      }

      (squareG * (1.0 - beta2)).addToTensor(v)
      val vt = v / (1 - math.pow(beta2, mem.t))
      DenseGradient(mt * learningRate / (ns.sqrt(vt) + epsilon))
    }
  }
}