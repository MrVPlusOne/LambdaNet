package funcdiff

import botkop.numsca.Tensor
import botkop.{numsca => ns}
import funcdiff.Optimizer.Adam.Momentum
import funcdiff.Optimizer.OptimizeStats
import funcdiff.ParameterAttribute.NeedRegularization

import collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

trait Optimizer extends Serializable {

  /** Given gradients, how much each gradient should change when maximizing the objective? */
  def parameterChangeAmount(param: Param, grad: Gradient): Gradient

  var warnEmptyUpdates = true

  var printUpdates = false

  def maximize(
      objective: CompNode,
      params: Seq[Param],
      weightDecay: Option[Double] = None,
      gradientTransform: Gradient => Gradient = identity,
      backPropInParallel: Option[(ExecutionContext, Duration)] = None,
      scaleLearningRate: Double = 1.0
  ): OptimizeStats = {

    if (warnEmptyUpdates && params.isEmpty) {
      println("Warning: optimizer's param list is empty.")
    }

    val gradients = objective.backpropForParams(backPropInParallel)

    import collection.mutable
    val newlyCreated = mutable.HashSet[ParamNode]()

    for {
      decay <- weightDecay
      p <- params if p.attributes.contains(NeedRegularization)
    } {
      val v = p.node.value * (1.0 - decay)
      p.node = paramNode(v, p.path)
      newlyCreated += p.node
    }

    val paramMap = params.map(p => p.path -> p).toMap

    val transformed = gradients.mapValues(gradientTransform)
    val deltas = for {
      (path, g) <- transformed
      p <- paramMap.get(path)
      delta = parameterChangeAmount(p, g) * scaleLearningRate
    } yield {
      if (newlyCreated contains p.node) {
        delta.addToTensor(p.node.value)
      } else {
        val newValue = p.node.value.copy()
        delta.addToTensor(newValue)
        p.node = paramNode(newValue, p.path)
      }

      if (printUpdates) {
        println(s"update param $p")
      }
      path -> delta
    }

    OptimizeStats(gradients, transformed, deltas)
  }

  def minimize(
      objective: CompNode,
      params: Seq[Param],
      weightDecay: Option[Double] = None,
      gradientTransform: Gradient => Gradient = identity,
      backPropInParallel: Option[(ExecutionContext, Duration)] = None,
      scaleLearningRate: Double = 1.0
  ): OptimizeStats = {
    maximize(
      -objective,
      params,
      weightDecay,
      gradientTransform,
      backPropInParallel,
      scaleLearningRate,
    )
  }
}

object Optimizer {
  case class OptimizeStats(
      gradients: Map[SymbolPath, Gradient],
      transformedGrads: Map[SymbolPath, Gradient],
      deltas: Map[SymbolPath, Gradient],
  )

  @SerialVersionUID(0)
  case class SGD(learningRate: Double) extends Optimizer {

    def parameterChangeAmount(param: Param, grad: Gradient): Gradient = {
      grad * learningRate
    }

  }

  object Adam {
    @SerialVersionUID(0)
    case class Momentum(m: Tensor, v: Tensor, var t: Int = 0)

  }
  @SerialVersionUID(0)
  case class Adam(
      learningRate: Double,
      beta1: Double = 0.9,
      beta2: Double = 0.999,
      epsilon: Double = 1e-8,
      momenta: mutable.HashMap[SymbolPath, Momentum] = mutable.HashMap(),
  ) extends Optimizer {

    override def toString: String =
      s"Adam(lr=$learningRate, beta1=$beta1, beta2=$beta2, epsilon=$epsilon)"

    def parameterChangeAmount(p: Param, g: Gradient): Gradient = {
      val mem @ Momentum(m, v, _) =
        momenta.getOrElse(
          p.path,
          Momentum(ns.zeros(g.shape()), ns.zeros(g.shape())),
        )
      momenta(p.path) = mem
      mem.t += 1
      m *= beta1
      (g * (1 - beta1)).addToTensor(m)

      val mt = m / (1 - math.pow(beta1, mem.t))

      v *= beta2

      val squareG = g match {
        case zg: ZeroGradient     => zg
        case DenseGradient(core)  => DenseGradient(ns.square(core))
        case ig: InflatedGradient => ig.copy(core = ns.square(ig.core))
      }

      (squareG * (1.0 - beta2)).addToTensor(v)
      val vt = v / (1 - math.pow(beta2, mem.t))
      DenseGradient(mt * learningRate / (ns.sqrt(vt) + epsilon))
    }
  }
}
