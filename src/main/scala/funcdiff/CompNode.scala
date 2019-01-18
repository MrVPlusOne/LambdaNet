package funcdiff

import botkop.numsca._
import funcdiff.DiffFunc.ConstFunc
import SimpleMath.Extensions._

class CompNode(val func: DiffFunc){
  def value: Tensor = func.value

  def shape: Array[Int] = value.shape

  def backprop: Map[CompNode, Gradient] = CompNode.backprop(this)

  def backpropForParams: Map[SymbolPath, Gradient] = {
    this.backprop.collect{case (n: ParamNode, g) if g.nonZero => n.path -> g}
  }

  override def toString: String = {
    s"Node[name=${func.name}, shape=${TensorExtension.showShape(shape)}, value=$value]"
  }

  def withShape(shape: Int*): CompNode = {
    assert(this.shape sameElements shape,
      s"expected shape: ${TensorExtension.showShape(shape.toArray)}, " +
        s"actual: ${TensorExtension.showShape(this.shape)}, node: $this")
    this
  }
}

class ParamNode(v: Tensor, val path: SymbolPath) extends CompNode(ConstFunc(v)){

  override def toString: String = {
    s"param{$path, shape=${TensorExtension.showShape(value.shape)}"
  }
}



object CompNode {
  /**
    * Back-propagate gradients through the computation graph
    */
  def backprop(nodes: List[CompNode], grads: List[Gradient]): Map[CompNode, Gradient] = {
    import collection.mutable
    assert(nodes.length == grads.length)

    val sorted = topologicalSort(nodes)
    val gBuilders = grads.map{g => new GradientBuilder(g, needCopy = true)}
    val gradients = mutable.HashMap[CompNode, GradientBuilder](nodes.zip(gBuilders) :_*)

    for(n <- sorted;
        gBuilder <- gradients.get(n);
        argGradients = n.func.backProp(gBuilder.retrieve);
        (arg, grad) <- n.func.args.zip(argGradients) if !grad.isZero) {

      val aG = gradients.getOrElse(arg, new GradientBuilder(ZeroGradient(arg.shape), needCopy = false))
//      val aG = gradients.getOrElse(arg, DenseGradient(numsca.zeros(arg.shape)))
      gradients(arg) = aG
      aG.add(grad)
    }

    assert(grads.any(_.isZero) || !gradients.values.exists(_.retrieve.isZero),
      "nonempty gradients after backprop created empty gradients!")
    gradients.toMap.mapValuesNow(_.retrieve)
  }

  def backprop(node: CompNode): Map[CompNode, Gradient] = {
    backprop(List(node), List(DenseGradient(ones(node.shape))))
  }


  /**
    * Returns a reverse-topologically sorted list of the nodes. The first element corresponds
    * to the first node we should perform backprop on.
    */
  def topologicalSort(nodes: List[CompNode]): List[CompNode] = {
    import collection.mutable
    val nodeCounters = mutable.HashMap[CompNode, Int]()

    def setupCounter(n: CompNode): Unit = {
      nodeCounters.get(n) match {
        case None =>
          nodeCounters(n) = 1
          n.func.args.foreach(setupCounter)
        case Some(c) =>
          nodeCounters(n) = c + 1
      }
    }

    def rec(sorted: List[CompNode], left: List[CompNode]): List[CompNode] = {
      left match {
        case h :: tl =>
          var newLeft = tl
          h.func.args.foreach{ n =>
            nodeCounters(n) -= 1
            if(nodeCounters(n) == 0) {
              newLeft = n :: newLeft
            }
          }
          rec(h :: sorted, newLeft)
        case List() => sorted
      }
    }

    nodes.foreach( setupCounter )
    rec(List(), nodes).reverse
  }
}