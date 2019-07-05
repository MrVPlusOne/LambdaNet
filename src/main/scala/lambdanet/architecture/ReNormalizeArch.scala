package lambdanet.architecture

import botkop.numsca.Shape
import funcdiff._
import lambdanet.translation.PredicateGraph

case class ReNormalizeArch(dimMessage: Int, pc: ParamCollection)
    extends NNArchitecture(s"renorm-$dimMessage", dimMessage, pc) {
  import layerFactory._

  def initialEmbedding(
      projectNodes: Set[PredicateGraph.ProjNode],
  ): Embedding = {
    val vec = randomUnitVar('nodeInitVec)
    projectNodes.map(_ -> vec).toMap
  }

  private val emptyMessage = getVar('emptyMessage) { randomVec() }

  def update(
      embedding: Map[PredicateGraph.ProjNode, CompNode],
      messages: Map[PredicateGraph.ProjNode, CompNode],
  ): Map[PredicateGraph.ProjNode, CompNode] = {
    val inputs = embedding.toVector.map {
      case (k, v) =>
        k -> v.concat(messages.getOrElse(k, emptyMessage), axis = 1)
    }
    verticalBatching(
      inputs,
      stacked => singleLayer('updateEmbedding, stacked) ~> renormalize,
    ).mapValuesNow { chain =>
      val Vector(x) = chain.toVector
      x
    }
  }

  private val epsilon = 1e-9
  private def renormalize(vecs: CompNode): CompNode = {
    val lens = sqrt(sum(square(vecs), axis = 1))
    assert(lens.shape == Shape(Vector(vecs.shape(0), 1)))
    vecs / (lens + epsilon)
  }
}
