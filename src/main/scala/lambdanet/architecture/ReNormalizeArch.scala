package lambdanet.architecture

import botkop.numsca.Shape
import funcdiff._
import lambdanet.translation.PredicateGraph

case class ReNormalizeArch(dimEmbedding: Int, pc: ParamCollection)
    extends NNArchitecture(s"renorm-$dimEmbedding", dimEmbedding, pc) {
  import layerFactory._

  def initialEmbedding(projectNodes: Set[PredicateGraph.ProjNode], labels: Set[Symbol], encodeLibLabel: Symbol => CompNode): Embedding = {
    val vec = randomVar('nodeInitVec)
    val vars = projectNodes.map(_ -> vec).toMap
    val ls = labels.map(_ -> const(randomUnitVec())).toMap
    Embedding(vars, ls)
  }

  private val emptyMessage = getVar('emptyMessage) { randomVec() }

  def update[K](
      name: SymbolPath,
      embedding: Map[K, CompNode],
      messages: Map[K, CompNode],
  ): Map[K, CompNode] = {
    val inputs = embedding.toVector.map {
      case (k, v) =>
        k -> v.concat(messages.getOrElse(k, emptyMessage), axis = 1)
    }
    verticalBatching(
      inputs,
      stacked => singleLayer('updateEmbedding, stacked) ~> renormalize,
    ).map {
      case (k, chain) =>
        val Vector(x) = chain.toVector
        k -> x
    }
  }

  private val epsilon = 1e-9
  private def renormalize(vecs: CompNode): CompNode = {
    val lens = sqrt(sum(square(vecs), axis = 1))
    assert(lens.shape == Shape(Vector(vecs.shape(0), 1)))
    vecs / (lens + epsilon)
  }
}
