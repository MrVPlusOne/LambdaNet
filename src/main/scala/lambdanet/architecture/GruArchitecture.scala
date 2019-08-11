package lambdanet.architecture

import botkop.numsca
import funcdiff._
import lambdanet.translation.PredicateGraph.ProjNode

case class GruArchitecture(dimEmbedding: Int, pc: ParamCollection)
    extends NNArchitecture(s"gru-$dimEmbedding", dimEmbedding, pc) {

  def initialEmbedding(projectNodes: Set[ProjNode]): Embedding = {
    val nodeVec = randomVar('nodeInitVec)
    val vars = projectNodes.map(_ -> nodeVec).toMap
    Embedding(vars)
  }

  import layerFactory._

  private val emptyMessage = getVar('emptyMessage) { randomVec() }

  def update[K](
      name: SymbolPath,
      embedding: Map[K, CompNode],
      messages: Map[K, CompNode]
  ): Map[K, CompNode] = {
    val inputs = embedding.toVector.map {
      case (k, v) =>
        k -> (v, messages.getOrElse(k, emptyMessage))
    }
    verticalBatching2[K](
      inputs,
      (old, msg) => gru(name / 'updateEmbedding)(old, msg)
    ).map {
      case (k, chain) =>
        val Vector(x) = chain.toVector
        k -> x
    }
  }
}
