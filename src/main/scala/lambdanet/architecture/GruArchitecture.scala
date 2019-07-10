package lambdanet.architecture

import botkop.numsca
import funcdiff._
import lambdanet.translation.PredicateGraph.ProjNode

case class GruArchitecture(dimMessage: Int, pc: ParamCollection)
    extends NNArchitecture(s"gru-$dimMessage", dimMessage, pc) {

  def initialEmbedding(
      projectNodes: Set[ProjNode],
      labels: Set[Symbol],
  ): Embedding = {
    val vec = randomVar('nodeInitVec)
    val vars = projectNodes.map(_ -> vec).toMap
    val ls = labels.map(_ -> const(randomUnitVec())).toMap
    Embedding(vars, ls)
  }

  import layerFactory._

  private val emptyMessage = getVar('emptyMessage) { randomVec() }

  def update[K](
      name: SymbolPath,
      embedding: Map[K, CompNode],
      messages: Map[K, CompNode],
  ): Map[K, CompNode] = {
    import numsca._

    val inputs = embedding.toVector.map {
      case (k, v) =>
        k -> v.concat(messages.getOrElse(k, emptyMessage), axis = 1)
    }
    verticalBatching[K](inputs, stacked => {
      val old = stacked.slice(:>, 0 :> dimMessage)
      val msg = stacked.slice(:>, dimMessage :> 2 * dimMessage)
      gru(name / 'updateEmbedding)(old, msg)
    }).map {
      case (k, chain) =>
        val Vector(x) = chain.toVector
        k -> x
    }
  }
}
