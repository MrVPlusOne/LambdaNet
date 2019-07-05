package lambdanet.architecture

import botkop.numsca
import funcdiff._
import lambdanet.translation.PredicateGraph.{ProjNode}

case class GruArchitecture(dimMessage: Int, pc: ParamCollection)
    extends NNArchitecture(s"hybrid-$dimMessage", dimMessage, pc) {

  def initialEmbedding(projectNodes: Set[ProjNode]): Embedding = {
    val vec = randomVar('nodeInitVec)
    projectNodes.map(_ -> vec).toMap
  }

  import layerFactory._

  private val emptyMessage = getVar('emptyMessage) { randomVec() }

  def update(
      embedding: Map[ProjNode, CompNode],
      messages: Map[ProjNode, CompNode],
  ): Map[ProjNode, CompNode] = {
    import numsca._

    val inputs = embedding.toVector.map {
      case (k, v) =>
        k -> v.concat(messages.getOrElse(k, emptyMessage), axis = 1)
    }
    verticalBatching(inputs, stacked => {
      val old = stacked.slice(:>, 0 :> dimMessage)
      val msg = stacked.slice(:>, dimMessage :> 2 * dimMessage)
      gru('updateEmbedding)(old, msg)
    }).mapValuesNow { chain =>
      val Vector(x) = chain.toVector
      x
    }
  }
}
