package lambdanet

import funcdiff.CompNode
import lambdanet.translation.PredicateGraph.ProjNode

package object architecture {
  case class Embedding(
      vars: Map[ProjNode, CompNode]
  )
}
