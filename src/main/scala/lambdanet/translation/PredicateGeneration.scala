package lambdanet.translation

import lambdanet.IdEqualityAllocator
import lambdanet.translation.IR._
import lambdanet.translation.PredicateGraph.{LibraryType, PNode}

object PredicateGeneration {
  case class PContext(
      vars: Map[Var, PNode],
      types: Map[TypeName, PNode],
      nameSpaces: Map[Symbol, PContext]
  )

}
