package lambdanet

import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PNode, PType}

package object correctness {
  type Assignment = Map[PNode, PType]
  type TypeDistrs = Map[PNode, TopNDistribution[PType]]

  type NodeOrType = Either[PNode, PType]
  type Bounds[T] = Map[PNode, Set[Either[T, T]]]
}
