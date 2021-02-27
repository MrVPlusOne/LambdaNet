package lambdanet.correctness

import lambdanet.translation.PredicateGraph.{PNode, PType}

trait InferenceState extends Iterator[(PNode, Seq[PType])]{
  def updated(node: PNode, typ: PType): InferenceState
  def assignment: Assignment
}