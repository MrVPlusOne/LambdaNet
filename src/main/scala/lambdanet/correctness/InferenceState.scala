package lambdanet.correctness

import lambdanet.translation.PredicateGraph.{PAny, PNode, PType}

import java.util.concurrent.ThreadLocalRandom
import scala.util.Random.javaRandomToRandom

trait InferenceState {
  def hasNext: Boolean
  // fixme: Perhaps should return set of nodes that will be propagated?
  def next: (PNode, Seq[PType])
  def updated(node: PNode, typ: PType): InferenceState
  def assignment: Assignment
}

case class BasicInferenceState(
    checker: ValidTypeGen,
    sameNodes: Array[Set[PNode]],
    precomputedValidTypes: Map[PNode, Seq[PType]],
    assignment: Assignment,
    currentNodesIndex: Int
) extends InferenceState {
  def updated(node: PNode, typ: PType): BasicInferenceState = {
    val nodes = sameNodes(currentNodesIndex)
    assert(nodes contains node)
    val newAssignment: Map[PNode, PType] = assignment ++ nodes.map(node => node -> typ)
    this.copy(assignment = newAssignment, currentNodesIndex = currentNodesIndex + 1)
  }

  def hasNext: Boolean = currentNodesIndex != sameNodes.length

  def next: (PNode, Seq[PType]) = {
    val nodes = sameNodes(currentNodesIndex)
    val node = nodes.head
    val allNodeTypes = precomputedValidTypes(node)
    val validTypes = checker.validTypes(allNodeTypes, nodes, assignment)
    assert(validTypes.nonEmpty, s"no available type for node $node")
    (node, validTypes)
  }
}

object BasicInferenceState {
  def apply(
      checker: ValidTypeGen,
      sameNodes: Set[Set[PNode]],
      precomputedValidTypes: Map[PNode, Seq[PType]],
      fixedAssignment: Assignment
  ): BasicInferenceState = {
    val random = ThreadLocalRandom.current()
    val permutedNodes = random.shuffle(sameNodes.map(random.shuffle(_))).toArray
    new BasicInferenceState(checker, permutedNodes, precomputedValidTypes, fixedAssignment.withDefaultValue(PAny), currentNodesIndex = 0)
  }
}
