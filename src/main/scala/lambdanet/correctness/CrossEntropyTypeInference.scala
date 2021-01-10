package lambdanet.correctness

import lambdanet.translation.PredicateGraph.{PAny, PNode, PType}

import scala.util.Random

object CrossEntropyTypeInference {
  type Samples = IndexedSeq[Assignment]
  case class AssignmentGen(
      projectNodes: Set[PNode],
      context: PTypeContext,
      parents: Map[PNode, Set[PNode]]
  ) extends ((TypeDistrs, Int) => Samples) {
    def apply(param: TypeDistrs, numSamples: Int): Samples =
      IndexedSeq.fill(numSamples)(gen(param))

    def gen(param: TypeDistrs): Assignment = {
      var assignment = Map.empty[PNode, PType].withDefaultValue(PAny)
      val perm = Random.shuffle(projectNodes)
      for (node <- perm) {
        val allNodeTypes = param(node).distr.map(_._2).toSet
        val availableTypes = parents
          .getOrElse(node, Set.empty)
          .foldLeft(allNodeTypes) {
            case (subtypes, parent) =>
              subtypes.filter(typ => context.isSubtype(node, parent, assignment.updated(node, typ)))
          }
        assert(availableTypes.nonEmpty, s"no available type for node $node")
        val nodeType = Sampling.choose(
          availableTypes.toSeq,
          availableTypes.toSeq.map(param(node).typeProb(_))
        )
        assignment = assignment.updated(node, nodeType)
      }
      assignment
    }
  }
}
