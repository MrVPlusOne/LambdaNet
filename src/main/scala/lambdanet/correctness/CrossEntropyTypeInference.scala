package lambdanet.correctness

import lambdanet.translation.PredicateGraph.{PNode, PType}

import scala.util.Random

object CrossEntropyTypeInference {
  type Samples = IndexedSeq[Assignment]
  case class AssignmentGen(
      projectNodes: Set[PNode],
      subtypes: Map[PType, Set[PType]],
      parents: Map[PNode, Set[PNode]]
  ) extends ((TypeDistrs, Int) => Samples) {
    def apply(param: TypeDistrs, numSamples: Int): Samples =
      IndexedSeq.fill(numSamples)(gen(param))

    def gen(param: TypeDistrs): Assignment = {
      val assignment = collection.mutable.Map.empty[PNode, PType]
      val perm = Random.shuffle(projectNodes)
      for (node <- perm) {
        val allNodeTypes = param(node).distr.map(_._2).toSet
        val availableTypes = parents
          .getOrElse(node, Set.empty)
          .filter(assignment.contains)
          .foldLeft(allNodeTypes) {
            case (intersection, parent) =>
              val parentSubtypes = subtypes(assignment(parent))
              intersection intersect parentSubtypes
          }
        val nodeType = Sampling.choose(
          availableTypes.toSeq,
          availableTypes.toSeq.map(param(node).typeProb(_))
        )
        assignment += node -> nodeType
      }
      assignment.toMap
    }
  }
}
