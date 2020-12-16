package lambdanet.correctness

import lambdanet.translation.PAnnot
import lambdanet.translation.PredicateGraph.{PNode, PType}

// TODO: Can pre-filter annots for better performance
case class AnnotAndType(annot: PType, typ: PType)

case class Accuracy private(truth: Assignment) {
  def getWrongNodes(assignment: Assignment): Map[PNode, AnnotAndType] = {
    val annotatedNodes = assignment.filterKeys(truth.contains)
    for {
      (node, typ) <- annotatedNodes
      gold = truth(node)
      if gold != typ
    } yield (node, AnnotAndType(gold, typ))
  }

  def get(assignment: Assignment): Double = {
    val annotatedNodes = assignment.filterKeys(truth.contains)
    val correctNodeCount = annotatedNodes.count {
      case (node, typ) =>
        truth(node) == typ
    }
    correctNodeCount / annotatedNodes.size.toDouble
  }
}

object Accuracy {
  def apply(groundTruth: GroundTruth): Accuracy = Accuracy(groundTruth.truth)
}

case class GroundTruth(truth: Assignment)
object GroundTruth {
  // dummyImplicit needed to avoid type erasure issue
  def apply(
    annots: Map[PNode, PAnnot]
  )(implicit dummyImplicit: DummyImplicit): GroundTruth = {
    val truth = annots.collect {
      case (node, annot) if annot.typeOpt.nonEmpty => (node, annot.typeOpt.get)
    }
    GroundTruth(truth)
  }
}
