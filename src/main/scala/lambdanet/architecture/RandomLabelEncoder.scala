package lambdanet.architecture

import funcdiff._

import scala.collection.GenSeq

case class RandomLabelEncoder(architecture: NNArchitecture)
    extends LabelEncoder {
  def name: String = "RandomLabelEncoder"

  def encode(labels: GenSeq[Symbol]): Symbol => CompNode = {
    val map = labels.map { l =>
      l -> const(architecture.randomUnitVec())
    }.toMap
    map.apply
  }
}
