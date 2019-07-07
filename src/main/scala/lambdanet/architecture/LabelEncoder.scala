package lambdanet.architecture

import funcdiff.CompNode

import scala.collection.GenSeq

trait LabelEncoder {
  def name: String

  def encode(labels: GenSeq[Symbol]): Symbol => CompNode
}
