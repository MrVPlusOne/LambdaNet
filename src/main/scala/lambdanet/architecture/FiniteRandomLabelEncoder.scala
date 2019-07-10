package lambdanet.architecture

import lambdanet._
import funcdiff._

import scala.collection.GenSeq
import scala.util.Random

case class FiniteRandomLabelEncoder(size: Int, architecture: NNArchitecture, random: Random)
    extends LabelEncoder {
  def name: String = s"Finite[size=$size]RandomLabelEncoder"

  private val labelVars = (0 until size)
    .map(i => architecture.randomUnitVar('labels / Symbol(s"$i")))
    .toVector

  def encode(labels: GenSeq[Symbol]): Symbol => CompNode = {
    val n = labels.length
    if(n > size){
      throw new Error(s"Number of labels ($n) exceeds label encoder size ($size)")
    }
    val used = random.shuffle(labelVars).take(n)
    val map = labels.zip(used).toMap
    map.apply
  }

}
