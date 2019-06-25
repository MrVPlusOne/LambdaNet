package lambdanet

import lambdanet.translation.ImportsResolution.NameDef
import lambdanet.translation.PredicateGraph.{PTyVar, PType}
import PredictionSpace._

object PredictionSpace{
  val unknownType = PTyVar(NameDef.unknownDef.ty.get)
}

case class PredictionSpace(allTypes: Set[PType]) {
  val typeVector: Vector[PType] = allTypes.toVector
  val maxIndex: Int = typeVector.length

  private val indexMap: Map[PType, Int] = {
    typeVector.zipWithIndex.toMap
  }
  require(indexMap.contains(unknownType))

  def indexOfType(ty: PType): Int = {
    indexMap.getOrElse(ty, indexMap(unknownType))
  }

}