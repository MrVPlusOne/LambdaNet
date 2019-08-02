package lambdanet

import lambdanet.translation.ImportsResolution.NameDef
import lambdanet.translation.PredicateGraph.{PTyVar, PType, PNode}
import PredictionSpace._

object PredictionSpace {
  val unknownTypeNode: PNode = NameDef.unknownDef.ty.get
  val unknownType: PType = PTyVar(unknownTypeNode)
}

case class PredictionSpace(allTypes: Set[PType]) {
  val libTypeVec: Vector[PType] = allTypes.filter(_.madeFromLibTypes).toVector
  val projTypeVec: Vector[PType] = allTypes.filterNot(_.madeFromLibTypes).toVector
  val typeVector: Vector[PType] = libTypeVec ++ projTypeVec

  val size: Int = typeVector.size

  private val indexMap: Map[PType, Int] = {
    typeVector.zipWithIndex.toMap
  }

  def indexOfType(ty: PType): Int = {
    indexMap(ty)
  }

  def isLibType(i: Int): Boolean = {
    require(i < size, s"i = $i >= size = $size")
    typeVector(i).madeFromLibTypes
  }

}
