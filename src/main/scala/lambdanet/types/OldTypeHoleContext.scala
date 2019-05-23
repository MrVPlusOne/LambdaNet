package lambdanet.types

import lambdanet.surface.TypeAnnotation

import scala.collection.mutable

class OldTypeHoleContext {
  var typeHoleId: Int = 0
  val holeTypeMap: mutable.HashMap[GTHole, GType] = mutable.HashMap[GTHole, GType]()
  val userAnnotatedSet: mutable.HashSet[GTHole] = mutable.HashSet[GTHole]()

  def newTHole(mark: Option[TypeAnnotation]): GTHole = {
    val h = new GTHole(typeHoleId, mark.map(_.ty))
    typeHoleId += 1
    mark.foreach { m =>
      assert(!holeTypeMap.contains(h))
      holeTypeMap(h) = m.ty
      if (m.needInfer) {
        userAnnotatedSet += h
      }
    }
    h
  }
}

class TypeHoleContext {
  var typeHoleId: Int = 0

  def newTHole(mark: Option[GType]): GTHole = {
    val h = new GTHole(typeHoleId, mark)
    typeHoleId += 1
    h
  }
}
