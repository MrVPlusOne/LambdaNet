package lambdanet.types

import lambdanet.{Annot, GTHole, GType, IdAllocator, TyAnnot}

import scala.collection.mutable

class OldTypeHoleContext {
  var typeHoleId: Int = 0
  val holeTypeMap: mutable.HashMap[GTHole, GType] =
    mutable.HashMap[GTHole, GType]()
  val userAnnotatedSet: mutable.HashSet[GTHole] = mutable.HashSet[GTHole]()

  def newTHole(mark: TyAnnot): GTHole = ???
}

class TypeHoleContext extends IdAllocator[GTHole] {

  def newTHole(mark: TyAnnot): GTHole =
    useNewId(id => new GTHole(id, mark))
}
