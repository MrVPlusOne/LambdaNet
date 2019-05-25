import scala.collection.mutable

package object lambdanet {

  /** the path related to the project root */
  type ProjectPath = ammonite.ops.RelPath

  val returnSymbol = 'return
  val thisSymbol = 'this
  val superSymbol = 'super
  val defaultSymbol = 'default

  trait IdEquality {
    protected def id: Int

    override def hashCode(): Int = id.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case t: IdEquality => id == t.id
      case _             => false
    }
  }

  trait IdAllocator[T] {
    val allocated: mutable.Buffer[T] = mutable.Buffer()

    private var _id = 0

    def useNewId(f: Int => T): T = {
      val r = f(_id)
      _id += 1
      allocated += r
      r
    }
  }
}
