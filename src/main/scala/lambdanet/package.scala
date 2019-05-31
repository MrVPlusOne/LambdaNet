import lambdanet.GType

import scala.collection.mutable

package object lambdanet {

  /** the path related to the project root */
  type ProjectPath = ammonite.ops.RelPath

  val returnSymbol = 'return
  val thisSymbol = 'this
  val superSymbol = 'super
  val defaultSymbol = 'default
  val undefinedSymbol = 'undefined

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

  sealed trait Annot[+T] {
    def map[B](f: T => B): Annot[B] = this match {
      case Annot.User(ty)  => Annot.User(f(ty))
      case Annot.Fixed(ty) => Annot.Fixed(f(ty))
      case Annot.Missing   => Annot.Missing
    }

    override def toString: String = this match {
      case Annot.User(ty)  => s"$ty"
      case Annot.Fixed(ty) => s"$ty!"
      case Annot.Missing   => "?"
    }

    def typeOpt: Option[T] = this match {
      case Annot.User(ty)  => Some(ty)
      case Annot.Fixed(ty) => Some(ty)
      case Annot.Missing   => None
    }

    def get: T =
      typeOpt.getOrElse(throw new Error("Type annotation missing."))

    def forFixed(f: T => Unit): Unit = this match {
      case Annot.Fixed(ty) => f(ty)
      case _               =>
    }
  }

  object Annot {
    sealed trait WithContent[T] extends Annot[T] {
      def ty: T
    }

    object WithContent {
      def unapply[T](arg: WithContent[T]): Option[T] = Some(arg.ty)
    }
    case class User[T](ty: T) extends WithContent[T]

    case class Fixed[T](ty: T) extends WithContent[T]

    case object Missing extends Annot[Nothing]
  }

  implicit class AssertionSyntax[T](x: T) {
    def which(p: T => Boolean): T = {
      assert(p(x))
      x
    }
  }

  type TyAnnot = Annot[GType]
}
