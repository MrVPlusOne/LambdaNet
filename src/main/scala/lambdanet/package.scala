import lambdanet.types.GType

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

  sealed trait TyAnnot {
    def map(f: GType => GType): TyAnnot = this match {
      case TyAnnot.User(ty)  => TyAnnot.User(f(ty))
      case TyAnnot.Fixed(ty) => TyAnnot.Fixed(f(ty))
      case TyAnnot.Missing   => TyAnnot.Missing
    }

    override def toString: String = this match {
      case TyAnnot.User(ty)  => s"$ty"
      case TyAnnot.Fixed(ty) => s"$ty!"
      case TyAnnot.Missing   => "?"
    }

    def typeOpt: Option[GType] = this match {
      case TyAnnot.User(ty)  => Some(ty)
      case TyAnnot.Fixed(ty) => Some(ty)
      case TyAnnot.Missing   => None
    }

    def get: GType =
      typeOpt.getOrElse(throw new Error("Type annotation missing."))

    def forFixed(f: GType => Unit): Unit = this match {
      case TyAnnot.Fixed(ty) => f(ty)
      case _                 =>
    }
  }

  object TyAnnot {
    sealed trait WithType extends TyAnnot {
      def ty: GType
    }

    object WithType {
      def unapply(arg: WithType): Option[GType] = Some(arg.ty)
    }
    case class User(ty: GType) extends WithType

    case class Fixed(ty: GType) extends WithType

    case object Missing extends TyAnnot
  }

  implicit class AssertionSyntax[T](x: T) {
    def which(p: T => Boolean): T = {
      assert(p(x))
      x
    }
  }
}
