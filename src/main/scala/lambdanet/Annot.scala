package lambdanet

sealed trait Annot[+T] {
  def map[B](f: T => B): Annot[B] = this match {
    case Annot.User(ty, b) => Annot.User(f(ty), b)
    case Annot.Fixed(ty)   => Annot.Fixed(f(ty))
    case Annot.Missing     => Annot.Missing
  }

  override def toString: String = this match {
    case Annot.User(ty, b) => (if (b) "[i]" else "") + s"$ty"
    case Annot.Fixed(ty)   => s"$ty!"
    case Annot.Missing     => "?"
  }

  def typeOpt: Option[T] = this match {
    case Annot.User(ty, _) => Some(ty)
    case Annot.Fixed(ty)   => Some(ty)
    case Annot.Missing     => None
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
  case class User[T](ty: T, inferred: Boolean) extends WithContent[T]

  case class Fixed[T](ty: T) extends WithContent[T]

  case object Missing extends Annot[Nothing]
}
