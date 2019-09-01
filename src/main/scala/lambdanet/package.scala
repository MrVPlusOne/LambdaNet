import ammonite.ops.RelPath
import funcdiff.SimpleMath
import lambdanet.translation.PredicateGraph
import org.nd4j.linalg.api.buffer.DataType
import org.nd4j.linalg.factory.Nd4j

import scala.collection.mutable

/**
  * == LambdaNet: Deep Probabilistic Type Inference that Type-Checks ==
  *
  * Main classes to look at:
  * [[lambdanet.utils.ProgramParsing]]: Parsing from Typescript source code to the [[lambdanet.Surface]] AST.
  * [[lambdanet.translation]]: A serious of transformations from [[lambdanet.Surface]] to [[lambdanet.translation.PredicateGraph]].
  *
  *
  */
package object lambdanet extends SimpleMath.ExtensionsTrait {

  Nd4j.setDefaultDataTypes(DataType.DOUBLE, DataType.DOUBLE)

  /** the path related to the project root */
  type ProjectPath = RelPath
  case class ReferencePath(path: RelPath, isRelative: Boolean) {
    override def toString: String = {
      val prefix = if (isRelative) "relative" else "alias"
      s"$prefix'$path'"
    }
  }

  val returnSymbol = 'return
  val thisSymbol = 'this
  val superSymbol = 'super
  val defaultSymbol = 'default
  val undefinedSymbol = 'undefined
  val unknownSymbol = Symbol("<UNKNOWN>")

  val SM: SimpleMath.type = SimpleMath

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

  implicit class AssertionSyntax[T](x: T) {
    def which(p: T => Boolean): T = {
      assert(p(x))
      x
    }

  }

  type TyAnnot = Annot[GType]

  def combineOption[T](x: Option[T], y: Option[T]): Option[T] = {
    y match {
      case Some(v) => Some(v)
      case None    => x
    }
  }

  implicit class ChainingSyntax[A](x: A) {
    def pipe[B](f: A => B): B = f(x)

    def tap(f: A => Unit): A = {
      f(x)
      x
    }
  }

  def tryEach[A, B](x1: A, x2: A)(f: A => Option[B]): Option[B] = {
    f(x1).orElse(f(x2))
  }

  var shouldWarn = true
  def printWarning(str: String, mustWarn: Boolean = false): Unit = {
    if (mustWarn || shouldWarn)
      Console.err.println(warnStr("[warn] " + str))
  }

  def printInfo(a: Any): Unit = {
    println(infoStr(a.toString))
  }

  def printResult(a: Any): Unit = {
    println(resultStr(a.toString))
  }

  import Console.{RED, BLUE, GREEN, RESET}
  def warnStr(s: String) = s"$RED$s$RESET"

  def infoStr(s: String) = s"$BLUE$s$RESET"

  def resultStr(s: String) = s"$GREEN$s$RESET"

  def announced[A](actionName: String)(action: => A): A = {
    import SimpleMath.prettyPrintTime

    println(infoStr(s"  [start] $actionName started..."))
    val startTime = System.nanoTime()
    (try {
      action
    } catch {
      case e: Exception => throw e
    }).tap { _ =>
        val took = prettyPrintTime(System.nanoTime() - startTime, 2)
        println(infoStr(s"  [finish] $actionName finished. (took $took)"))
      }
  }

}
