import scala.language.implicitConversions

package object funcdiff extends APITrait {
  type IS[T] = IndexedSeq[T]
  val IS = IndexedSeq

  type Real = Double

  implicit def toPath(str: String): SymbolPath = {
    SymbolPath.empty / Symbol(str)
  }
}
