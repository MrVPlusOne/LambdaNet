package lambdanet

import types._

package object surface {

  case class TypeAnnotation(ty: GType, needInfer: Boolean) {
    override def toString: String = {
      s"$ty${if (needInfer) "*" else ""}"
    }
  }
}
