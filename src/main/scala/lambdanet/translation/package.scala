package lambdanet

import lambdanet.translation.IR.{BlockStmt, IRStmt, namedVar}

package object translation {
  def groupInBlock(stmts: Vector[IRStmt]): BlockStmt = {
    stmts match {
      case Vector(b: BlockStmt) => b
      case _                    => BlockStmt(stmts)
    }
  }

  val thisVar: IR.Var = namedVar(thisSymbol)
  val superVar: IR.Var = namedVar(superSymbol)
}
