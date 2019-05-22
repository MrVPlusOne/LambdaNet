package lambdanet

import lambdanet.translation.IR.{BlockStmt, IRStmt}

package object translation {
  def groupInBlock(stmts: Vector[IRStmt]): BlockStmt = {
    stmts match {
      case Vector(b: BlockStmt) => b
      case _                    => BlockStmt(stmts)
    }
  }
}
