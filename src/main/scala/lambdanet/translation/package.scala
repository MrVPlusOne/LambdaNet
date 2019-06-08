package lambdanet

import lambdanet.Surface.GStmt
import lambdanet.translation.IR.{BlockStmt, IRStmt}
import lambdanet.translation.PredicateGraph.{PNode, PType}

package object translation {
  type PAnnot = Annot[PType]

  def groupInBlock(stmts: Vector[IRStmt]): BlockStmt = {
    stmts match {
      case Vector(b: BlockStmt) => b
      case _                    => BlockStmt(stmts)
    }
  }

  def groupInBlockSurface(stmts: Vector[GStmt]): Surface.BlockStmt = {
    stmts match {
      case Vector(b: Surface.BlockStmt) => b
      case _                            => Surface.BlockStmt(stmts)
    }
  }

}
