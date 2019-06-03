package lambdanet

import lambdanet.surface.GStmt
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

  def groupInBlockSurface(stmts: Vector[GStmt]): surface.BlockStmt = {
    stmts match {
      case Vector(b: surface.BlockStmt) => b
      case _                            => surface.BlockStmt(stmts)
    }
  }

}
