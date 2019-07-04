package lambdanet

import lambdanet.Surface.GStmt
import lambdanet.translation.IR.{BlockStmt, IRStmt}
import lambdanet.translation.PredicateGraph.{PNode, PType}
import lambdanet.translation.QLang.QStmt

package object translation {
  type PAnnot = Annot[PType]

  def makeSureInBlock(stmts: Vector[IRStmt]): BlockStmt = {
    stmts match {
      case Vector(b: BlockStmt) => b
      case _                    => BlockStmt(stmts)
    }
  }

  def makeSureInBlockSurface(stmts: Vector[GStmt]): Surface.BlockStmt = {
    stmts match {
      case Vector(b: Surface.BlockStmt) => b
      case _                            => Surface.BlockStmt(stmts)
    }
  }

  def makeSureInBlockQ(stmt: QStmt): QLang.BlockStmt = {
    stmt match {
      case b: QLang.BlockStmt => b
      case _                  => QLang.BlockStmt(Vector(stmt))
    }
  }

}
