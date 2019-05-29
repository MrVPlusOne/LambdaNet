package lambdanet

import lambdanet.surface.GStmt
import lambdanet.translation.IR.{BlockStmt, IRStmt, namedVar}

package object translation {
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

  val thisVar: IR.Var = namedVar(thisSymbol)
  val superVar: IR.Var = namedVar(superSymbol)
  val returnVar: IR.Var = namedVar(lambdanet.returnSymbol)

}
