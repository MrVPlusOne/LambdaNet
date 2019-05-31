package lambdanet

import lambdanet.surface.GStmt
import lambdanet.translation.IR.{BlockStmt, IRStmt, namedVar}
import lambdanet.translation.PLang.PStmt
import lambdanet.translation.PredicateGraph.{PNode, PType}

package object translation {
  type PAnnot = Annot[PType]

  def groupInBlock(stmts: Vector[IRStmt]): BlockStmt = {
    stmts match {
      case Vector(b: BlockStmt) => b
      case _                    => BlockStmt(stmts)
    }
  }

  def groupInBlockPLang(stmts: Vector[PStmt]): PLang.BlockStmt = {
    stmts match {
      case Vector(b: PLang.BlockStmt) => b
      case _                          => PLang.BlockStmt(stmts)
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
