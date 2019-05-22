package lambdanet

object ImportStmt {

  case class ImportSingle(oldName: Symbol, relPath: ProjectPath, newName: Symbol)
      extends ImportStmt

  case class ImportDefault(path: ProjectPath, newName: Symbol) extends ImportStmt

  case class ImportModule(path: ProjectPath, newName: Symbol) extends ImportStmt

}

sealed trait ImportStmt
