package lambdanet

object ExportStmt {

  case class ExportDefault(newName: Option[Symbol], from: Option[ProjectPath])
      extends ExportStmt

  case class ExportSingle(oldName: Symbol, newName: Symbol, from: Option[ProjectPath])
      extends ExportStmt

  //  case class ExportTypeAlias(name: Symbol, tVars: Vector[Symbol], `type`: GType)
  //      extends ExportStmt
  case class ExportOtherModule(from: ProjectPath) extends ExportStmt

}

sealed trait ExportStmt
