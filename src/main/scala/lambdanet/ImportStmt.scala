package lambdanet

object ImportStmt {

  case class ImportSingle(
      oldName: Symbol,
      path: ProjectPath,
      newName: Symbol
  ) extends ImportStmt

  case class ImportDefault(path: ProjectPath, newName: Symbol)
      extends ImportStmt

  case class ImportModule(path: ProjectPath, newName: Symbol) extends ImportStmt

}

sealed trait ImportStmt {
  val path: ProjectPath
}

object ExportStmt {

  case class ExportDefault(newName: Option[Symbol], from: Option[ProjectPath])
      extends ExportStmt

  case class ExportSingle(
      oldName: Symbol,
      newName: Symbol,
      from: Option[ProjectPath]
  ) extends ExportStmt

  //  case class ExportTypeAlias(name: Symbol, tVars: Vector[Symbol], `type`: GType)
  //      extends ExportStmt
  case class ExportOtherModule(from: ProjectPath) extends ExportStmt

}

sealed trait ExportStmt

//noinspection TypeAnnotation
object ExportLevel extends Enumeration {

  /** Corresponds to an exported Typescript definition */
  val Public = Value

  /** Corresponds to a not exported Typescript definition */
  val Unspecified = Value

  /** Corresponds to 'default export' in typescript */
  val Default = Value

  def asPrefix(level: ExportLevel.Value): String = level match {
    case Public      => "export "
    case Unspecified => ""
    case Default     => "export default "
  }
}
