package lambdanet

object ImportStmt {

  case class ImportSingle(
      oldName: Symbol,
      path: ReferencePath,
      newName: Symbol
  ) extends ImportStmt

  case class ImportDefault(path: ReferencePath, newName: Symbol)
      extends ImportStmt

  case class ImportModule(path: ReferencePath, newName: Symbol)
      extends ImportStmt

}

sealed trait ImportStmt {
  val path: ReferencePath
}

object ExportStmt {

  case class ExportDefault(newName: Option[Symbol], from: Option[ReferencePath])
      extends ExportStmt

  case class ExportSingle(
      oldName: Symbol,
      newName: Symbol,
      from: Option[ReferencePath]
  ) extends ExportStmt

  //  case class ExportTypeAlias(name: Symbol, tVars: Vector[Symbol], `type`: GType)
  //      extends ExportStmt
  case class ExportOtherModule(from: ReferencePath) extends ExportStmt

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
