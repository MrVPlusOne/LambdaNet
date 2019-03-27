package gtype

import gtype.GModule.ProjectPath

/** Models a source file */
case class GModule(
  path: ProjectPath,
  imports: Vector[ImportStmt],
  stmts: Vector[GStmt]
) {
  val moduleName: String = path.segments.last
}

object GModule {
  import ammonite.ops.RelPath
  type ProjectPath = RelPath
}

//noinspection TypeAnnotation
object ExportLevel extends Enumeration {

  /** Corresponds to an exported Typescript definition */
  val Public = Value

  /** Corresponds to a not exported Typescript definition */
  val Private = Value

  /** Corresponds to 'default export' in typescript */
  val MainExport = Value

  def asPrefix(level: ExportLevel.Value): String = level match {
    case Public     => "export "
    case Private    => ""
    case MainExport => "export default "
  }
}

sealed trait ImportStmt

object ImportStmt{
  case class ImportSingle(oldName: Symbol, relPath: ProjectPath, newName: Symbol)
    extends ImportStmt

  case class ImportDefault(path: ProjectPath, newName: Symbol) extends ImportStmt
  case class ImportModule(path: ProjectPath, newName: Symbol) extends ImportStmt
}

