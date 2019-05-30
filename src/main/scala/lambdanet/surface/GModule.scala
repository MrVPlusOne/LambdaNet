package lambdanet.surface

import lambdanet._
import lambdanet.ProjectPath

/** Models a source file */
case class GModule(
    path: ProjectPath,
    imports: Vector[ImportStmt],
    exportStmts: Vector[ExportStmt],
    stmts: Vector[GStmt]
) {
  val moduleName: String = path.toString()
}
