package lambdanet.surface

import lambdanet.ProjectPath

/** Models a source file */
case class GModule(
    path: ProjectPath,
    stmts: Vector[GStmt]
) {
  val moduleName: String = path.toString()
}
