package lambdanet

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
