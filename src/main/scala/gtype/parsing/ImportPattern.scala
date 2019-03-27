package gtype.parsing

import ammonite.ops.RelPath
import gtype.GModule.ProjectPath
import gtype.ImportStmt
import ImportStmt._
import fastparse.Parsed.{Failure, Success}
import gtype.parsing.ProgramParsing.{asObj, asString}

object ImportPattern {
  def unapply(v: Js.Val): Option[Vector[ImportStmt]] = {
    val map = asObj(v)
    asString(map("category")) match {
      case "ImportStmt" =>
        val importString = asString(map("text")).replace("\\\"", "\"")
        Some(parseImports(importString))
      case _ => None
    }
  }

  import fastparse._, JavaWhitespace._

//  def spaceSep[_: P]: P[Unit] = P(CharsWhileIn(" \r\n", 1))

  def identifier[_: P]: P[String] = CharsWhileIn("a-zA-Z0-9$").!

  def path[_: P]: P[ProjectPath] = P(JsonParsing.string).map(s => RelPath(s.value))

  def parseImports(importText: String): Vector[ImportStmt] = {
    def importDefault[_: P] = P(identifier ~/ "from" ~ path).map {
      case (name, p) =>
        Vector(ImportDefault(p, Symbol(name)))
    }

    def importModule[_: P] = P("*" ~/ "as" ~ identifier ~/ "from" ~ path).map {
      case (name, p) =>
        Vector(ImportModule(p, Symbol(name)))
    }

    def clause[_: P]: P[(String, Option[String])] = P(identifier ~ ("as" ~/ identifier).?)

    def importSingles[_: P]: P[Vector[ImportSingle]] =
      P("{" ~/ clause.rep(min = 1, sep = ",") ~ "}" ~/ "from" ~ path).map {
        case (clauses, path) =>
          clauses.map {
            case (oldName, newNameOpt) =>
              val newName = Symbol(newNameOpt.getOrElse(oldName))
              ImportSingle(Symbol(oldName), path, newName)
          }.toVector
      }

    def importForEffects[_:P] = P(path).map(_ => Vector[ImportStmt]())

    def stmt[_: P]: P[Vector[ImportStmt]] =
      P("import" ~/ (importSingles | importModule | importDefault | importForEffects) ~ ";")

    parse(importText, stmt(_)) match {
      case Success(value, _) => value
      case f: Failure => throw new Error(s"Failed to parse import statement: '$importText', errors: ${f.trace().longMsg}")
    }
  }
}
