package gtype.parsing

import ammonite.ops.RelPath
import gtype.GModule.ProjectPath
import gtype.{ExportStmt, ImportStmt}
import ImportStmt._
import fastparse.Parsed.{Failure, Success}
import gtype.parsing.ProgramParsing.{asObj, asString, asVector, asSymbol}

import fastparse._, JavaWhitespace._

object ImportPattern {
  def unapply(v: Js.Val): Option[Vector[ImportStmt]] = {
    val map = asObj(v)
    asString(map("category")) match {
      case "ImportStmt" =>
        val importString = StringContext.treatEscapes(asString(map("text")))
        Some(parseImports(importString))
      case _ => None
    }
  }

//  def spaceSep[_: P]: P[Unit] = P(CharsWhileIn(" \r\n", 1))

  def identifier[_: P]: P[String] = CharsWhileIn("a-zA-Z0-9$_").!

  def path[_: P]: P[ProjectPath] =
    P(JsonParsing.string | JsonParsing.singleQuoteString).map(s => RelPath(s.value))

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
      P("{" ~/ clause.rep(min = 1, sep = ",") ~ ",".? ~ "}" ~/ "from" ~ path).map {
        case (clauses, path) =>
          clauses.map {
            case (oldName, newNameOpt) =>
              val newName = Symbol(newNameOpt.getOrElse(oldName))
              ImportSingle(Symbol(oldName), path, newName)
          }.toVector
      }

    def importForEffects[_: P] = P(path).map(_ => Vector[ImportStmt]())

    def stmt[_: P]: P[Vector[ImportStmt]] =
      P(
        "import" ~/ (importSingles | importModule | importDefault | importForEffects) ~ (";" | End)
      )

    parse(importText, stmt(_)) match {
      case Success(value, _) => value
      case f: Failure =>
        throw new Error(
          s"Failed to parse import statement: '$importText', errors: ${f.trace().longMsg}"
        )
    }
  }
}

object ExportPattern {
  import gtype.ExportStmt._

  def unapply(v: Js.Val): Option[Vector[ExportStmt]] = {
    val map = asObj(v)
    asString(map("category")) match {
      case "ExportStmt" =>
        val str = StringContext.treatEscapes(asString(map("text")))
        Some(parseExports(str))
//      case "TypeAliasStmt" =>
//        val name = Symbol(asString(map("name")))
//        val tVars = asVector(map("tyVars")).map(asSymbol)
//        val `type` = ProgramParsing.parseType(map("type"))
//        Some(Vector(ExportTypeAlias(name, tVars, `type`)))
      case _ => None
    }
  }

  def parseExports(str: String): Vector[ExportStmt] = {
    import ImportPattern.{identifier, path}

    type Creator = Option[ProjectPath] => Vector[ExportStmt]

    def exportDefault[_: P]: P[Creator] =
      P("{" ~ "default" ~/ ("as" ~/ identifier).? ~ "}").map { newNameOpt => p =>
        Vector(ExportDefault(newNameOpt.map(Symbol.apply), p))
      }

    def exportDefault2[_: P]: P[Creator] =
      P("default" ~ identifier).map(n => p => Vector(ExportDefault(Some(Symbol(n)), p)))

    def exportSingles[_: P]: P[Creator] =
      P(
        "{" ~ (identifier ~/ ("as" ~/ identifier).?).rep(min = 1, sep = ",") ~ ",".? ~ "}"
      ).map { clauses => p =>
        clauses.toVector.map {
          case (oldName, newNameOpt) =>
            ExportSingle(Symbol(oldName), Symbol(newNameOpt.getOrElse(oldName)), p)
        }
      }

    def exportFromOther[_: P]: P[Creator] = {
      P("*").map(
        _ =>
          p => {
            Vector(ExportOtherModule(p.get))
          }
      )
    }

    def stmt[_: P] =
      P(
        "export" ~/ (exportFromOther | exportDefault | exportDefault2 | exportSingles)
          ~ ("from" ~/ path).? ~ ";".?
      ).map {
        case (creator, p) => creator(p)
      }

    parse(str, stmt(_)) match {
      case Success(value, _) => value
      case f: Failure =>
        throw new Error(
          s"Failed to parse export statement: '$str', errors: ${f.trace().longMsg}"
        )
    }
  }
}
