package gtype.parsing

import ammonite.ops._
import funcdiff.SimpleMath
import gtype.GStmt.{TypeAnnotation, TypeHoleContext}
import gtype.{ExportLevel, _}
import gtype.parsing.Js._

/** Parsing Typescript into the surface language */
object ProgramParsing {
  def parseContent(content: String): Vector[GStmt] = {
    SimpleMath.addMessagesForExceptions("failed when parsing content: \n" + content) {
      val r = %%('node, "./parsingFromString.js", content)(pwd / 'scripts / 'ts)
      val json = ProgramParsing.parseJson(r.out.string).asInstanceOf[Js.Arr]
      val parser = new ProgramParsing()
      json.value.toVector.map {
        parser.parseGStmt
      }
    }
  }

  def parseModulesFromFiles(
    srcFiles: Seq[RelPath],
    libraryFiles: Set[RelPath],
    projectRoot: Path,
    writeToFile: Option[Path] = None
  ): Seq[GModule] = {
    val totalLibraries = libraryFiles ++ srcFiles
    val r = %%(
      'node,
      pwd / RelPath("scripts/ts/parsingFromFile.js"),
      "--src",
      srcFiles.map(_.toString()),
      "--lib",
      totalLibraries.toList.map(_.toString())
    )(projectRoot)
    val parsedJson = r.out.string
    writeToFile.foreach(p => write.over(p, parsedJson))
    parseModulesFromJson(parsedJson)
  }

  def parseModulesFromJson(parsedJson: String): Seq[GModule] = {
    val modules = ProgramParsing.parseJson(parsedJson).asInstanceOf[Js.Arr]
    val parser = new ProgramParsing()
    modules.value.map(parser.parseGModule)
  }

  def parseJson(text: String): Js.Val = {
    SimpleMath.addMessagesForExceptions(s"JSON source text: $text") {
      fastparse.parse(text, JsonParsing.jsonExpr(_)).get.value
    }
  }

  def parseJsonFromFile(jsonFile: Path): Js.Val = {
    val text = read(jsonFile)
    fastparse.parse(text, JsonParsing.jsonExpr(_)).get.value
  }

  def asString(v: Js.Val): String = v.asInstanceOf[Str].value

  def asArray(v: Js.Val): List[Val] = v.asInstanceOf[Arr].value.toList

  def asVector(v: Js.Val): Vector[Val] = v.asInstanceOf[Arr].value.toVector

  def asNumber(v: Js.Val): Double = v.asInstanceOf[Num].value

  def asSymbol(v: Js.Val): Symbol = Symbol(asString(v))

  def asOptionSymbol(v: Js.Val): Option[Symbol] = v match {
    case Null => None
    case _    => Some(asSymbol(v))
  }

  def asObj(v: Val): Map[String, Val] = v.asInstanceOf[Obj].value

  def parseNamedValue(v: Js.Val): (String, Val) = {
    val p = asObj(v)
    asString(p("name")) -> p("value")
  }

  def arrayToMap(value: Js.Val): Map[String, Val] = {
    value.asInstanceOf[Arr].value.map { parseNamedValue }.toMap
  }

  def asBoolean(v: Js.Val): Boolean = {
    (v: @unchecked) match {
      case False => false
      case True  => true
    }
  }
}

import ProgramParsing._

class ProgramParsing(tHoleContext: TypeHoleContext = new TypeHoleContext()) {

  def parseArgPair(value: Js.Val): (Symbol, GTMark) = {
    val (name, v) = parseNamedValue(value)
    val ty = parseGTMark(v)
    (Symbol(name), ty)
  }

  def parseArgList(value: Js.Val): List[(Symbol, GTMark)] = {
    val list = asArray(value)
    list.map(parseArgPair)
  }

  def parseType(v: Js.Val): GType = {
    assert(
      v != Null,
      "Use parseGTMark instead if you are parsing an optional user type annotation."
    )
    val o = asObj(v)
    val t = asString(o("category")) match {
      case "TVar"    => TyVar(asSymbol(o("name")))
      case "AnyType" => AnyType
    }
    t
  }

  def parseGTMark(v: Js.Val): GTMark = {
    v match {
      case Null => tHoleContext.newTHole(None)
      case _    => parseType(v)
    }
  }

  def newTyHole(mark: Option[TypeAnnotation]): GTHole = {
    tHoleContext.newTHole(mark)
  }

  def parseGExpr(v: Js.Val): GExpr = {
    val map = asObj(v)
    asString(map("category")) match {
      case "FuncCall" =>
        val f = parseGExpr(map("f"))
        val args = asArray(map("args")).map(parseGExpr)
        FuncCall(f, args)

      case "Var" =>
        val name = asSymbol(map("name"))
        Var(name)
      case "Const" =>
        val ty = parseType(map("ty"))
        val value = asString(map("value"))
        Const(value, ty)
      case "ObjLiteral" =>
        val obj = arrayToMap(map("fields"))
        val objMap = obj.map { case (x, y) => (Symbol(x), parseGExpr(y)) }
        ObjLiteral(objMap)
      case "Access" =>
        val expr = parseGExpr(map("expr"))
        val field = asSymbol(map("field"))
        Access(expr, field)
      case "IfExpr" =>
        val cond = parseGExpr(map("cond"))
        val e1 = parseGExpr(map("e1"))
        val e2 = parseGExpr(map("e2"))
//        val resultType = parseType(map("resultType"))
        IfExpr(cond, e1, e2, newTyHole(None))

      case _ => throw new Error("Unhandled GExpr case.")
    }
  }

  val exportLevel: ExportLevel.Value = ExportLevel.Public


  def parseGStmt(v: Js.Val): GStmt = {
    val map = asObj(v)
    asString(map("category")) match {
      case "VarDef" =>
        val name = asString(map("x"))
        val t = parseGTMark(map("mark"))
        val init = parseGExpr(map("init"))
        val b = asBoolean(map("isConst"))
        VarDef(Symbol(name), t, init, isConst = b, exportLevel)
      case "AssignStmt" =>
        val lhs = parseGExpr(map("lhs"))
        val rhs = parseGExpr(map("rhs"))
        AssignStmt(lhs, rhs)
      case "ExprStmt" =>
        val e = parseGExpr(map("expr"))
        val isReturn = asBoolean(map("isReturn"))
        ExprStmt(e, isReturn)
      case "IfStmt" =>
        val cond = parseGExpr(map("cond"))
        val branch1 = parseGStmt(map("branch1"))
        val branch2 = parseGStmt(map("branch2"))
        IfStmt(cond, branch1, branch2)
      case "WhileStmt" =>
        val cond = parseGExpr(map("cond"))
        val body = parseGStmt(map("body"))
        WhileStmt(cond, body)
      case "CommentStmt" =>
        val text = asString(map("text"))
        CommentStmt(text)
      case "BlockStmt" =>
        val stmts = asVector(map("stmts")).map(parseGStmt)
        BlockStmt(stmts)
      case "FuncDef" =>
        val name = Symbol(asString(map("name")))
        val args = parseArgList(map("args"))
        val returnType =
          if (name == 'Constructor) GType.voidType else parseGTMark(map("returnType"))
        val body = parseGStmt(map("body"))
        val tyVars = List() //fixme
        FuncDef(name, tyVars, args, returnType, body, exportLevel)
      case "ClassDef" =>
        val name = asSymbol(map("name"))
        val superType = asOptionSymbol(map("superType"))
        val constructor = {
          val consName = ClassDef.constructorName(name)
          val constructorValue = map("constructor")
          val f = if (constructorValue == Null) {
            // make an empty constructor
            val tyVars = List() //fixme
            FuncDef(
              consName,
              tyVars,
              List(),
              GType.voidType,
              BlockStmt(Vector()),
              exportLevel
            )
          } else {
            parseGStmt(constructorValue).asInstanceOf[FuncDef]
          }
          f.copy(name = consName)
        }
        val vars = parseArgList(map("vars"))
        val funcDefs =
          asVector(map("funcDefs")).map(x => parseGStmt(x).asInstanceOf[FuncDef])
        val tyVars = List() //fixme
        ClassDef(name, tyVars, superType, constructor, vars.toMap, funcDefs, exportLevel)

      case other => throw new Error(s"Unknown category: $other")
    }
  }

  def parseGModule(v: Js.Val): GModule = {
    val obj = asObj(v)
    val name = asString(obj("name"))
    var imports = Vector[ImportStmt]()
    var exports = Vector[ExportStmt]()
    var stmts = Vector[GStmt]()

    asVector(obj("stmts")).foreach {
      case ImportPattern(ss) =>
        imports ++= ss
      case ExportPattern(ss) =>
        exports ++= ss
      case other =>
        stmts :+= parseGStmt(other)
    }

    val modulePath = RelPath(name.replace("." + RelPath(name).ext, ""))
    GModule(modulePath, imports, exports, stmts)
  }

  def main(args: Array[String]): Unit = {
    val jValue = parseJsonFromFile(pwd / up / 'DeepTypeTS / 'output / "foo.json")
//    println(jValue)

    asArray(jValue).map(parseGStmt).foreach(println)
  }
}
