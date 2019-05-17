package gtype.parsing

import ammonite.ops._
import funcdiff.SimpleMath
import gtype.GStmt.{TypeAnnotation, TypeHoleContext}
import gtype.parsing.Js._
import gtype.{ExportLevel, _}
import infer.IRTranslation

import scala.collection.mutable

/** Parsing Typescript into the surface language */
object ProgramParsing {

  def parseJson(text: String): Js.Val = {
    SimpleMath.withErrorMessage(s"JSON source text: $text") {
      fastparse.parse(text, JsonParsing.jsonExpr(_)).get.value
    }
  }

  def parseJsonFromFile(jsonFile: Path): Js.Val = {
    val text = read(jsonFile)
    fastparse.parse(text, JsonParsing.jsonExpr(_)).get.value
  }

  def asString(v: Js.Val): String = v.asInstanceOf[Str].value

  def asArray(v: Js.Val): List[Val] = v match {
    case Js.Null => List[Val]()
    case _       => v.asInstanceOf[Arr].value.toList
  }

  def asVector(v: Js.Val): Vector[Val] = v match {
    case Js.Null => Vector[Val]()
    case _       => v.asInstanceOf[Arr].value.toVector
  }

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
    value
      .asInstanceOf[Arr]
      .value
      .map {
        parseNamedValue
      }
      .toMap
  }

  def asBoolean(v: Js.Val): Boolean = {
    (v: @unchecked) match {
      case False => false
      case True  => true
    }
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
      case "FuncType" =>
        val fr = asArray(o("fro")).map(parseType)
        val to = parseType(o("to"))
        FuncType(fr, to)
      case "ObjectType" =>
        val fields = asArray(o("fields"))
          .map(pair => {
            val (k, v) = parseNamedValue(pair)
            (Symbol(k), parseType(v))
          })
          .toMap
        ObjectType(fields)
    }
    t
  }

  case class DeclarationModule(
      varDefs: Map[Symbol, GType],
      typeDefs: Map[Symbol, GType]
  )

  def extractDeclarationModule(module: GModule): DeclarationModule = {
    val varDefs: mutable.Map[Symbol, GType] = mutable.HashMap()
    val typeDefs: mutable.Map[Symbol, GType] = mutable.HashMap()

    module.stmts.foreach {
      case VarDef(x, t: GType, _: Const, _, ExportLevel.Private) =>
        varDefs(x) = t
      case f: FuncDef =>
        varDefs(f.name) = GStmt.extractSignature(f)
      case alias @ TypeAliasStmt(name, tyVars, ty, ExportLevel.Private) =>
        val rhs = try {
          IRTranslation.translateType(ty)(tyVars.toSet)
        } catch {
          case _: ClassCastException => throw new Error(s"Failed for $alias")
        }

        typeDefs(name) = typeDefs.get(name) match {
          case Some(o1: ObjectType) =>
            o1.merge(rhs.asInstanceOf[ObjectType])
          case Some(_) => throw new Error("Contradicting type aliases")
          case None    => rhs
        }
      case _: CommentStmt =>
      case other =>
        throw new Error(s"Illegal statement encountered in a declaration file: $other")
    }

    DeclarationModule(varDefs.toMap, typeDefs.toMap)
  }
}

import gtype.parsing.ProgramParsing._

class ProgramParsing(
    val tHoleContext: TypeHoleContext = new TypeHoleContext()
) {

  val marksToHoles: Boolean = false

  def parseContent(content: String): Vector[GStmt] = {
    SimpleMath.withErrorMessage(
      "failed when parsing content: \n" + content
    ) {
      val r = %%('node, "./parsingFromString.js", content)(pwd / 'scripts / 'ts)
      val json = ProgramParsing.parseJson(r.out.string).asInstanceOf[Js.Arr]
      json.value.toVector.map {
        parseGStmt
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
    modules.value.map(parseGModule)
  }

  def parseArgPair(value: Js.Val): (Symbol, GTMark) = {
    val (name, v) = parseNamedValue(value)
    val ty = parseGTMark(v)
    (Symbol(name), ty)
  }

  def parseArgList(value: Js.Val): List[(Symbol, GTMark)] = {
    val list = asArray(value)
    list.map(parseArgPair)
  }

  def parseGTMark(v: Js.Val): GTMark = {
    v match {
      case Null => tHoleContext.newTHole(None)
      case _ =>
        val ty = parseType(v)
        if (marksToHoles)
          tHoleContext.newTHole(Some(TypeAnnotation(ty, needInfer = true)))
        else ty
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

      case cat => throw new Error(s"Unhandled GExpr case: $cat")
    }
  }

  case class DefModifiers(
      isConst: Boolean,
      exportLevel: ExportLevel.Value,
      isGetter: Boolean,
      isSetter: Boolean,
      isAbstract: Boolean
  )

  def parseModifiers(v: Js.Val): DefModifiers = {
    val modifiers = asArray(v).map(asString).toSet
    val isConst = modifiers.contains("const")
    val exportLevel =
      if (modifiers.contains("export"))
        if (modifiers.contains("default")) ExportLevel.Default
        else ExportLevel.Public
      else ExportLevel.Private
    val isGetter = modifiers.contains("get")
    val isSetter = modifiers.contains("set")
    val isAbstract = modifiers.contains("abstract")
    DefModifiers(isConst, exportLevel, isGetter, isSetter, isAbstract)
  }

  def parseGStmt(v: Js.Val): GStmt =
    SimpleMath.withErrorMessage(s"Error when parsing $v") {
      val map = asObj(v)
      asString(map("category")) match {
        case "VarDef" =>
          val name = asString(map("x"))
          val t = parseGTMark(map("mark"))
          val init = parseGExpr(map("init"))
          val ms = parseModifiers(map("modifiers"))
          val b = asBoolean(map("isConst")) || ms.isConst
          VarDef(Symbol(name), t, init, isConst = b, ms.exportLevel)
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
            if (name == 'Constructor) GType.voidType
            else parseGTMark(map("returnType"))
          val body = parseGStmt(map("body"))

          val tyVars = asVector(map("tyVars")).map(asSymbol).toList
          val ms = parseModifiers(map("modifiers"))
          FuncDef(name, tyVars, args, returnType, body, ms.exportLevel)
        case "TypeAliasStmt" =>
          val name = Symbol(asString(map("name")))
          val tyVars = asVector(map("tyVars")).map(asSymbol).toList
          val ty = parseType(map("type"))
          val ms = parseModifiers(map("modifiers"))
          TypeAliasStmt(name, tyVars, ty, ms.exportLevel)
        case "ClassDef" =>
          val name = asSymbol(map("name"))
          val superType = asOptionSymbol(map("superType"))
          val ms = parseModifiers(map("modifiers"))
          val constructor = {
            val constructorValue = map("constructor")
            val f = if (constructorValue == Null) {
              // make an empty constructor
              val tyVars = List() //fixme
              FuncDef(
                GStmt.constructorName,
                tyVars,
                List(),
                GType.voidType,
                BlockStmt(Vector()),
                ExportLevel.Private
              )
            } else {
              parseGStmt(constructorValue).asInstanceOf[FuncDef]
            }
            f.copy(name = GStmt.constructorName)
          }
          val vars = asArray(map("vars")).map { v1 =>
            val List(named, b) = asArray(v1)
            val (name, mark) = parseNamedValue(named)
            val ty = parseGTMark(mark)
            val isStatic = asBoolean(b)
            (Symbol(name), (ty, isStatic))
          }
          val funcDefs =
            asVector(map("funcDefs")).map { v =>
              val List(s, b) = asArray(v)
              parseGStmt(s).asInstanceOf[FuncDef] -> asBoolean(b)
            }
          val tyVars = asVector(map("tyVars")).map(asSymbol).toList
          ClassDef(
            name,
            tyVars,
            superType,
            constructor,
            vars.toMap,
            funcDefs,
            ms.exportLevel,
            ms.isAbstract
          )

        case other => throw new Error(s"Unknown category: $other")
      }
    }

  def parseGModule(v: Js.Val): GModule = {
    val obj = asObj(v)
    val name = asString(obj("name"))
    var imports = Vector[ImportStmt]()
    var exports = Vector[ExportStmt]()
    var stmts = Vector[GStmt]()

    SimpleMath.withErrorMessage(s"Error when parsing module: $name") {
      asVector(obj("stmts")).foreach { s =>
        SimpleMath.withErrorMessage(s"when parsing $s") {
          s match {
            case ImportPattern(ss) =>
              imports ++= ss
            case ExportPattern(ss) =>
              exports ++= ss
            case other =>
              stmts :+= parseGStmt(other)
          }
        }
      }
    }

    val modulePath = RelPath(name.replace("." + RelPath(name).ext, ""))
    GModule(modulePath, imports, exports, stmts)
  }

  def main(args: Array[String]): Unit = {
    val jValue = parseJsonFromFile(
      pwd / up / 'DeepTypeTS / 'output / "foo.json"
    )
    //    println(jValue)

    asArray(jValue).map(parseGStmt).foreach(println)
  }
}
