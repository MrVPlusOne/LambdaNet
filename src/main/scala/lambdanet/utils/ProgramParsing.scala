package lambdanet.utils

import ammonite.ops._
import funcdiff.SimpleMath
import lambdanet._
import lambdanet.utils.Js._
import lambdanet.surface._
import lambdanet.types._
import lambdanet.translation.OldIRTranslation
import ProgramParsing.GTMark
import SimpleMath.Extensions._
import lambdanet.translation.groupInBlockSurface

import scala.collection.mutable

/** Parsing Typescript into the surface language */
object ProgramParsing {
  type GTMark = TyAnnot

  case class DeclarationModule(
      varDefs: Map[Symbol, GType],
      typeDefs: Map[Symbol, GType],
      namespaces: Map[Symbol, DeclarationModule]
  )

  def extractDeclarationModule(stmts: Vector[GStmt]): DeclarationModule = {
    val varDefs: mutable.Map[Symbol, GType] = mutable.HashMap()
    val typeDefs: mutable.Map[Symbol, GType] = mutable.HashMap()
    val namespaces: mutable.Map[Symbol, DeclarationModule] = mutable.HashMap()

    stmts.foreach {
      case VarDef(
          x,
          Annot.WithContent(t),
          _: Const,
          _,
          ExportLevel.Unspecified
          ) =>
        varDefs(x) = t
      case f: FuncDef =>
        varDefs(f.name) = GStmt.extractSignature(f)
      case alias @ TypeAliasStmt(name, tyVars, ty, ExportLevel.Unspecified) =>
        val rhs = try {
          OldIRTranslation.translateType(ty)(tyVars.toSet)
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
      case ns: Namespace =>
        namespaces(ns.name) = extractDeclarationModule(ns.block.stmts)
      case other =>
        throw new Error(
          s"Illegal statement encountered in a declaration file: $other"
        )
    }

    DeclarationModule(varDefs.toMap, typeDefs.toMap, namespaces.toMap)
  }

  def parseGModulesFromRoot(root: Path) = {
    val sources = ls
      .rec(root)
      .filter { f =>
        if (f.last.endsWith(".d.ts")) {
          throw new Error(
            s".d.ts file encountered: $f, you are probably " +
              s"parsing the wrong files."
          )
        }
        f.ext == "ts"
      }
      .map(_.relativeTo(root))
    val parser = ProgramParsing
    parser.parseGModulesFromFiles(
      sources,
      root
    )
  }

  def parseContent(content: String): Vector[GStmt] = {
    SimpleMath.withErrorMessage(
      "failed when parsing content: \n" + content
    ) {
      val r = %%('node, "./parsingFromString.js", content)(pwd / 'scripts / 'ts)
      val json = ProgramParsing.parseJson(r.out.string).asInstanceOf[Js.Arr]
      json.value.toVector.flatMap {
        parseGStmt
      }
    }
  }

  def parseJson(text: String): Js.Val = {
    SimpleMath.withErrorMessage(s"JSON source text: $text") {
      fastparse.parse(text, JsonParsing.jsonExpr(_)).get.value
    }
  }

  def parseJsonFromFile(jsonFile: Path): Js.Val = {
    val text = read(jsonFile)
    fastparse.parse(text, JsonParsing.jsonExpr(_)).get.value
  }

  /**
    * only srcFiles get parsed into [[GModule]]s, libraryFiles are simply provided to
    * the compiler for parsing purposes. Both kinds of paths are relative to the project
    * root.
    */
  def parseGModulesFromFiles(
      srcFiles: Seq[RelPath],
      projectRoot: Path,
      writeToFile: Option[Path] = None
  ): Vector[GModule] = {
    val r = %%(
      'node,
      pwd / RelPath("scripts/ts/parsingFromFile.js"),
      "--src",
      srcFiles.map(_.toString()),
      "--lib",
      srcFiles.toList.map(_.toString())
    )(projectRoot)
    val parsedJson = r.out.string
    writeToFile.foreach(p => write.over(p, parsedJson))
    parseGModulesFromJson(parsedJson)
  }

  /** Parses a sequence of [[GModule]] from Json string. These strings can be
    * generated through [[parseGModulesFromFiles]] when writeToFile is set to none-empty. */
  def parseGModulesFromJson(parsedJson: String): Vector[GModule] = {
    val modules = ProgramParsing.parseJson(parsedJson).asInstanceOf[Js.Arr]
    modules.value.map(parseGModule).toVector
  }

  private def asString(v: Js.Val): String = v.asInstanceOf[Str].value

  private def asArray(v: Js.Val): List[Val] = v match {
    case Js.Null => List[Val]()
    case _       => v.asInstanceOf[Arr].value.toList
  }

  private def asVector(v: Js.Val): Vector[Val] = v match {
    case Js.Null => Vector[Val]()
    case _       => v.asInstanceOf[Arr].value.toVector
  }

  private def asNumber(v: Js.Val): Double = v.asInstanceOf[Num].value

  private def asSymbol(v: Js.Val): Symbol = Symbol(asString(v))

  private def asOptionSymbol(v: Js.Val): Option[Symbol] = v match {
    case Null => None
    case _    => Some(asSymbol(v))
  }

  private def asObj(v: Val): Map[String, Val] = v.asInstanceOf[Obj].value

  private def parseNamedValue(v: Js.Val): (String, Val) = {
    val p = asObj(v)
    asString(p("name")) -> p("value")
  }

  private def arrayToMap(value: Js.Val): Map[String, Val] = {
    value
      .asInstanceOf[Arr]
      .value
      .map {
        parseNamedValue
      }
      .toMap
  }

  private def asBoolean(v: Js.Val): Boolean = {
    (v: @unchecked) match {
      case False => false
      case True  => true
    }
  }

  private def parseType(v: Js.Val): GType = {
    assert(
      v != Null,
      "Use parseGTMark instead if you are parsing an optional user type annotation."
    )
    val o = asObj(v)
    val t = asString(o("category")) match {
      case "TVar" =>
        val n = asSymbol(o("name"))
        if (n == AnyType.id) AnyType else TyVar(n)
      case "AnyType" => AnyType
      case "FuncType" =>
        val fr = asArray(o("args")).map(parseType)
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

  private def parseArgPair(value: Js.Val): (Symbol, GTMark) = {
    val (name, v) = parseNamedValue(value)
    val ty = parseGTMark(v)
    (Symbol(name), ty)
  }

  private def parseArgList(value: Js.Val): Vector[(Symbol, GTMark)] = {
    val list = asVector(value)
    list.map(parseArgPair)
  }

  private def parseGTMark(v: Js.Val): GTMark = {
    v match {
      case Null => Annot.Missing
      case _    => Annot.User(parseType(v))
    }
  }

  private def parseGExpr(v: Js.Val): GExpr = {
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
        val ty = parseType(map("ty")).asInstanceOf[GroundType]
        val value = asString(map("value"))
        val c = Const(value, ty)
        c.line = asNumber(map("line")).toInt
        c
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
        IfExpr(cond, e1, e2)

      case cat => throw new Error(s"Unhandled GExpr case: $cat")
    }
  }

  private case class DefModifiers(
      isConst: Boolean,
      exportLevel: ExportLevel.Value,
      isGetter: Boolean,
      isSetter: Boolean,
      isAbstract: Boolean
  )

  private def parseModifiers(v: Js.Val): DefModifiers = {
    val modifiers = asArray(v).map(asString).toSet
    val isConst = modifiers.contains("const")
    val exportLevel =
      if (modifiers.contains("export"))
        if (modifiers.contains("default")) ExportLevel.Default
        else ExportLevel.Public
      else ExportLevel.Unspecified
    val isGetter = modifiers.contains("get")
    val isSetter = modifiers.contains("set")
    val isAbstract = modifiers.contains("abstract")
    DefModifiers(isConst, exportLevel, isGetter, isSetter, isAbstract)
  }

  private def parseGStmt(v: Js.Val): Vector[GStmt] =
    SimpleMath.withErrorMessage(s"Error when parsing $v") {
      val map = asObj(v)
      asString(map("category")) match {
        case "VarDef" =>
          val name = asString(map("x"))
          val t = parseGTMark(map("mark"))
          val init = parseGExpr(map("init"))
          val ms = parseModifiers(map("modifiers"))
          val b = asBoolean(map("isConst")) || ms.isConst
          Vector(VarDef(Symbol(name), t, init, isConst = b, ms.exportLevel))
        case "AssignStmt" =>
          val lhs = parseGExpr(map("lhs"))
          val rhs = parseGExpr(map("rhs"))
          Vector(AssignStmt(lhs, rhs))
        case "ExprStmt" =>
          val e = parseGExpr(map("expr"))
          val isReturn = asBoolean(map("isReturn"))
          Vector(ExprStmt(e, isReturn))
        case "IfStmt" =>
          val cond = parseGExpr(map("cond"))
          val branch1 = parseGStmt(map("branch1"))
          val branch2 = parseGStmt(map("branch2"))
          Vector(
            IfStmt(
              cond,
              groupInBlockSurface(branch1),
              groupInBlockSurface(branch2)
            )
          )
        case "WhileStmt" =>
          val cond = parseGExpr(map("cond"))
          val body = parseGStmt(map("body"))
          Vector(WhileStmt(cond, groupInBlockSurface(body)))
        case "CommentStmt" =>
          val text = asString(map("text"))
          Vector(CommentStmt(text))
        case "BlockStmt" =>
          val stmts = asVector(map("stmts")).flatMap(parseGStmt)
          Vector(BlockStmt(stmts))
        case "NamespaceStmt" =>
          val name = asString(map("name"))
          val body = groupInBlockSurface(parseGStmt(map("block")))
          val ms = parseModifiers(map("modifiers"))
          Vector(Namespace(Symbol(name), body, ms.exportLevel))
        case "FuncDef" =>
          val name = Symbol(asString(map("name")))
          val args = parseArgList(map("args"))
          val returnType =
            if (name == 'Constructor) Annot.Missing
            else parseGTMark(map("returnType"))
          val body = groupInBlockSurface(parseGStmt(map("body")))

          val tyVars = asVector(map("tyVars")).map(asSymbol)
          val ms = parseModifiers(map("modifiers"))
          Vector(FuncDef(name, tyVars, args, returnType, body, ms.exportLevel))
        case "TypeAliasStmt" =>
          val name = Symbol(asString(map("name")))
          val tyVars = asVector(map("tyVars")).map(asSymbol)
          val ty = parseType(map("type"))
          val ms = parseModifiers(map("modifiers"))
          Vector(TypeAliasStmt(name, tyVars, ty, ms.exportLevel))
        case "ClassDef" =>
          val name = asSymbol(map("name"))
          val superType = asOptionSymbol(map("superType"))
          val ms = parseModifiers(map("modifiers"))
          val tyVars = asVector(map("tyVars")).map(asSymbol)
          val vars = asVector(map("vars")).map { v1 =>
            val (name, v2) = parseNamedValue(v1)
            val List(tyV, initV, isStaticV) = asArray(v2)
            val ty = parseGTMark(tyV)
            val init = parseGExpr(initV)
            val isStatic = asBoolean(isStaticV)
            (Symbol(name), (ty, init, isStatic))
          }
          val funcDefs =
            asVector(map("funcDefs")).map { v =>
              val List(s, b) = asArray(v)
              parseGStmt(s).asInstanceOf[Vector[FuncDef]].head -> asBoolean(b)
            }

          val instanceInits = mutable.HashMap[Symbol, GExpr]()

          val (instanceVars, staticVars) = {
            val v1 = vars.groupBy(_._2._3)
            (
              v1.getOrElse(false, Map()).map {
                case (s, (mark, expr, _)) =>
                  instanceInits(s) = expr
                  s -> mark
              },
              v1.getOrElse(true, Map()).map(p => p._1 -> (p._2._1, p._2._2))
            )
          }
          val (instanceMethods, staticMethods0) = {
            val v1 = funcDefs.groupBy(_._2)
            (
              v1.getOrElse(false, Vector()).map(_._1),
              v1.getOrElse(true, Vector()).map(_._1)
            )
          }

          val constructor0 = {
            val constructorValue = map("constructor")
            val f = if (constructorValue == Null) {
              // make an empty constructor
              val tyVars = Vector()
              FuncDef(
                GStmt.constructorName,
                tyVars,
                Vector(),
                Annot.Missing,
                BlockStmt(Vector()),
                ExportLevel.Unspecified
              )
            } else {
              parseGStmt(constructorValue).asInstanceOf[Vector[FuncDef]].head
            }
            f.copy(
              name = GStmt.constructorName,
              returnType = Annot.Fixed(TyVar(name)),
              tyVars = tyVars // constructor has the same tyVars as the class
            )
          }
          //put instance var instantiation into the constructor
          val constructor = {
            val thisDef = VarDef(
              thisSymbol,
              Annot.Fixed(TyVar(name)),
              Var(undefinedSymbol),
              isConst = true,
              ExportLevel.Unspecified
            )
            val thisSuperVars = Vector(
              thisDef,
              thisDef.copy(name = superSymbol)
            )
            val lambdas = asVector(map("initLambdas"))
              .flatMap(parseGStmt)
              .asInstanceOf[Vector[FuncDef]]
            val stmts = groupInBlockSurface(Vector(constructor0.body)).stmts
            val inits = instanceInits.toVector.map {
              case (s, expr) =>
                AssignStmt(Access(Var(thisSymbol), s), expr)
            }
            constructor0.copy(
              body =
                groupInBlockSurface(thisSuperVars ++ lambdas ++ inits ++ stmts)
            )
          }

          val isAbstract = ms.isAbstract
          val staticMethods =
            if (isAbstract) staticMethods0 else constructor +: staticMethods0

          val staticVarIsConst = true
          val staticMembers = staticVars.map {
            case (vn, (ty, init)) =>
              VarDef(vn, ty, init, staticVarIsConst, ms.exportLevel)
          }.toVector ++ staticMethods

          Vector(
            // Wrap all static methods into a namespace,
            Namespace(name, BlockStmt(staticMembers), ms.exportLevel),
            ClassDef(
              name,
              tyVars,
              superType,
              instanceVars.toMap,
              instanceMethods,
              ms.exportLevel
            )
          )

        case other => throw new Error(s"Unknown category: $other")
      }
    }

  private def parseGModule(v: Js.Val): GModule = {
    val obj = asObj(v)
    val name = asString(obj("name"))
    var imports = Vector[ImportStmt]()
    var exports = Vector[ExportStmt]()
    var stmts = Vector[GStmt]()

    val aliases = mutable.HashMap[Symbol, TypeAliasStmt]()

    SimpleMath.withErrorMessage(s"Error when parsing module: $name") {
      asVector(obj("stmts")).foreach { s =>
        SimpleMath.withErrorMessage(s"when parsing $s") {
          s match {
            case ImportPattern(ss) =>
              imports ++= ss
            case ExportPattern(ss) =>
              exports ++= ss
            case a: TypeAliasStmt if !aliases.contains(a.name) =>
              aliases(a.name) = a
            case a: TypeAliasStmt =>
              // merge interfaces
              val o1 = aliases(a.name).ty.asInstanceOf[ObjectType]
              val o2 = a.ty.asInstanceOf[ObjectType]
              aliases(a.name) =
                TypeAliasStmt(a.name, a.tyVars, o1.merge(o2), a.exportLevel)
            case other =>
              stmts ++= parseGStmt(other)
          }
        }
      }
    }
    val stmts1 = aliases.values.toVector ++ stmts

    val modulePath = RelPath(name.replace("." + RelPath(name).ext, ""))
    GModule(modulePath, imports, exports, stmts1)
  }

  import fastparse.JavaWhitespace._
  import fastparse.Parsed.{Failure, Success}
  import fastparse._
  import lambdanet.ImportStmt._

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
      P(JsonParsing.string | JsonParsing.singleQuoteString)
        .map(s => RelPath(s.value))

    def parseImports(importText: String): Vector[ImportStmt] = {
      def importDefault[_: P] = P(identifier ~/ "from" ~ path).map {
        case (name, p) =>
          Vector(ImportDefault(p, Symbol(name)))
      }

      def importModule[_: P] =
        P("*" ~/ "as" ~ identifier ~/ "from" ~ path).map {
          case (name, p) =>
            Vector(ImportModule(p, Symbol(name)))
        }

      def clause[_: P]: P[(String, Option[String])] =
        P(identifier ~ ("as" ~/ identifier).?)

      def importSingles[_: P]: P[Vector[ImportSingle]] =
        P("{" ~/ clause.rep(min = 1, sep = ",") ~ ",".? ~ "}" ~/ "from" ~ path)
          .map {
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
    import lambdanet.ExportStmt._

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
        P("{" ~ "default" ~/ ("as" ~/ identifier).? ~ "}").map {
          newNameOpt => p =>
            Vector(ExportDefault(newNameOpt.map(Symbol.apply), p))
        }

      def exportDefault2[_: P]: P[Creator] =
        P("default" ~ identifier)
          .map(n => p => Vector(ExportDefault(Some(Symbol(n)), p)))

      def exportSingles[_: P]: P[Creator] =
        P(
          "{" ~ (identifier ~/ ("as" ~/ identifier).?)
            .rep(min = 1, sep = ",") ~ ",".? ~ "}"
        ).map { clauses => p =>
          clauses.toVector.map {
            case (oldName, newNameOpt) =>
              ExportSingle(
                Symbol(oldName),
                Symbol(newNameOpt.getOrElse(oldName)),
                p
              )
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

  def main(args: Array[String]): Unit = {
    val jValue = parseJsonFromFile(
      pwd / up / 'DeepTypeTS / 'output / "foo.json"
    )
    //    println(jValue)

    asArray(jValue).map(parseGStmt).foreach(println)
  }
}
