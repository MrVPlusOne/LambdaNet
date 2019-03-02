package gtype.parsing

import ammonite.ops._
import gtype.GStmt.{TypeAnnotation, TypeHoleContext}
import gtype._
import gtype.parsing.Js._

object GStmtParsing{
  def parseContent(content: String): Vector[GStmt] = {
    val r = %%('node, "./test-cases/parsingService.js", content)(pwd / 'scripts / 'ts)
    val json = GStmtParsing.parseJson(r.out.string).asInstanceOf[Js.Arr]
    val parser = new GStmtParsing()
    json.value.toVector.map {
      parser.parseGStmt
    }
  }

  def parseJson(text: String): Js.Val = {
    try {
      fastparse.parse(text, JsonParsing.jsonExpr(_)).get.value
    } catch {
      case e: Exception =>
        System.err.println(s"source text: $text")
        throw e
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

  def arrayToMap(value: Js.Val): Map[String, Val] = {
    value.asInstanceOf[Arr].value.map{ v =>
      val p = asObj(v)
      asString(p("name")) -> p("expr")
    }.toMap
  }

  def asBoolean(v: Js.Val): Boolean = {
    (v: @unchecked) match {
      case False => false
      case True => true
    }
  }
}

import GStmtParsing._

class GStmtParsing(tHoleContext: TypeHoleContext = new TypeHoleContext()) {

  def parseClassVars(v: Js.Val): Map[Symbol, GTMark] = {
    val obj = asObj(v)
    obj.map {
      case (x, y) => (Symbol(x), parseType(y))
    }
  }

  def parseArgPair(value: Js.Val): (Symbol, GTMark) = {
    val list = asArray(value)
    val name = Symbol(asString(list.head))
    val ty = parseType(list.tail.head)
    (name, ty)
  }

  def parseArgList(value: Js.Val): List[(Symbol, GTMark)] = {
    val list = asArray(value)
    list.map(parseArgPair)
  }

  def parseType(v: Js.Val): GType = {
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
      case _ => parseType(v)
    }
  }

  def newTyHole(mark: Option[TypeAnnotation]): GTHole = {
    tHoleContext.newTHole(mark)
  }

  /*
   *  e :=                         ([[GExpr]])
   *     | x                       ([[Var]])
   *     | c                       ([[Const]])
   *     | e(e,...,e)              ([[FuncCall]])
   *     | e as t                  ([[Cast]])
   *     | { l: e, ..., l: e }     ([[ObjLiteral]])
   *     | e.l                     ([[Access]])
   *     | if[α] e then e else e   ([[IfExpr]])
   */
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

      case _ => ???
    }
  }

  /*
   * S :=                                    ([[GStmt]])
   *       var x: α = e                      ([[VarDef]])
   *       e := e                            ([[AssignStmt]])
   *       [return] e                        ([[ExprStmt]])
   *       if e then S else S                ([[IfStmt]])
   *       while e do S                      ([[WhileStmt]])
   * B in  { S; ...; S }                     ([[BlockStmt]])
   * f in  function x (x: α, ..., x:α): α B  ([[FuncDef]])
   *       class x (l: α, ..., l:α)          ([[ClassDef]])
   *       ↳ [extends x]{ f, ..., f }
   */
  def parseGStmt(v: Js.Val): GStmt = {
    val map = asObj(v)
    asString(map("category")) match {
      case "VarDef" =>
        val name = asString(map("x"))
        val t = parseGTMark(map("mark"))
        val init = parseGExpr(map("init"))
        val b = asBoolean(map("isConst"))
        VarDef(Symbol(name), t, init, isConst = b)
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
      case "BlockStmt" =>
        val stmts = asVector(map("stmts")).map(parseGStmt)
        BlockStmt(stmts)
      case "FuncDef" =>
        val name = Symbol(asString(map("name")))
        val args = parseArgList(map("args"))
        val returnType = parseType(map("returnType"))
        val body = parseGStmt(map("body"))
        FuncDef(name, args, returnType, body)
      case "ClassDef" =>
        val name = asSymbol(map("name"))
        val superType = asOptionSymbol(map("superType"))
        val constructor = parseGStmt(map("constructor")).asInstanceOf[FuncDef]
        val vars = parseClassVars(map("vars"))
        val funcDefs = asVector(map("funcDefs")).map(x => x.asInstanceOf[FuncDef])
        ClassDef(name, superType, constructor, vars, funcDefs)

      case _ => ???
    }
  }

  def main(args: Array[String]): Unit = {
    val jValue = parseJsonFromFile(pwd / up / 'DeepTypeTS / 'output / "foo.json")
//    println(jValue)

    asArray(jValue).map(parseGStmt).foreach(println)
  }
}
