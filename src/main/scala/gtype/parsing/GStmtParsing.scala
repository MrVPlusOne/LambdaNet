package gtype.parsing

import ammonite.ops._
import fastparse.Parsed
import Js._
import gtype.GStmt.TypeHoleContext
import gtype._

object GStmtParsing {
  def parseAsJSon(jsonFile: Path): Js.Val = {
    val text = read(jsonFile)
    fastparse.parse(text, JsonParsing.jsonExpr(_)).get.value
  }

  def asString(v: Js.Val): String = v.asInstanceOf[Str].value

  def asArray(v: Js.Val): List[Val] = v.asInstanceOf[Arr].value.toList

  def asObj(v: Val): Map[String, Val] = v.asInstanceOf[Obj].value

  def parseBoolean(v: Js.Val): Boolean = {
    (v: @unchecked) match {
      case False => false
      case True  => true
    }
  }

  val tHoleContext = new TypeHoleContext()

  def parseType(v: Js.Val): GTMark = {
    val s = v.asInstanceOf[Str].value
    if (s == "missing") tHoleContext.newTHole(None)
    else {
      val tName = Symbol(s)
      val t =
        if (tName == AnyType.id) AnyType
        else TyVar(tName)
      tHoleContext.newTHole(Some(t))
    }
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
        val t = parseType(map("type"))
        val init = parseGExpr(map("init"))
        val b = parseBoolean(map("isConst"))
        VarDef(Symbol(name), t, init, isConst = b)
      case _ => ???
    }
  }

  def main(args: Array[String]): Unit = {
    val jValue = parseAsJSon(pwd / 'data / 'tests / "mergeSort.json")
    println(jValue)

    asArray(jValue).map(parseGStmt).foreach(println)
  }
}
