package gtype.parsing

import ammonite.ops._
import gtype.GStmt.TypeHoleContext
import gtype._
import gtype.parsing.Js._

object GStmtParsing {
  def parseAsJSon(jsonFile: Path): Js.Val = {
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
    case _ => Some(asSymbol(v))
  }

  def asObj(v: Val): Map[String, Val] = v.asInstanceOf[Obj].value

  def parseClassVars(v: Js.Val): Map[Symbol, GTMark] = {
    val obj = asObj(v)
    obj.map {
      case (x, y) => (Symbol(x), parseType(y))
    }
  }

  def parseBoolean(v: Js.Val): Boolean = {
    (v: @unchecked) match {
      case False => false
      case True => true
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

  def parseGStmtList(v: Js.Val): Vector[GStmt] = {
    val list = asVector(v)
    list.map(parseGStmt)
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

      case "Var" =>
        val name = asSymbol(map("name"))
        Var(name)
      case "Const" =>
        val ty_name = asSymbol(map("ty"))
        val ty = TyVar(ty_name)
        val val_ = map("value")

        ty_name match {
          case 'number => Const(asNumber(val_), ty.asInstanceOf[GType])
          case 'array => Const(asVector(val_), ty.asInstanceOf[GType])
          case 'string => Const(asString(val_), ty.asInstanceOf[GType])
          case 'boolean => Const(parseBoolean(val_), ty.asInstanceOf[GType])
          case 'null => Const(null, ty.asInstanceOf[GType])
        }

      case "ObjLiteral" =>
        val obj = asObj(map("fields"))
        val obj_map = obj.map { case (x, y) => (Symbol(x), parseGExpr(y)) }
        ObjLiteral(obj_map)
      case "Access" =>
        val expr = parseGExpr(map("expr"))
        val field = asSymbol(map("field"))
        Access(expr, field)
      case "IfExpr" =>
        val cond = parseGExpr(map("cond"))
        val e1 = parseGExpr(map("e1"))
        val e2 = parseGExpr(map("e2"))
        val resultType = parseType(map("resultType"))
        IfExpr(cond, e1, e2, resultType)

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
      case "AssignStmt" =>
        val lhs = parseGExpr(map("lhs"))
        val rhs = parseGExpr(map("rhs"))
        AssignStmt(lhs, rhs)
      case "ExprStmt" =>
        val e = parseGExpr(map("e"))
        val isReturn = parseBoolean(map("isReturn"))
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
        val stmts = parseGStmtList(map("stmts"))
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
    val jValue = parseAsJSon(pwd / 'data / 'tests / "mergeSort.json")
    println(jValue)

    asArray(jValue).map(parseGStmt).foreach(println)
  }
}
