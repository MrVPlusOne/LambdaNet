package infer

import gtype.{GTHole, JSExamples, GType}

object IRTranslation {
  import IR._

  class TranslationEnv() {

    var varIdx: Int = 0
    var tyVarIdx: Int = 0

    def newTyVar(ty: Option[GType], origin: Option[GTHole]): TyVar = {
      assert(tyVarIdx >= 0)
      val tv = TyVar(tyVarIdx, ty, origin)
      tyVarIdx += 1
      tv
    }

    def newVar(): Var = {
      assert(varIdx >= 0)
      val v = Var(Symbol(s"𝓥$varIdx"))
      varIdx += 1
      v
    }

    def getTyVar(gMark: gtype.GTMark): TyVar = {
      gMark match {
        case h: gtype.GTHole => newTyVar(None, Some(h))
        case ty: gtype.GType => newTyVar(Some(ty), None)
      }
    }
  }

  def exprAsVar(expr: IRExpr)(implicit env: TranslationEnv): (Vector[IRStmt], Var) =
    expr match {
      case v: Var => (Vector(), v)
      case _ =>
        val v = env.newVar()
        Vector(
          VarDef(v, env.newTyVar(None, None), expr)
        ) -> v
    }

  def translateStmt(
    stmt: gtype.GStmt
  )(
    implicit env: TranslationEnv
  ): Vector[IR.IRStmt] = {
    /*
     * S :=                                  ([[GStmt]])
     *   | var x: α = e                      ([[VarDef]])
     *   | e := e                            ([[AssignStmt]])
     *   | [return] e                        ([[ExprStmt]])
     *   | if e then e else e                ([[IfStmt]])
     *   | while e do e                      ([[WhileStmt]])
     *   | { S; ...; S }                     ([[BlockStmt]])
     *   | function x (x: α, ..., x:α): α    ([[FuncDef]])
     *   | class x (l: α, ..., l:α)          ([[ClassDef]])
     *     ↳ [extends x]{ f, ..., f }
     */
    stmt match {
      case gtype.VarDef(x, ty, init, isConst) =>
        val v = Var(x)
        val (defs, initE) = translateExpr2(init)
        if (isConst) {
          defs ++ Vector(VarDef(v, env.getTyVar(ty), initE))
        } else {
          val (defs2, initV) = exprAsVar(initE)
          defs ++ defs2 ++ Vector(
            VarDef(
              v,
              env.getTyVar(ty),
              Const("undefined", gtype.AnyType)
            ),
            Assign(v, initV)
          )
        }
      case gtype.AssignStmt(lhs, rhs) =>
        val (lDefs, lE) = translateExpr2(lhs)
        val (rDefs, rE) = translateExpr2(rhs)
        val (defs3, lV) = exprAsVar(lE)
        val (defs4, rV) = exprAsVar(rE)
        (lDefs ++ rDefs ++ defs3 ++ defs4) :+ Assign(lV, rV)
      case gtype.ExprStmt(expr, isReturn) =>
        val (defs, e) = translateExpr2(expr)
        val (defs2, r) = exprAsVar(e)
        if (isReturn) defs ++ defs2 :+ ReturnStmt(r)
        else defs ++ defs2
      case gtype.IfStmt(cond, branch1, branch2) =>
        val (condDef, condE) = translateExpr2(cond)
        val (condDef2, condV) = exprAsVar(condE)
        val branch1Stmt = tryToBlock(translateStmt(branch1))
        val branch2Stmt = tryToBlock(translateStmt(branch2))
        val ifStmt = IfStmt(condV, branch1Stmt, branch2Stmt)
        condDef ++ condDef2 :+ ifStmt
      case gtype.WhileStmt(cond, body) =>
        val (condDef, condE) = translateExpr2(cond)
        val (condDef2, condV) = exprAsVar(condE)
        // recompute the conditional expression value at the end of the loop
        val condCompute = (condDef ++ condDef2).filterNot(_.isInstanceOf[VarDef])
        val bodyStmt = tryToBlock(translateStmt(body) ++ condCompute)
        condDef ++ condDef2 :+ WhileStmt(condV, bodyStmt)
      case gtype.BlockStmt(stmts) =>
        stmts.flatMap(translateStmt)
      case f: gtype.FuncDef => Vector(translateFunc(f))
      case gtype.ClassDef(name, superType, constructor, vars, funcDefs) =>
        Vector(
          ClassDef(
            Var(name),
            superType.map(Var),
            translateFunc(constructor),
            vars.mapValues(mark => env.getTyVar(mark)),
            funcDefs.map(translateFunc)
          )
        )
    }
  }

  def translateFunc(
    func: gtype.FuncDef
  )(
    implicit env: TranslationEnv
  ): IR.FuncDef = {
    import func._
    val fV = Var(name)
    val args1 = args.map {
      case (argName, mark) =>
        Var(argName) -> env.getTyVar(mark)
    }
    FuncDef(fV, args1, env.getTyVar(returnType), translateStmt(body))
  }

  import collection.mutable

  /** @see [[translateExpr]] */
  def translateExpr2(expr: gtype.GExpr)(
    implicit env: TranslationEnv
  ): (Vector[IRStmt], IRExpr) = {
    val defs = mutable.ListBuffer[IRStmt]()
    val e = translateExpr(expr)(env, defs)
    (defs.toVector, e)
  }

  /**
    * Translate an expr from the surface language into a Var in IR,
    * possibly generate some extra definitional statements and append them
    * into the ListBuffer argument.
    */
  def translateExpr(expr: gtype.GExpr)(
    implicit env: TranslationEnv,
    defs: mutable.ListBuffer[IRStmt]
  ): IRExpr = {
    def asVar(expr: IRExpr): Var = {
      val (stmts, v) = exprAsVar(expr)
      defs.appendAll(stmts)
      v
    }

    expr match {
      case gtype.Var(name) => Var(name)
      case gtype.Const(value, ty) =>
        Const(value, ty)
      case gtype.FuncCall(f, args) =>
        val fVar = asVar(translateExpr(f))
        val argsVars = args.map(e => asVar(translateExpr(e)))
        IR.FuncCall(fVar, argsVars)
      case _: gtype.Cast => ???
      case gtype.ObjLiteral(fields) =>
        ObjLiteral(fields.mapValues(e => asVar(translateExpr(e))))
      case gtype.Access(receiver, field) =>
        val v = asVar(translateExpr(receiver))
        FieldAccess(v, field)
      case gtype.IfExpr(cond, e1, e2, _) =>
        val condV = asVar(translateExpr(cond))
        val e1V = asVar(translateExpr(e1))
        val e2V = asVar(translateExpr(e2))
        IfExpr(condV, e1V, e2V)
    }
  }

  def main(args: Array[String]): Unit = {
    val env = new TranslationEnv()
    val example = JSExamples.Collection.whileExample
    translateStmt(example.program)(env).foreach(println)
  }
}
