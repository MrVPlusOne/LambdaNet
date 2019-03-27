package infer

import funcdiff.SimpleMath.Extensions._
import gtype._
import infer.PredicateGraphConstruction.PredicateContext

object IRTranslation {
  import IR._

  def translateType(ty: GType)(implicit tyVars: Set[Symbol]): GType = {
    import gtype._
    ty match {
      case tyVar: TyVar => if (tyVars.contains(tyVar.id)) AnyType else tyVar
      case AnyType      => AnyType
      case f: FuncType  => f.from.map(translateType) -: translateType(f.to)
      case o: ObjectType =>
        ObjectType(o.fields.mapValuesNow(translateType))
    }
  }

  class TranslationEnv() {
    import collection.mutable

    var varIdx: Int = 0
    var tyVarIdx: IRTypeId = 0
    //noinspection TypeAnnotation
    val idTypeMap = mutable.HashMap[IRTypeId, IRType]()
    //noinspection TypeAnnotation
    val holeTyVarMap = mutable.HashMap[GTHole, IRType]()
    //noinspection TypeAnnotation
    val tyVarHoleMap = mutable.HashMap[IRTypeId, GTHole]()

    /**
      * Create and register a new [[IRType]].
      */
    def newTyVar(
      origin: Option[GTHole],
      name: Option[Symbol],
      freezeToType: Option[gtype.GType] = None
    )(implicit tyVars: Set[Symbol]): IRType = {
      assert(tyVarIdx >= 0)
      val tv = IRType(tyVarIdx, name, freezeToType.map(translateType))
      idTypeMap(tv.id) = tv
      origin.foreach { h =>
        holeTyVarMap(h) = tv
        tyVarHoleMap(tv.id) = h
      }
      tyVarIdx += 1
      tv
    }

    def newVar(): Var = {
      assert(varIdx >= 0)
      val v = Var(Left(varIdx))
      varIdx += 1
      v
    }

    def getTyVar(gMark: gtype.GTMark, name: Option[Symbol])(
      implicit tyVars: Set[Symbol]
    ): IRType = {
      gMark match {
        case h: gtype.GTHole => newTyVar(Some(h), name, None)
        case ty: gtype.GType => newTyVar(None, name, Some(ty))
      }
    }
  }

  def exprAsVar(
    expr: IRExpr
  )(implicit tyVars: Set[Symbol], env: TranslationEnv): (Vector[IRStmt], Var) =
    expr match {
      case v: Var => (Vector(), v)
      case _ =>
        val v = env.newVar()
        Vector(
          VarDef(v, env.newTyVar(None, None), expr, ExportLevel.Private)
        ) -> v
    }

  def translateStmt(
    stmt: gtype.GStmt
  )(
    implicit quantifiedTypes: Set[Symbol],
    env: TranslationEnv
  ): Vector[IR.IRStmt] = {
    stmt match {
      case gtype.VarDef(x, ty, init, isConst, level) =>
        val v = namedVar(x)
        val (defs, initE) = translateExpr2(init)
        if (isConst) {
          defs ++ Vector(VarDef(v, env.getTyVar(ty, v.nameOpt), initE, level))
        } else {
          val (defs2, initV) = exprAsVar(initE)
          defs ++ defs2 ++ Vector(
            VarDef(
              v,
              env.getTyVar(ty, v.nameOpt),
              Const("undefined", gtype.AnyType),
              level
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
        val branch1Stmt = groupInBlock(translateStmt(branch1))
        val branch2Stmt = groupInBlock(translateStmt(branch2))
        val ifStmt = IfStmt(condV, branch1Stmt, branch2Stmt)
        condDef ++ condDef2 :+ ifStmt
      case gtype.WhileStmt(cond, body) =>
        val (condDef, condE) = translateExpr2(cond)
        val (condDef2, condV) = exprAsVar(condE)
        // recompute the conditional expression value at the end of the loop
        val condCompute =
          (condDef ++ condDef2).filterNot(_.isInstanceOf[VarDef])
        val bodyStmt = groupInBlock(translateStmt(body) ++ condCompute)
        condDef ++ condDef2 :+ WhileStmt(condV, bodyStmt)
      case gtype.CommentStmt(_) => Vector()
      case gtype.BlockStmt(stmts) =>
        stmts.flatMap(translateStmt)
      case f: gtype.FuncDef =>
        val newTyVars = quantifiedTypes ++ f.tyVars.toSet
        Vector(translateFunc(f)(newTyVars, env))
      case gtype.ClassDef(name, tyVars, superType, constructor, vars, funcDefs, level) =>
        val classT = env.newTyVar(None, Some(name))
        val newTyVars = quantifiedTypes ++ tyVars.toSet
        Vector(
          ClassDef(
            name,
            superType,
            translateFunc(constructor)(newTyVars, env)
              .copy(returnType = classT),
            vars.map {
              case (v, mark) => v -> env.getTyVar(mark, Some(v))(newTyVars)
            },
            funcDefs.map(f => translateFunc(f)(newTyVars, env)),
            classT,
            level
          )
        )
    }
  }

  def translateFunc(
    func: gtype.FuncDef
  )(
    implicit quantifiedTypes: Set[Symbol],
    env: TranslationEnv
  ): IR.FuncDef = {
    import func._
    val args1 = args.map {
      case (argName, mark) =>
        namedVar(argName) -> env.getTyVar(mark, Some(argName))
    }
    FuncDef(
      name,
      args1,
      env.getTyVar(returnType, None),
      translateStmt(body),
      env.newTyVar(None, Some(name)),
      exportLevel
    )
  }

  import collection.mutable

  /** @see [[translateExpr]] */
  def translateExpr2(expr: gtype.GExpr)(
    implicit tyVars: Set[Symbol],
    env: TranslationEnv
  ): (Vector[IRStmt], IRExpr) = {
    val defs = mutable.ListBuffer[IRStmt]()
    val e = translateExpr(expr)(tyVars, env, defs)
    (defs.toVector, e)
  }

  /**
    * Translate an expr from the surface language into a Var in IR,
    * possibly generate some extra definitional statements and append them
    * into the ListBuffer argument.
    */
  def translateExpr(expr: gtype.GExpr)(
    implicit tyVars: Set[Symbol],
    env: TranslationEnv,
    defs: mutable.ListBuffer[IRStmt]
  ): IRExpr = {
    def asVar(expr: IRExpr): Var = {
      val (stmts, v) = exprAsVar(expr)
      defs.appendAll(stmts)
      v
    }

    expr match {
      case gtype.Var(name) => namedVar(name)
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

  /** collect the <b>top-level</b> public exports */
  def collectExports(stmts: Vector[IRStmt]): ModuleExports = {
    val terms = mutable.HashMap[Var, IRType]()
    val types = mutable.HashMap[ClassName, IRType]()
    var defaultExport: Option[(Either[Var, ClassName], IRType)] = None

    /*
     *   | var x: τ = e                      ([[VarDef]])
     *   | x := x                            ([[Assign]])
     *   | [return] x                        ([[ReturnStmt]])
     *   | if(x) S else S                    ([[IfStmt]])
     *   | while(x) S                        ([[WhileStmt]])
     *   | { S; ...; S }                     ([[BlockStmt]])
     *   | function x (x: τ, ..., x:τ): τ S  ([[FuncDef]])
     *   | class x (l: α, ..., l:α)          ([[ClassDef]])
     */
    def rec(stmt: IRStmt): Unit = stmt match {
      case VarDef(v, mark, _, exportLevel) =>
        exportLevel match {
          case ExportLevel.Public =>
            require(!terms.contains(v))
            terms(v) = mark
          case ExportLevel.MainExport =>
            require(defaultExport.isEmpty)
            defaultExport = Some(Left(v) -> mark)
          case ExportLevel.Private =>
        }
      case f: FuncDef =>
        val funcVar = Var(Right(f.name))
        f.exportLevel match {
          case ExportLevel.Public =>
            require(!terms.contains(funcVar))
            terms(funcVar) = f.funcT
          case ExportLevel.MainExport =>
            require(defaultExport.isEmpty)
            defaultExport = Some(Left(funcVar) -> f.funcT)
          case ExportLevel.Private =>
        }
      case c: ClassDef =>
        c.exportLevel match {
          case ExportLevel.Public =>
            require(!types.contains(c.name))
            types(c.name) = c.classT
          case ExportLevel.MainExport =>
            require(defaultExport.isEmpty)
            defaultExport = Some(Right(c.name) -> c.classT)
          case ExportLevel.Private =>
        }
      case _ =>
    }
    stmts.foreach(rec)

    ModuleExports(terms.toMap, types.toMap, defaultExport)
  }

  def translateModule(module: GModule)(
    implicit env: TranslationEnv
  ): IRModule = {
    val irStmts = module.stmts.flatMap(s => translateStmt(s)(Set(), env))
    IRModule(module.path, module.imports, collectExports(irStmts), irStmts)
  }

  def main(args: Array[String]): Unit = {
    val env = new TranslationEnv()
    val example = JSExamples.Collection.doublyLinkedList

    println {
      example.program.prettyPrint()
    }

    val stmts = translateStmt(example.program)(Set(), env)
    stmts.foreach(println)

    import infer.PredicateGraphConstruction._
    val ctx = PredicateContext.jsCtx(env)

    println {
      "oldCtx: " + ctx.newTypeMap
    }

    println {
      "newCtx: " + PredicateGraphConstruction.encodeIR(stmts, ctx)._2.newTypeMap
    }
  }
}
