package lambdanet.translation

import lambdanet.types._
import funcdiff.SimpleMath.Extensions._
import IR._
import lambdanet.{
  Annot,
  AnyType,
  ExportLevel,
  GTHole,
  ObjectType,
  TyAnnot,
  TyVar,
  surface
}
import lambdanet.surface.{GExpr, GModule}
import IRTranslation._
import lambdanet.translation.ImportsResolution.ModuleExports
import lambdanet.translation.PLangTranslation.monotype

object IRTranslation {

  val undefinedVar = Var(Right('undefined))

  def constToVar(c: surface.Const): Var = {
    c.ty match {
      case AnyType   => undefinedVar
      case TyVar(id) => Var(Right(id))
    }
  }
}

/** Used during the translation to allocate new [[IR.Var]]s and [[GTHole]]s. */
class IRTranslation {

  val holeContext = new TypeHoleContext

  var varIdx: Int = 0

  def newVar(): Var = {
    assert(varIdx >= 0)
    val v = Var(Left(varIdx))
    varIdx += 1
    v
  }

  private def translateMark(
      mark: TyAnnot
  )(implicit tyVars: Set[Symbol]): GTHole = {
    holeContext.newTHole(mark.map(monotype))
  }

  private def exprAsVar(
      expr: IRExpr
  ): (Vector[IRStmt], Var) =
    expr match {
      case v: Var => (Vector(), v)
      case _ =>
        val v = newVar()
        Vector(
          VarDef(
            v,
            holeContext.newTHole(Annot.Missing),
            expr,
            ExportLevel.Unspecified
          )
        ) -> v
    }

  def translateModule(
      module: GModule,
      moduleExports: ModuleExports
  ): IRModule = {
    val irStmts = module.stmts.flatMap(s => translateStmt(s)(Set()))

    IRModule(
      module.path,
      module.imports,
      module.exportStmts,
      irStmts
    )
  }

  def translateStmt(
      stmt: surface.GStmt
  )(
      implicit quantifiedTypes: Set[Symbol]
  ): Vector[IRStmt] = {

    stmt match {
      case surface.VarDef(x, ty, init, isConst, level) =>
        val v = namedVar(x)
        val (defs, initE) = translateExpr2(init)
        val t1 = translateMark(ty)
        if (isConst) {
          defs ++ Vector(VarDef(v, t1, initE, level))
        } else {
          val (defs2, initV) = exprAsVar(initE)
          defs ++ defs2 ++ Vector(
            VarDef(
              v,
              t1,
              undefinedVar,
              level
            ),
            Assign(v, initV)
          )
        }
      case surface.AssignStmt(lhs, rhs) =>
        val (lDefs, lE) = translateExpr2(lhs)
        val (rDefs, rE) = translateExpr2(rhs)
        val (defs3, lV) = exprAsVar(lE)
        val (defs4, rV) = exprAsVar(rE)
        (lDefs ++ rDefs ++ defs3 ++ defs4) :+ Assign(lV, rV)
      case surface.ExprStmt(expr, isReturn) =>
        val (defs, e) = translateExpr2(expr)
        val (defs2, r) = exprAsVar(e)
        if (isReturn) defs ++ defs2 :+ ReturnStmt(r)
        else defs ++ defs2
      case surface.IfStmt(cond, branch1, branch2) =>
        val (condDef, condE) = translateExpr2(cond)
        val (condDef2, condV) = exprAsVar(condE)
        val branch1Stmt = groupInBlock(translateStmt(branch1))
        val branch2Stmt = groupInBlock(translateStmt(branch2))
        val ifStmt = IfStmt(condV, branch1Stmt, branch2Stmt)
        condDef ++ condDef2 :+ ifStmt
      case surface.WhileStmt(cond, body) =>
        val (condDef, condE) = translateExpr2(cond)
        val (condDef2, condV) = exprAsVar(condE)
        // recompute the conditional expression value at the end of the loop
        val condCompute =
          (condDef ++ condDef2).filterNot(_.isInstanceOf[VarDef])
        val bodyStmt = groupInBlock(translateStmt(body) ++ condCompute)
        condDef ++ condDef2 :+ WhileStmt(condV, bodyStmt)
      case surface.CommentStmt(_) => Vector()
      case b: surface.BlockStmt =>
        Vector(translateBlock(b))
      case f: surface.FuncDef =>
        val newTyVars = quantifiedTypes ++ f.tyVars.toSet
        Vector(translateFunc(f, newTyVars))
      case surface.ClassDef(
          name,
          tyVars,
          superType,
          vars,
          funcDefs,
          level
          ) =>
        val newTyVars = quantifiedTypes ++ tyVars.toSet

        val isAbstract = ???
        if (isAbstract) {
          val fields = (vars.map {
            case (n, m) =>
              n -> m.get
          } ++ funcDefs
            .map { m =>
              m.name -> m.functionType
            }).map(p => p._1 -> monotype(p._2))
          val objType = ObjectType(fields)
          translateStmt(surface.TypeAliasStmt(name, tyVars, objType, level))(
            newTyVars
          )
        } else {
          Vector(
            ClassDef(
              name,
              superType,
              vars.mapValuesNow(p => translateMark(p)(newTyVars)),
              funcDefs.map(f => translateFunc(f, newTyVars)),
              level
            )
          )
        }

      case surface.TypeAliasStmt(name, tyVars, ty, level) =>
        Vector(
          TypeAliasStmt(
            name,
            monotype(ty)(quantifiedTypes ++ tyVars),
            level
          )
        )
      case surface.Namespace(name, block, _) =>
        Vector(Namespace(name, translateBlock(block)))
    }
  }

  def translateBlock(
      b: surface.BlockStmt
  )(implicit tyVars: Set[VarName]): IR.BlockStmt = {
    groupInBlock(b.stmts.flatMap(translateStmt))
  }

  def translateFunc(
      func: surface.FuncDef,
      quantifiedTypes: Set[VarName]
  ): IR.FuncDef = {
    import func._
    implicit val newTyVars: Set[Symbol] = quantifiedTypes ++ tyVars
    val args1 = args.map {
      case (argName, mark) =>
        namedVar(argName) -> translateMark(mark)
    }

    FuncDef(
      name,
      args1,
      translateMark(returnType),
      groupInBlock(translateStmt(body)(newTyVars)),
      exportLevel
    )
  }

  import collection.mutable

  /** @see [[translateExpr]] */
  def translateExpr2(expr: GExpr)(
      implicit tyVars: Set[Symbol]
  ): (Vector[IRStmt], IRExpr) = {
    val defs = mutable.ListBuffer[IRStmt]()
    val e = translateExpr(expr)(tyVars, defs)
    (defs.toVector, e)
  }

  /**
    * Translate an expr from the surface language into a Var in IR,
    * possibly generate some extra definitional statements and append them
    * into the ListBuffer argument.
    */
  def translateExpr(expr: GExpr)(
      implicit tyVars: Set[Symbol],
      defs: mutable.ListBuffer[IRStmt]
  ): IRExpr = {
    def asVar(expr: IRExpr): Var = {
      val (stmts, v) = exprAsVar(expr)
      defs.appendAll(stmts)
      v
    }

    expr match {
      case surface.Var(name) => namedVar(name)
      case c: surface.Const  => constToVar(c)
      case surface.FuncCall(f, args) =>
        val fVar = asVar(translateExpr(f))
        val argsVars = args.map(e => asVar(translateExpr(e))).toVector
        IR.FuncCall(fVar, argsVars)
      case surface.Cast(e, ty) =>
        Cast(asVar(translateExpr(e)), monotype(ty))
      case surface.ObjLiteral(fields) =>
        ObjLiteral(fields.mapValuesNow(e => asVar(translateExpr(e))))
      case surface.Access(receiver, field) =>
        val v = asVar(translateExpr(receiver))
        FieldAccess(v, field)
      case surface.IfExpr(cond, e1, e2) =>
        val condV = asVar(translateExpr(cond))
        val e1V = asVar(translateExpr(e1))
        val e2V = asVar(translateExpr(e2))
        IfExpr(condV, e1V, e2V)
    }
  }
}
