package lambdanet.translation

import lambdanet.types._
import funcdiff.SimpleMath.Extensions._
import IR._
import lambdanet.ExportLevel
import lambdanet.surface
import lambdanet.surface.{GExpr, GModule}
import IRTranslation._

object IRTranslation {
  def translateType(ty: GType)(implicit tyVars: Set[Symbol]): GType = {
    ty match {
      case tyVar: TyVar => if (tyVars.contains(tyVar.id)) AnyType else tyVar
      case AnyType      => AnyType
      case f: FuncType  => f.from.map(translateType) -: translateType(f.to)
      case o: ObjectType =>
        ObjectType(o.fields.mapValuesNow(translateType))
    }
  }

  def translateMark(mark: GTMark)(implicit tyVars: Set[Symbol]): GTMark = {
    mark match {
      case t: GType => translateType(t)
      case _        => mark
    }
  }
}

/** Used during the translation to allocate new [[IR.Var]]s and [[GTHole]]s. */
class IRTranslation(holeContext: TypeHoleContext) {

  var varIdx: Int = 0

  def newVar(): Var = {
    assert(varIdx >= 0)
    val v = Var(Left(varIdx))
    varIdx += 1
    v
  }

  private def exprAsGround(
      expr: IRExpr
  ): (Vector[IRStmt], Ground) =
    expr match {
      case v: Ground => (Vector(), v)
      case _ =>
        val v = newVar()
        Vector(
          VarDef(v, holeContext.newTHole(None), expr, ExportLevel.Private)
        ) -> v
    }

  def translateModule(module: GModule): IRModule = {
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
          val (defs2, initV) = exprAsGround(initE)
          defs ++ defs2 ++ Vector(
            VarDef(
              v,
              t1,
              Const("undefined", AnyType),
              level
            ),
            Assign(v, initV)
          )
        }
      case surface.AssignStmt(lhs, rhs) =>
        val (lDefs, lE) = translateExpr2(lhs)
        val (rDefs, rE) = translateExpr2(rhs)
        val (defs3, lV) = exprAsGround(lE)
        val (defs4, rV) = exprAsGround(rE)
        (lDefs ++ rDefs ++ defs3 ++ defs4) :+ Assign(lV.asInstanceOf[Var], rV)
      case surface.ExprStmt(expr, isReturn) =>
        val (defs, e) = translateExpr2(expr)
        val (defs2, r) = exprAsGround(e)
        if (isReturn) defs ++ defs2 :+ ReturnStmt(r)
        else defs ++ defs2
      case surface.IfStmt(cond, branch1, branch2) =>
        val (condDef, condE) = translateExpr2(cond)
        val (condDef2, condV) = exprAsGround(condE)
        val branch1Stmt = groupInBlock(translateStmt(branch1))
        val branch2Stmt = groupInBlock(translateStmt(branch2))
        val ifStmt = IfStmt(condV, branch1Stmt, branch2Stmt)
        condDef ++ condDef2 :+ ifStmt
      case surface.WhileStmt(cond, body) =>
        val (condDef, condE) = translateExpr2(cond)
        val (condDef2, condV) = exprAsGround(condE)
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
          constructor,
          vars,
          funcDefs,
          level,
          isAbstract
          ) =>
        val (instanceVars, staticVars) = {
          val v1 = vars.groupBy(_._2._2)
          (
            v1.getOrElse(false, Map()).mapValuesNow(_._1),
            v1.getOrElse(true, Map()).mapValuesNow(_._1)
          )
        }
        val (instanceMethods, staticMethods0) = {
          val v1 = funcDefs.groupBy(_._2)
          (
            v1.getOrElse(false, Vector()).map(_._1),
            v1.getOrElse(true, Vector()).map(_._1)
          )
        }

        val staticMethods =
          if (isAbstract) staticMethods0 else constructor +: staticMethods0

        def renameStatic(methodName: Symbol): Symbol = {
          Symbol(name.name + "." + methodName.name)
        }

        val staticMembers = staticVars.mapValuesNow {
          case t: GType  => surface.Const("unparsed", t)
          case _: GTHole =>
            //fixme: properly handle this using initialization experssion
            surface.Const("unhandled", AnyType)
        } ++ staticMethods.map { m =>
          m.name -> surface.Var(renameStatic(m.name))
        }

        val (defs, companionLiteral) = translateExpr2(
          surface.ObjLiteral(staticMembers)
        )
        val companionDef = {
          VarDef(
            Var(Right(name)),
            // companion object's type is fully determined by the object literal
            holeContext.newTHole(None),
            companionLiteral,
            level
          )
        }

        val newTyVars = quantifiedTypes ++ tyVars.toSet

        val allMethods = staticMethods
          .map(m => m.copy(name = renameStatic(m.name)))
          .flatMap(m => translateStmt(m)(newTyVars))

        val instanceStmts = if (isAbstract) {
          val fields = (instanceVars.map {
            case (n, m) =>
              n -> m.asInstanceOf[GType]
          } ++ instanceMethods
            .map { m =>
              m.name -> m.functionType
            }).map(p => p._1 -> translateType(p._2))
          val objType = ObjectType(fields)
          translateStmt(surface.TypeAliasStmt(name, tyVars, objType, level))(
            newTyVars
          )
        } else {
          val classT = TyVar(name)
          val cons = allMethods.head.asInstanceOf[FuncDef]
          Vector(
            // need to modify the return type of constructor
            cons.copy(returnType = classT),
            ClassDef(
              name,
              superType,
              instanceVars,
              instanceMethods.map(f => translateFunc(f, newTyVars)),
              level
            )
          )
        }

        allMethods.drop(if (isAbstract) 0 else 1) ++ defs ++ instanceStmts :+ companionDef
      case surface.TypeAliasStmt(name, tyVars, ty, level) =>
        Vector(
          TypeAliasStmt(
            name,
            translateType(ty)(quantifiedTypes ++ tyVars),
            level
          )
        )
      case surface.Namespace(name, block) =>
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
    }.toVector

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
    def asGround(expr: IRExpr): Ground = {
      val (stmts, v) = exprAsGround(expr)
      defs.appendAll(stmts)
      v
    }

    expr match {
      case surface.Var(name) => namedVar(name)
      case surface.Const(value, ty) =>
        Const(value, translateType(ty))
      case surface.FuncCall(f, args) =>
        val fVar = asGround(translateExpr(f))
        val argsVars = args.map(e => asGround(translateExpr(e))).toVector
        IR.FuncCall(fVar, argsVars)
      case surface.Cast(expr, ty) =>
        Cast(asGround(translateExpr(expr)), translateType(ty))
      case surface.ObjLiteral(fields) =>
        ObjLiteral(fields.mapValuesNow(e => asGround(translateExpr(e))))
      case surface.Access(receiver, field) =>
        val v = asGround(translateExpr(receiver))
        FieldAccess(v, field)
      case surface.IfExpr(cond, e1, e2, _) =>
        val condV = asGround(translateExpr(cond))
        val e1V = asGround(translateExpr(e1))
        val e2V = asGround(translateExpr(e2))
        IfExpr(condV, e1V, e2V)
    }
  }
}
