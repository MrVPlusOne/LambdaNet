package lambdanet.translation

import funcdiff.SimpleMath
import funcdiff.SimpleMath.Extensions._
import lambdanet.surface.{GExpr, GModule, GStmt, TypeAnnotation}
import lambdanet.translation.OldIR._
import lambdanet.translation.OldIRTranslation._
import lambdanet._
import lambdanet.types.{
  AnyType,
  FuncType,
  GTHole,
  GTMark,
  GType,
  ObjectType,
  TyVar
}

import scala.collection.mutable

object OldIRTranslation {

  def groupInBlock(stmts: Vector[OldIR.IRStmt]): BlockStmt = {
    stmts match {
      case Vector(b: BlockStmt) => b
      case _                    => BlockStmt(stmts)
    }
  }

  def translateType(ty: GType)(implicit tyVars: Set[Symbol]): GType = {
    ty match {
      case tyVar: TyVar => if (tyVars.contains(tyVar.id)) AnyType else tyVar
      case AnyType      => AnyType
      case f: FuncType  => f.from.map(translateType) -: translateType(f.to)
      case o: ObjectType =>
        ObjectType(o.fields.mapValuesNow(translateType))
    }
  }

  /** collect the <b>top-level</b> public exports */
  def collectExports(stmts: Vector[IRStmt]): ModuleExports = {
    val terms = mutable.HashMap[Symbol, (IRType, Exported)]()
    val classes = mutable.HashMap[TypeName, (IRType, Exported)]()
    val aliases = mutable.HashMap[TypeName, (IRType, Exported)]()
    var defaultVar: Option[(Var, IRType)] = None
    var defaultClass: Option[(TypeName, IRType)] = None

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
        v.nameOpt.foreach(n => terms(n) = (mark, false))

        exportLevel match {
          case ExportLevel.Public =>
            terms(v.nameOpt.get) = (mark, true)
          case ExportLevel.Default =>
            require(
              defaultVar.isEmpty,
              s"trying to set default to $mark, but get ${defaultVar.get}"
            )
            defaultVar = Some(v -> mark)
          case ExportLevel.Unspecified =>
        }
      case f: FuncDef =>
        terms(f.name) = (f.funcT, false)

        f.exportLevel match {
          case ExportLevel.Public =>
            terms(f.name) = (f.funcT, true)
          case ExportLevel.Default =>
            require(defaultVar.isEmpty)
            defaultVar = Some(Var(Right(f.name)) -> f.funcT)
          case ExportLevel.Unspecified =>
        }
      case c: ClassDef =>
        classes(c.name) = (c.classT, false)
        terms(c.name) = (c.companionT, false)

        c.exportLevel match {
          case ExportLevel.Public =>
            classes(c.name) = (c.classT, true)
            terms(c.name) = (c.companionT, true)
          case ExportLevel.Default =>
            require(defaultClass.isEmpty)
            defaultClass = Some(c.name -> c.classT)
          case ExportLevel.Unspecified =>
        }
      case c: TypeAliasIRStmt =>
        c.level match {
          case ExportLevel.Public =>
            require(!aliases.contains(c.name))
            aliases(c.name) = (c.aliasT, true)
          case ExportLevel.Default =>
            throw new Error("Type Alias default export not supported")
          case ExportLevel.Unspecified =>
            aliases(c.name) = (c.aliasT, false)
        }
      case _ =>
    }
    stmts.foreach(rec)

    val exports = terms.toMap.map {
      case (n, v) => (n, ExportCategory.Term) -> v
    } ++ classes.toMap.map {
      case (n, v) => (n, ExportCategory.Class) -> v
    } ++ aliases.toMap.map {
      case (n, v) => (n, ExportCategory.TypeAlias) -> v
    }
    ModuleExports(exports, defaultVar, defaultClass)
  }

}

/** Used during the translation to allocate new [[OldIR.Var]]s, [[OldIR.IRType]]s, and [[GTHole]]s. */
class OldIRTranslation() {
  import collection.mutable

  var varIdx: Int = 0
  var tyVarIdx: Int = 0
  //noinspection TypeAnnotation
  val irTypes = mutable.Buffer[IRType]()
  //noinspection TypeAnnotation
  val holeTyVarMap = mutable.HashMap[GTHole, IRType]()
  //noinspection TypeAnnotation
  val tyVarHoleMap = mutable.HashMap[IRType, GTHole]()

  /**
    * Create and register a new [[IRType]].
    */
  def newTyVar(
      origin: Option[GTHole],
      name: Option[Symbol],
      freezeToType: Option[TypeAnnotation] = None,
      libId: Option[Symbol]
  )(implicit tyVars: Set[Symbol]): IRType = {
    assert(tyVarIdx >= 0)
    val tv =
      IRType(
        tyVarIdx,
        name,
        freezeToType.map(a => a.copy(ty = translateType(a.ty))),
        libId
      )
    irTypes += tv
    origin.foreach { h =>
      holeTyVarMap(h) = tv
      tyVarHoleMap(tv) = h
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

  def getTyVar(gMark: GTMark, name: Option[Symbol])(
      implicit tyVars: Set[Symbol]
  ): IRType = {
    gMark match {
      case h: GTHole => newTyVar(Some(h), name, None, None)
      case ty: GType =>
        newTyVar(None, name, Some(TypeAnnotation(ty, needInfer = true)), None)
    }
  }

  private def exprAsVar(
      expr: IRExpr
  )(implicit tyVars: Set[Symbol]): (Vector[IRStmt], Var) =
    expr match {
      case v: Var => (Vector(), v)
      //      case c: Const => todo: merge const nodes for inference
      //        if(env.constMap.contains(c)) (Vector(), env.constMap(c))
      //        else {
      //          val v = env.newVar()
      //          Vector(
      //            VarDef(v, env.newTyVar(None, None), c, ExportLevel.Private)
      //          ) -> v
      //        }
      case _ =>
        val v = newVar()
        Vector(
          VarDef(
            v,
            newTyVar(None, None, None, None),
            expr,
            ExportLevel.Unspecified
          )
        ) -> v
    }

  def translateModule(module: GModule): IRModule = {
    val irStmts = module.stmts.flatMap(s => translateStmt(s)(Set()))

    val moduleExports =
      SimpleMath.withErrorMessage(
        s"collectExports failed for ${module.path}\nmodule:\n${irStmts
          .map(_.prettyPrint())
          .mkString("\n")}"
      ) {
        collectExports(irStmts)
      }

    IRModule(
      module.path,
      module.imports,
      module.exportStmts,
      moduleExports,
      irStmts
    )
  }

  def groupInBlock(stmts: Vector[OldIR.IRStmt]): OldIR.BlockStmt = {
    stmts match {
      case Vector(b: OldIR.BlockStmt) => b
      case _                          => OldIR.BlockStmt(stmts)
    }
  }

  def translateStmt(
      stmt: GStmt
  )(
      implicit quantifiedTypes: Set[Symbol]
  ): Vector[OldIR.IRStmt] = {

    stmt match {
      case surface.VarDef(x, ty, init, isConst, level) =>
        val v = namedVar(x)
        val (defs, initE) = translateExpr2(init)
        if (isConst) {
          defs ++ Vector(VarDef(v, getTyVar(ty, v.nameOpt), initE, level))
        } else {
          val (defs2, initV) = exprAsVar(initE)
          defs ++ defs2 ++ Vector(
            VarDef(
              v,
              getTyVar(ty, v.nameOpt),
              Const("undefined", AnyType),
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
      case surface.BlockStmt(stmts) =>
        Vector(groupInBlock(stmts.flatMap(translateStmt)))
      case f: surface.FuncDef =>
        val newTyVars = quantifiedTypes ++ f.tyVars.toSet
        Vector(translateFunc(f)(newTyVars))
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
          v1.getOrElse(false, Map()).mapValuesNow(_._1) -> v1
            .getOrElse(true, Map())
            .mapValuesNow(_._1)
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
          case t: GType => surface.Const("unparsed", t)
          case _: GTHole =>
            surface.Const("unhandled", AnyType) //fixme: properly handle this
        } ++ staticMethods.map { m =>
          m.name -> surface.Var(renameStatic(m.name))
        }

        val renamed = staticMethods.map { m =>
          m.copy(name = renameStatic(m.name))
        }

        val defs = mutable.ListBuffer[IRStmt]()
        val irObj =
          translateExpr(surface.ObjLiteral(staticMembers))(Set(), defs)
        val companionT = newTyVar(None, None, None, None)
        val staticObject = {
          VarDef(
            Var(Right(name)),
            companionT,
            irObj,
            level
          )
        }

        val newTyVars = quantifiedTypes ++ tyVars.toSet

        val allMethods = renamed.flatMap(m => translateStmt(m)(newTyVars))

        val instanceStmts = if (isAbstract) {
          val fields = instanceVars.map {
            case (n, m) =>
              n -> m.asInstanceOf[GType]
          } ++ instanceMethods
            .map { m =>
              m.name -> m.functionType
            }
          val objType = ObjectType(fields)
          translateStmt(surface.TypeAliasStmt(name, tyVars, objType, level))(
            newTyVars
          )
        } else {
          val classT = newTyVar(None, Some(name), None, None)
          val cons = allMethods.head.asInstanceOf[FuncDef]
          Vector(
            cons
              .copy(returnType = classT), // need to modify the return type of constructor
            ClassDef(
              name,
              superType,
              instanceVars.map {
                case (v, mark) => v -> getTyVar(mark, Some(v))(newTyVars)
              },
              instanceMethods.map(f => translateFunc(f)(newTyVars)),
              classT,
              companionT,
              level
            )
          )
        }

        allMethods.drop(if (isAbstract) 0 else 1) ++ defs ++ instanceStmts :+ staticObject
      case surface.TypeAliasStmt(name, tyVars, ty, level) =>
        val aliasT = newTyVar(
          None,
          Some(name),
          freezeToType = Some(TypeAnnotation(ty, needInfer = false)),
          None
        )(quantifiedTypes ++ tyVars.toSet)
        Vector(TypeAliasIRStmt(aliasT, level))
    }
  }

  def translateFunc(
      func: surface.FuncDef
  )(
      quantifiedTypes: Set[Symbol]
  ): OldIR.FuncDef = {
    import func._
    implicit val newTyVars: Set[Symbol] = quantifiedTypes ++ tyVars
    val args1 = args.map {
      case (argName, mark) =>
        namedVar(argName) -> getTyVar(mark, Some(argName))
    }
    FuncDef(
      name,
      args1,
      getTyVar(returnType, None),
      groupInBlock(translateStmt(body)(newTyVars)),
      newTyVar(None, Some(name), None, None),
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
      case surface.Const(value, ty) =>
        Const(value, ty)
      case surface.FuncCall(f, args) =>
        val fVar = asVar(translateExpr(f))
        val argsVars = args.map(e => asVar(translateExpr(e)))
        OldIR.FuncCall(fVar, argsVars)
      case _: surface.Cast => ???
      case surface.ObjLiteral(fields) =>
        ObjLiteral(fields.mapValuesNow(e => asVar(translateExpr(e))))
      case surface.Access(receiver, field) =>
        val v = asVar(translateExpr(receiver))
        FieldAccess(v, field)
      case surface.IfExpr(cond, e1, e2, _) =>
        val condV = asVar(translateExpr(cond))
        val e1V = asVar(translateExpr(e1))
        val e2V = asVar(translateExpr(e2))
        IfExpr(condV, e1V, e2V)
    }
  }
}
