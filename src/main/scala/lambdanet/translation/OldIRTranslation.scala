package lambdanet.translation

import funcdiff.SimpleMath
import funcdiff.SimpleMath.Extensions._
import lambdanet.surface.{GExpr, GModule, GStmt}
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
      freezeToType: Option[TyAnnot] = None,
      libId: Option[Symbol]
  )(implicit tyVars: Set[Symbol]): IRType = ???
  def newVar(): Var = {
    assert(varIdx >= 0)
    val v = Var(Left(varIdx))
    varIdx += 1
    v
  }

  def getTyVar(gMark: GTMark, name: Option[Symbol])(
      implicit tyVars: Set[Symbol]
  ): IRType = ???

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
  ): Vector[OldIR.IRStmt] = ???

  def translateFunc(
      func: surface.FuncDef
  )(
      quantifiedTypes: Set[Symbol]
  ): OldIR.FuncDef = ???
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
  ): IRExpr = ???
}
