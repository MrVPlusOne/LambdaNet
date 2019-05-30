package lambdanet.translation

import lambdanet.{
  Annot,
  AnyType,
  ExportLevel,
  ExportStmt,
  FuncType,
  GType,
  ImportStmt,
  ObjectType,
  ProjectPath,
  TyAnnot,
  TyVar,
  surface
}
import lambdanet.surface.{GExpr, GModule}
import funcdiff.SimpleMath.Extensions._
import PredicateGraph.{PConst, PNode, PVar}
import lambdanet.translation.PLang._
import lambdanet.translation.PredicateGraph.PConst.PConstAllocator
import lambdanet.translation.PredicateGraph.PVar.PVarAllocator

import scala.collection.mutable

/** [[surface.GStmt]] marked with [[PNode]]s  */
object PLang {

  type NodeMapping = PNode => Option[GType]

  case class PModule(
      path: ProjectPath,
      imports: Vector[ImportStmt],
      exportStmts: Vector[ExportStmt],
      stmts: Vector[PStmt],
      mapping: Map[PNode, TyAnnot]
  ) {
    val moduleName: String = path.toString()
  }

  sealed trait PStmt

  case class VarDef(
      name: Symbol,
      node: PNode,
      init: GExpr,
      isConst: Boolean,
      exportLevel: ExportLevel.Value
  ) extends PStmt

  case class AssignStmt(lhs: GExpr, rhs: GExpr) extends PStmt

  case class ExprStmt(e: GExpr, isReturn: Boolean) extends PStmt

  case class IfStmt(cond: GExpr, branch1: PStmt, branch2: PStmt) extends PStmt

  case class WhileStmt(cond: GExpr, body: PStmt) extends PStmt

  case class CommentStmt(text: String) extends PStmt

  case class BlockStmt(stmts: Vector[PStmt]) extends PStmt

  case class FuncDef(
      name: Symbol,
      funcNode: PNode,
      args: Vector[(Symbol, PNode)],
      returnType: PNode,
      body: PStmt,
      exportLevel: ExportLevel.Value
  ) extends PStmt {
    @throws[NoSuchElementException]
    def functionType(implicit mapping: NodeMapping): FuncType = {
      FuncType(args.map(x => mapping(x._2).get).toList, mapping(returnType).get)
    }

  }

  case class ClassDef(
      name: Symbol,
      classNode: PNode,
      superType: Option[Symbol] = None,
      vars: Map[Symbol, (PNode, GExpr)],
      funcDefs: Vector[FuncDef],
      exportLevel: ExportLevel.Value
  ) extends PStmt {
    @throws[NoSuchElementException]
    def objectType(implicit mapping: NodeMapping): ObjectType = {
      ObjectType(
        vars.mapValuesNow(x => mapping(x._1).get) ++
          funcDefs.map(fd => fd.name -> fd.functionType)
      )
    }

  }

  case class TypeAliasStmt(
      name: Symbol,
      node: PNode,
      exportLevel: ExportLevel.Value
  ) extends PStmt

  case class Namespace(name: Symbol, block: BlockStmt) extends PStmt

}

/**
  * Duties:
  *   1. Getting rid of type variables
  *   2. Assigning [[PNode]]s. ([[PConst]] for library modules, [[PVar]] for project modules).
  */
object PLangTranslation {

  def fromGModule(
      module: GModule,
      allocator: Either[PVarAllocator, PConstAllocator]
  ): PModule = {
    val mapping = mutable.HashMap[PNode, TyAnnot]()

    def allocate(nameOpt: Option[Symbol], annot: TyAnnot, isTerm: Boolean)(
        implicit outerTyVars: Set[Symbol]
    ): PNode = {
      val v = allocator match {
        case Left(pVarAllocator) =>
          pVarAllocator.newVar(nameOpt, !isTerm)
        case Right(pConstAllocator) =>
          pConstAllocator.newVar(nameOpt.get, !isTerm)
      }
      mapping(v) = annot.map(monotype)
      v
    }

    def translateStmt(
        stmt: surface.GStmt
    )(implicit outerTyVars: Set[Symbol]): PStmt = {
      stmt match {
        case surface.VarDef(name, annot, init, isConst, exportLevel) =>
          val node = allocate(Some(name), annot, isTerm = true)
          VarDef(name, node, init, isConst, exportLevel)
        case surface.FuncDef(
            name,
            tyVars,
            args,
            returnType,
            body,
            exportLevel
            ) =>
          val newTyVars: Set[Symbol] = outerTyVars ++ tyVars
          val funcNode = allocate(Some(name), Annot.Missing, isTerm = true)
          val argNodes = args.map {
            case (n, annot) =>
              n -> allocate(Some(n), annot, isTerm = true)(newTyVars)
          }
          val returnNode = allocate(None, returnType, isTerm = true)(newTyVars)
          FuncDef(
            name,
            funcNode,
            argNodes,
            returnNode,
            translateStmt(body)(newTyVars),
            exportLevel
          )
        case surface.ClassDef(
            name,
            tyVars,
            superType,
            vars,
            funcDefs,
            exportLevel,
            _
            ) =>
          val newTyVars: Set[Symbol] = outerTyVars ++ tyVars
          val classNode = allocate(Some(name), Annot.Missing, isTerm = false)
          val varNodes = vars.map {
            case (s, (annot, expr)) =>
              (s, (allocate(Some(s), annot, isTerm = true)(newTyVars), expr))
          }

          ClassDef(
            name,
            classNode,
            superType,
            varNodes,
            funcDefs.map(
              f => translateStmt(f)(newTyVars).asInstanceOf[FuncDef]
            ),
            exportLevel
          )
        case surface.TypeAliasStmt(name, tyVars, ty, exportLevel) =>
          val newTyVars: Set[Symbol] = outerTyVars ++ tyVars
          val newTy = monotype(ty)(newTyVars)
          val node = allocate(Some(name), Annot.Fixed(newTy), isTerm = false)
          TypeAliasStmt(name, node, exportLevel)
        // uninteresting cases
        case surface.IfStmt(cond, branch1, branch2) =>
          IfStmt(cond, translateStmt(branch1), translateStmt(branch2))
        case surface.WhileStmt(cond, body) =>
          WhileStmt(cond, translateStmt(body))
        case surface.CommentStmt(text) =>
          CommentStmt(text)
        case surface.BlockStmt(stmts) =>
          BlockStmt(stmts.map(translateStmt))
        case surface.Namespace(name, block) =>
          Namespace(name, translateStmt(block).asInstanceOf[BlockStmt])
        case surface.AssignStmt(lhs, rhs) =>
          AssignStmt(lhs, rhs)
        case surface.ExprStmt(e, isReturn) =>
          ExprStmt(e, isReturn)
      }
    }

    val pStmts = module.stmts.map(s => translateStmt(s)(Set()))
    import module._
    PModule(path, imports, exportStmts, pStmts, mapping.toMap)
  }

  /** project generic types into non-generic types */
  def monotype(ty: GType)(implicit tyVars: Set[Symbol]): GType = {
    ty match {
      case tyVar: TyVar => if (tyVars.contains(tyVar.id)) AnyType else tyVar
      case AnyType      => AnyType
      case f: FuncType =>
        f.from.map(PLangTranslation.monotype) -: PLangTranslation
          .monotype(f.to)
      case o: ObjectType =>
        ObjectType(o.fields.mapValuesNow(PLangTranslation.monotype))
    }
  }
}
