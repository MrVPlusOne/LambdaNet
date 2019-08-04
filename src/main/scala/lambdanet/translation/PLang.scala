package lambdanet.translation

import funcdiff.SimpleMath
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
  Surface,
}
import lambdanet.Surface.{GExpr, GModule}
import lambdanet.translation.PLang._
import lambdanet.translation.PredicateGraph.{PNode, PNodeAllocator}

import scala.collection.mutable

/** [[Surface.GStmt]] marked with [[PNode]]s  */
object PLang {

  type NodeMapping = PNode => Option[GType]

  case class PModule(
      path: ProjectPath,
      stmts: Vector[PStmt],
      mapping: Map[PNode, TyAnnot],
  ) {
    val moduleName: String = path.toString()
  }

  sealed trait PStmt

  case class VarDef(
      name: Symbol,
      node: PNode,
      init: Option[GExpr],
      isConst: Boolean,
      exportLevel: ExportLevel.Value,
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
      exportLevel: ExportLevel.Value,
  ) extends PStmt {
    @throws[NoSuchElementException]
    def functionType(implicit mapping: NodeMapping): FuncType = {
      FuncType(args.map(x => mapping(x._2).get).toList, mapping(returnType).get)
    }

  }

  case class ClassDef(
      name: Symbol,
      classNode: PNode,
      thisNode: PNode,
      superTypes: Set[Symbol],
      vars: Map[Symbol, PNode],
      funcDefs: Vector[FuncDef],
      exportLevel: ExportLevel.Value,
  ) extends PStmt {
    @throws[NoSuchElementException]
    def objectType(implicit mapping: NodeMapping): ObjectType = {
      ObjectType(
        vars.mapValuesNow(x => mapping(x).get) ++
          funcDefs.map(fd => fd.name -> fd.functionType),
      )
    }

  }

  case class TypeAliasStmt(
      name: Symbol,
      node: PNode,
      superTypes: Set[Symbol],
      exportLevel: ExportLevel.Value,
  ) extends PStmt

  case class Namespace(name: Symbol, block: BlockStmt, level: ExportLevel.Value)
      extends PStmt

  case class PImport(content: ImportStmt) extends PStmt
  case class PExport(content: ExportStmt) extends PStmt
  case class NamespaceAliasStmt(name: Symbol, rhs: Vector[Symbol]) extends PStmt
}

/**
  * Duties:
  *   1. Getting rid of type variables
  *   2. Assigning [[PNode]]s.
  */
object PLangTranslation {

  def fromGModule(
      module: GModule,
      allocator: PNodeAllocator,
  ): PModule = {
    val mapping = mutable.HashMap[PNode, TyAnnot]()

    def allocate(nameOpt: Option[Symbol], annot: TyAnnot, isTerm: Boolean)(
        implicit outerTyVars: Set[Symbol],
    ): PNode = {
      val v = allocator.newNode(nameOpt, !isTerm)
      mapping(v) = annot.map(monotype)
      v
    }

    def translateStmt(
        stmt: Surface.GStmt,
    )(implicit outerTyVars: Set[Symbol]): PStmt =
      SimpleMath.withErrorMessage(s"failed to translate stmt: $stmt") {
        stmt match {
          case Surface.VarDef(name, annot, init, isConst, exportLevel) =>
            val node = allocate(Some(name), annot, isTerm = true)
            VarDef(name, node, init, isConst, exportLevel)
          case Surface.FuncDef(
              name,
              tyVars,
              args,
              returnType,
              body,
              exportLevel,
              ) =>
            val newTyVars: Set[Symbol] = outerTyVars ++ tyVars
            val funcNode = allocate(Some(name), Annot.Missing, isTerm = true)
            val argNodes = args.map {
              case (n, annot) =>
                n -> allocate(Some(n), annot, isTerm = true)(newTyVars)
            }
            val returnNode =
              allocate(None, returnType, isTerm = true)(newTyVars)
            FuncDef(
              name,
              funcNode,
              argNodes,
              returnNode,
              translateStmt(body)(newTyVars),
              exportLevel,
            )
          case Surface.ClassDef(
              name,
              tyVars,
              superType,
              vars,
              funcDefs,
              exportLevel,
              ) =>
            val newTyVars: Set[Symbol] = outerTyVars ++ tyVars
            val classNode = allocate(Some(name), Annot.Missing, isTerm = false)
            val thisNode = allocate(
              Some(lambdanet.thisSymbol),
              Annot.Fixed(TyVar(name)),
              isTerm = true,
            )
            val varNodes = vars.map {
              case (s, annot) =>
                (s, allocate(Some(s), annot, isTerm = true)(newTyVars))
            }

            ClassDef(
              name,
              classNode,
              thisNode,
              superType,
              varNodes,
              funcDefs.map(
                f => translateStmt(f)(newTyVars).asInstanceOf[FuncDef],
              ),
              exportLevel,
            )
          case Surface.TypeAliasStmt(
              name,
              tyVars,
              ty,
              exportLevel,
              superTypes,
              ) =>
            val newTyVars: Set[Symbol] = outerTyVars ++ tyVars
            val newTy = monotype(ty)(newTyVars)
            val node = allocate(Some(name), Annot.Fixed(newTy), isTerm = false)
            TypeAliasStmt(name, node, superTypes, exportLevel)
          // uninteresting cases
          case Surface.IfStmt(cond, branch1, branch2) =>
            IfStmt(cond, translateStmt(branch1), translateStmt(branch2))
          case Surface.WhileStmt(cond, body) =>
            WhileStmt(cond, translateStmt(body))
          case Surface.CommentStmt(text) =>
            CommentStmt(text)
          case Surface.BlockStmt(stmts) =>
            BlockStmt(stmts.map(translateStmt))
          case Surface.Namespace(name, block, level) =>
            Namespace(name, translateStmt(block).asInstanceOf[BlockStmt], level)
          case Surface.AssignStmt(lhs, rhs) =>
            AssignStmt(lhs, rhs)
          case Surface.ExprStmt(e, isReturn) =>
            ExprStmt(e, isReturn)
          case Surface.GImport(content) =>
            PImport(content)
          case Surface.GExport(content) =>
            PExport(content)
          case Surface.NamespaceAliasStmt(name, rhs) =>
            NamespaceAliasStmt(name, rhs)
        }
      }

    val pStmts = module.stmts.map(s => translateStmt(s)(Set()))
    import module._
    PModule(path, pStmts, mapping.toMap)
  }

  /** projects generic types into non-generic types */
  def monotype(ty: GType)(implicit tyVars: Set[Symbol]): GType = {
    ty match {
      case tyVar: TyVar => if (tyVars.contains(tyVar.id)) AnyType else tyVar
      case AnyType      => AnyType
      case f: FuncType =>
        f.from.map(monotype) -: monotype(f.to)
      case o: ObjectType =>
        ObjectType(o.fields.mapValuesNow(monotype))
    }
  }
}
