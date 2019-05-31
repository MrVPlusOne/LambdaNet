package lambdanet.translation

import lambdanet._
import lambdanet.translation.PredicateGraph.{
  PAny,
  PFuncType,
  PNode,
  PNodeAllocator,
  PObjectType,
  PTyVar,
  PType
}
import QLang._
import funcdiff.SimpleMath
import lambdanet.translation.ImportsResolution.{
  ModuleExports,
  NameDef,
  PathMapping
}
import lambdanet.translation.PLang.PModule
import funcdiff.SimpleMath.Extensions._
import lambdanet.surface.GModule

import scala.collection.mutable

object QLang {
  type NodeMapping = PNode => PAnnot

  case class QModule(
      path: ProjectPath,
      stmts: Vector[QStmt],
      mapping: Map[PNode, PAnnot]
  )

  sealed trait QStmt

  case class VarDef(node: PNode, init: QExpr, isConst: Boolean) extends QStmt

  case class AssignStmt(lhs: QExpr, rhs: QExpr) extends QStmt

  case class ExprStmt(e: QExpr, isReturn: Boolean) extends QStmt

  case class IfStmt(cond: QExpr, branch1: QStmt, branch2: QStmt) extends QStmt

  case class WhileStmt(cond: QExpr, body: QStmt) extends QStmt

  case class BlockStmt(stmts: Vector[QStmt]) extends QStmt

  case class FuncDef(
      funcNode: PNode,
      args: Vector[(Symbol, PNode)],
      returnType: PNode,
      body: QStmt
  ) extends QStmt

  case class ClassDef(
      classNode: PNode,
      superType: Option[PType],
      vars: Map[Symbol, (PNode, QExpr)],
      funcDefs: Vector[FuncDef]
  ) extends QStmt

  case class TypeAliasStmt(
      node: PNode,
      exportLevel: ExportLevel.Value
  ) extends QStmt

  sealed trait QExpr

  case class Var(node: PNode) extends QExpr

  case class FuncCall(f: QExpr, args: Vector[QExpr]) extends QExpr

  case class Cast(expr: QExpr, ty: PType) extends QExpr

  case class ObjLiteral(fields: Map[Symbol, QExpr]) extends QExpr

  case class Access(expr: QExpr, field: Symbol) extends QExpr

  case class IfExpr(cond: QExpr, e1: QExpr, e2: QExpr) extends QExpr

}

object QLangTranslation {
  import cats.instances.map._
  import cats.syntax.monoid._

  def fromPModule(module: PModule, ctx: ModuleExports): QModule = {

    def translateExpr(
        expr: surface.GExpr
    )(implicit ctx: ModuleExports): QExpr = {

      def rec(expr: surface.GExpr): Either[ModuleExports, QExpr] = expr match {
        case surface.Var(s) =>
          ctx.nameSpaces.get(s) match {
            case Some(ex) => Left(ex)
            case None     => Right(Var(ctx.internalSymbols(s).term.get))
          }
        case surface.Access(e, l) =>
          rec(e) match {
            case Left(ex) =>
              ex.nameSpaces.get(l) match {
                case Some(ex1) => Left(ex1)
                case None      => Right(Var(ex.internalSymbols(l).term.get))
              }
            case Right(e1) =>
              Right(Access(e1, l))
          }
        case c: surface.Const =>
          Right(constToVar(c, ctx))
        case surface.FuncCall(f, args) =>
          Right(
            FuncCall(
              rec(f).right.get,
              args.map(a => rec(a).right.get).toVector
            )
          )
        case surface.Cast(e, ty) =>
          Right(Cast(rec(e).right.get, resolveType(ty)))
        case surface.ObjLiteral(fields) =>
          Right(ObjLiteral(fields.mapValuesNow(p => rec(p).right.get)))
        case surface.IfExpr(cond, e1, e2) =>
          Right(
            IfExpr(rec(cond).right.get, rec(e1).right.get, rec(e2).right.get)
          )
      }

      rec(expr).right.get
    }

    def collectDefs(
        stmts: Vector[PLang.PStmt]
    )(implicit ctx: ModuleExports): ModuleExports = {
      val defs = stmts.collect {
        case c: PLang.ClassDef =>
          c.name -> NameDef.typeDef(c.classNode)
        case a: PLang.TypeAliasStmt =>
          a.name -> NameDef.typeDef(a.node)
        case d: PLang.VarDef =>
          d.name -> NameDef.termDef(d.node)
        case f: PLang.FuncDef =>
          f.name -> NameDef.termDef(f.funcNode)
      }.toMap

      ctx.copy(internalSymbols = ctx.internalSymbols |+| defs)
    }

    def groupInBlock(stmts: Vector[QStmt]): BlockStmt = {
      stmts match {
        case Vector(b: BlockStmt) => b
        case _                    => BlockStmt(stmts)
      }
    }

    val newMapping = mutable.HashMap[PNode, PAnnot]()
    def mapNode(node: PNode)(implicit ctx: ModuleExports): Unit = {
      val pAnnot = module.mapping(node).map(resolveType)
      newMapping(node) = pAnnot
    }

    def translateStmt(
        stmt: PLang.PStmt
    )(implicit ctx: ModuleExports): Vector[QStmt] =
      SimpleMath.withErrorMessage(s"failed to translate stmt: $stmt") {

        stmt match {
          case PLang.VarDef(_, node, init, isConst, _) =>
            mapNode(node)
            Vector(VarDef(node, translateExpr(init), isConst))
          case PLang.AssignStmt(lhs, rhs) =>
            Vector(AssignStmt(translateExpr(lhs), translateExpr(rhs)))
          case PLang.ExprStmt(e, isReturn) =>
            Vector(ExprStmt(translateExpr(e), isReturn))
          case PLang.IfStmt(cond, branch1, branch2) =>
            Vector(
              IfStmt(
                translateExpr(cond),
                groupInBlock(translateStmt(branch1)),
                groupInBlock(translateStmt(branch2))
              )
            )
          case PLang.WhileStmt(cond, body) =>
            Vector(
              WhileStmt(translateExpr(cond), groupInBlock(translateStmt(body)))
            )
          case _: PLang.CommentStmt => Vector()
          case PLang.BlockStmt(stmts) =>
            val newCtx = collectDefs(stmts)
            Vector(BlockStmt(stmts.flatMap(s => translateStmt(s)(newCtx))))
          case PLang.FuncDef(_, funcNode, args, returnType, body, _) =>
            (Seq(funcNode, returnType) ++ args.map(_._2)).foreach(mapNode)

            val internals1 = ctx.internalSymbols |+| args.map {
              case (s, n) => s -> NameDef.termDef(n)
            }.toMap |+| Map(returnSymbol -> NameDef.termDef(returnType))
            val ctx1 = ctx.copy(internalSymbols = internals1)
            Vector(
              FuncDef(
                funcNode,
                args,
                returnType,
                groupInBlock(translateStmt(body)(ctx1))
              )
            )
          case PLang.ClassDef(_, classNode, superType, vars, funcDefs, _) =>
            mapNode(classNode)
            vars.values.map(_._1).foreach(mapNode)

            val internals1 = ctx.internalSymbols |+|
              Map(thisSymbol -> NameDef.termDef(classNode))
            val ctx1 = ctx.copy(internalSymbols = internals1)
            val vars1 = vars.mapValuesNow {
              case (t, init) => t -> translateExpr(init)(ctx1)
            }
            val funcDefs1 =
              funcDefs.map(f => translateStmt(f)(ctx1).asInstanceOf[FuncDef])
            Vector(
              ClassDef(
                classNode,
                superType.map(t => resolveType(TyVar(t))),
                vars1,
                funcDefs1
              )
            )
          case a: PLang.TypeAliasStmt =>
            mapNode(a.node)
            Vector()
          case PLang.Namespace(name, block) =>
            val ctx1 = ctx.combine(ctx.nameSpaces(name))
            block.stmts.flatMap(s => translateStmt(s)(ctx1))
        }
      }

    val ctx1 = collectDefs(module.stmts)(ctx)
    val stmts1 = module.stmts.flatMap(s => translateStmt(s)(ctx1))
    QModule(module.path, stmts1, newMapping.toMap)
  }

  def fromProject(
      projectModules: Vector[GModule],
      defaultModule: GModule,
      libModules: Vector[GModule],
      pathMapping: PathMapping
  ): (Map[ProjectPath, ModuleExports], Map[ProjectPath, QModule]) = {
    def pass(
        modules: Vector[GModule],
        baseCtx: ModuleExports,
        resolved: Map[ProjectPath, ModuleExports],
        allocator: PNodeAllocator
    ): (Map[ProjectPath, ModuleExports], Map[ProjectPath, QModule]) = {
      val modules1 = modules.map { m =>
        m.path ->
          PLangTranslation.fromGModule(m, allocator)
      }.toMap

      val exports = ImportsResolution.resolveExports(
        modules1,
        resolved,
        pathMapping
      )

      exports -> modules1.mapValuesNow { m =>
        fromPModule(m, baseCtx |+| exports(m.path))
      }
    }

    val libAllocator = new PNodeAllocator(fromLib = true)

    val (baseCtx, baseQModule) = {
      val (baseExports, baseModules) =
        pass(Vector(defaultModule), ModuleExports.empty, Map(), libAllocator)
      baseExports.values.head -> baseModules.values.head
    }

    val (libExports, libQModules) =
      pass(libModules, baseCtx, Map(), libAllocator)

    val projectAllocator = new PNodeAllocator(fromLib = false)
    val (projExports, projQModules) =
      pass(projectModules, baseCtx, libExports, projectAllocator)

    import cats.implicits._

    (libExports |+| projExports) -> (libQModules ++ projQModules)
  }

  private def constToVar(c: surface.Const, ctx: ModuleExports): Var = {
    Var(c.ty match {
      case AnyType   => ctx.internalSymbols(undefinedSymbol).term.get
      case TyVar(id) => ctx.internalSymbols(id).term.get
    })
  }

  private def resolveType(ty: GType)(implicit ctx: ModuleExports): PType = {
    def renameBasicType(symbol: Symbol): Symbol = symbol match {
      case 'number  => 'Number
      case 'string  => 'String
      case 'boolean => 'Boolean
      case 'symbol  => 'Symbol
      case 'void    => 'Void
      case _        => symbol
    }

    // todo: parsing qualified types, handle cases like 'pv => pv'
    ty match {
      case AnyType  => PAny
      case TyVar(n) => PTyVar(ctx.internalSymbols(renameBasicType(n)).ty.get)
      case FuncType(from, to) =>
        PFuncType(from.map(resolveType).toVector, resolveType(to))
      case ObjectType(fields) =>
        PObjectType(fields.mapValuesNow(resolveType))
    }
  }
}
