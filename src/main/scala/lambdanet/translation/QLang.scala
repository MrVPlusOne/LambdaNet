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
import lambdanet.surface.{GModule, JSExamples}
import lambdanet.utils.ProgramParsing

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

  case class ExprStmt(e: QExpr) extends QStmt

  case class ReturnStmt(e: QExpr, ret: PNode) extends QStmt

  case class IfStmt(cond: QExpr, branch1: QStmt, branch2: QStmt) extends QStmt

  case class WhileStmt(cond: QExpr, body: QStmt) extends QStmt

  case class BlockStmt(stmts: Vector[QStmt]) extends QStmt

  case class FuncDef(
      funcNode: PNode,
      args: Vector[PNode],
      returnType: PNode,
      body: QStmt
  ) extends QStmt

  case class ClassDef(
      classNode: PNode,
      superType: Option[PTyVar],
      vars: Map[Symbol, PNode],
      funcDefs: Map[Symbol, FuncDef]
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

  /** parse the typescript standard library definitions that are automatically
    * imported in other projects by default */
  def parseDefaultModule(): (
      ModuleExports,
      PNodeAllocator,
      Map[PNode, PAnnot]
  ) = {
    import ammonite.ops._

    val root = pwd / RelPath("data/libraries")
    val file = "default.lib.d.ts"
    val Vector(m) =
      ProgramParsing.parseGModulesFromFiles(Seq(file), root)
    val additionalDefs = JSExamples.specialVars.map {
      case (v, t) =>
        surface.VarDef(
          v,
          Annot.User(t),
          surface.Var(undefinedSymbol),
          isConst = true,
          ExportLevel.Unspecified
        )
    }
    val defaultModule = m.copy(stmts = m.stmts ++ additionalDefs)
    val libAllocator = new PNodeAllocator(forLib = true)
    val pModule = PLangTranslation.fromGModule(defaultModule, libAllocator)

    val exports = ImportsResolution
      .resolveExports(
        Map(pModule.path -> pModule),
        Map(),
        PathMapping.identity
      )
      .values
      .head
    val qModule = fromPModule(pModule, exports)

    (exports, libAllocator, qModule.mapping)
  }

  def fromProject(
      modules: Vector[GModule],
      baseCtx: ModuleExports,
      resolved: Map[ProjectPath, ModuleExports],
      allocator: PNodeAllocator,
      pathMapping: PathMapping
  ): Map[ProjectPath, QModule] = {
    val modules1 = modules.map { m =>
      m.path ->
        PLangTranslation.fromGModule(m, allocator)
    }.toMap

    val exports = ImportsResolution.resolveExports(
      modules1,
      resolved,
      pathMapping
    )

    modules1.mapValuesNow { m =>
      fromPModule(m, baseCtx |+| exports(m.path))
    }
  }

  def fromPModule(module: PModule, ctx: ModuleExports): QModule =
    SimpleMath.withErrorMessage(s"In PModule ${module.path}") {

      def translateExpr(
          expr: surface.GExpr
      )(implicit ctx: ModuleExports): QExpr = {

        def asTerm(value: Either[ModuleExports, QExpr]): QExpr = value match {
          case Left(_) =>
            // todo: check if there is a better way to handle namespace expressions
            Var(ctx.internalSymbols(undefinedSymbol).term.get)
          case Right(e) => e
        }

        def rec(expr: surface.GExpr): Either[ModuleExports, QExpr] =
          SimpleMath.withErrorMessage(s"In expr: $expr") {
            expr match {
              case surface.Var(s) =>
                val nd = ctx.internalSymbols(s)
                nd.namespace match {
                  case Some(ex) => Left(ex)
                  case None     => Right(Var(nd.term.get))
                }
              case surface.Access(e, l) =>
                rec(e) match {
                  case Left(ex) =>
                    val nd = ex.internalSymbols(l)
                    nd.namespace match {
                      case Some(ex1) => Left(ex1)
                      case None      => Right(Var(nd.term.get))
                    }
                  case Right(e1) =>
                    Right(Access(e1, l))
                }
              case c: surface.Const =>
                Right(constToVar(c, ctx))
              case surface.FuncCall(f, args) =>
                Right(
                  FuncCall(
                    asTerm(rec(f)),
                    args.map(a => asTerm(rec(a))).toVector
                  )
                )
              case surface.Cast(e, ty) =>
                Right(Cast(asTerm(rec(e)), resolveType(ty)))
              case surface.ObjLiteral(fields) =>
                Right(ObjLiteral(fields.mapValuesNow(p => asTerm(rec(p)))))
              case surface.IfExpr(cond, e1, e2) =>
                Right(
                  IfExpr(
                    asTerm(rec(cond)),
                    asTerm(rec(e1)),
                    asTerm(rec(e2))
                  )
                )
            }
          }

        asTerm(rec(expr))
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
              val e1 = translateExpr(e)
              val s =
                if (isReturn)
                  ReturnStmt(e1, ctx.internalSymbols(returnSymbol).term.get)
                else ExprStmt(e1)
              Vector(s)
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
                WhileStmt(
                  translateExpr(cond),
                  groupInBlock(translateStmt(body))
                )
              )
            case _: PLang.CommentStmt => Vector()
            case PLang.BlockStmt(stmts) =>
              val newCtx = collectDefs(stmts)
              Vector(BlockStmt(stmts.flatMap(s => translateStmt(s)(newCtx))))
            case PLang.FuncDef(_, funcNode, args, returnType, body, _) =>
              val args1 = args.map(_._2)
              (Seq(funcNode, returnType) ++ args1).foreach(mapNode)

              val internals1 = ctx.internalSymbols |+| args.map {
                case (s, n) => s -> NameDef.termDef(n)
              }.toMap |+| Map(returnSymbol -> NameDef.termDef(returnType))
              val ctx1 = ctx.copy(internalSymbols = internals1)
              Vector(
                FuncDef(
                  funcNode,
                  args1,
                  returnType,
                  groupInBlock(translateStmt(body)(ctx1))
                )
              )
            case PLang.ClassDef(
                _,
                classNode,
                thisNode,
                superType,
                vars,
                funcDefs,
                _
                ) =>
              mapNode(classNode)
              mapNode(thisNode)
              vars.values.foreach(mapNode)

              val thisDef = NameDef.termDef(thisNode)
              val internals1 = ctx.internalSymbols |+|
                Map(thisSymbol -> thisDef, superSymbol -> thisDef)
              val ctx1 = ctx.copy(internalSymbols = internals1)
              val funcDefs1 =
                funcDefs
                  .map(
                    f =>
                      f.name -> translateStmt(f)(ctx1).head
                        .asInstanceOf[FuncDef]
                  )
                  .toMap
              Vector(
                ClassDef(
                  classNode,
                  superType.map(
                    t => resolveType(TyVar(t)).asInstanceOf[PTyVar]
                  ),
                  vars,
                  funcDefs1
                )
              )
            case a: PLang.TypeAliasStmt =>
              mapNode(a.node)
              Vector()
            case PLang.Namespace(name, block, _) =>
              val ctx1 = ctx |+| ctx.internalSymbols(name).namespace.get
              block.stmts.flatMap(s => translateStmt(s)(ctx1))
            case _: PLang.PImport => Vector()
            case _: PLang.PExport => Vector()
          }
        }

      val ctx1 = collectDefs(module.stmts)(ctx)
      val stmts1 = module.stmts.flatMap(s => translateStmt(s)(ctx1))
      QModule(module.path, stmts1, newMapping.toMap)
    }

  private def constToVar(c: surface.Const, ctx: ModuleExports): Var =
    SimpleMath.withErrorMessage(s"In constToVar: $c") {
      Var(c.ty match {
        case AnyType => ctx.internalSymbols(undefinedSymbol).term.get
        case TyVar(id) =>
          ctx.internalSymbols(basicTypeRenaming.getOrElse(id, id)).term.get
      })
    }

  private val basicTypeRenaming = Map(
    'number -> 'Number,
    'string -> 'String,
    'boolean -> 'Boolean,
    'symbol -> 'Symbol,
    'void -> 'Void,
    'object -> 'Object,
    'array -> 'Array
  )

  private def resolveType(ty: GType)(implicit ctx: ModuleExports): PType =
    SimpleMath.withErrorMessage(s"Failed to resolve type: $ty") {
      // todo: parsing qualified types, handle cases like 'pv => pv'
      ty match {
        case AnyType => PAny
        case TyVar(n) =>
          val nameSegs = n.name.split("\\.").map(Symbol.apply).toVector
          if (nameSegs.length == 1) {
            val renamed = basicTypeRenaming.getOrElse(n, n)
            PTyVar(ctx.internalSymbols(renamed).ty.get)
          } else {
            assert(nameSegs.length > 1, s"empty name segs?: $nameSegs")
            val newCtx = nameSegs.init.foldLeft(ctx) {
              case (c, seg) => c.internalSymbols(seg).namespace.get
            }
            PTyVar(newCtx.internalSymbols(nameSegs.last).ty.get)
          }
        case FuncType(from, to) =>
          PFuncType(from.map(resolveType).toVector, resolveType(to))
        case ObjectType(fields) =>
          PObjectType(fields.mapValuesNow(resolveType))
      }
    }
}
