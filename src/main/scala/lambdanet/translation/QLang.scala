package lambdanet.translation

import lambdanet._
import lambdanet.translation.PredicateGraph.{
  PAny,
  PFuncType,
  PNode,
  PNodeAllocator,
  PObjectType,
  PTyVar,
  PType,
}
import QLang._
import ammonite.ops.RelPath
import funcdiff.SimpleMath
import lambdanet.translation.ImportsResolution.{
  ErrorHandler,
  ModuleExports,
  NameDef,
  PathMapping,
}
import lambdanet.translation.PLang.PModule
import lambdanet.Surface.{GModule, GStmt}
import lambdanet.utils.ProgramParsing
import lambdanet._
import scala.collection.mutable

object QLang {
  type NodeMapping = PNode => PAnnot

  case class QModule(
      path: ProjectPath,
      stmts: Vector[QStmt],
      mapping: Map[PNode, PAnnot],
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
      body: QStmt,
  ) extends QStmt

  case class ClassDef(
      classNode: PNode,
      superTypes: Set[PTyVar],
      vars: Map[Symbol, PNode],
      funcDefs: Map[Symbol, FuncDef],
  ) extends QStmt

  sealed trait QExpr {
    var tyAnnot: PAnnot = _

    override def toString: String = this match {
      case Var(n) => n.toString
      case FuncCall(f, args) =>
        s"$f${args.mkString("(", ",", ")")}"
      case Cast(expr, ty) =>
        s"$expr as $ty"
      case ObjLiteral(fields) =>
        fields.map { case (n, t) => s"${n.name}: $t" }.mkString("{", ",", "}")
      case Access(l, r) =>
        s"$l.${r.name}"
      case IfExpr(cond, e1, e2) =>
        s"($cond ? $e1 : $e2)"
    }
  }

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
      Map[PNode, PAnnot],
  ) = {
    import ammonite.ops._

    val root = pwd / RelPath("data/libraries")
    val files = ls
      .rec(root)
      .filter(f => f.last.endsWith(".d.ts"))
      .map(f => f.relativeTo(root))
    val allStmts =
      ProgramParsing
        .parseGModulesFromFiles(files, root)
        .map(_.stmts)
        .reduce(_ ++ _)
    val m =
      GModule(RelPath("default-imports"), allStmts, isDeclarationFile = true)
    val additionalDefs = JSExamples.specialVars.map {
      case (v, t) =>
        Surface.VarDef(
          v,
          Annot.User(t, inferred = false),
          None,
          isConst = true,
          ExportLevel.Unspecified,
        )
    }
    val defaultModule = m.copy(stmts = m.stmts ++ additionalDefs)
    val libAllocator = new PNodeAllocator(forLib = true)
    val pModule = PLangTranslation.fromGModule(defaultModule, libAllocator)

    val errorHandler = ErrorHandler.alwaysThrow
    val exports = ImportsResolution
      .resolveExports(
        ImportsResolution.ProjectInfo(
          Seq(pModule),
          ModuleExports.empty,
          Map(),
          PathMapping.empty,
          defaultPublicMode = true,
          devDependencies = Set(),
        ),
        errorHandler = errorHandler,
        libAllocator.newUnknownDef,
      )
      .values
      .head
    val qModule = fromPModule(pModule, exports)
    val unknownTerm = NameDef.unknownDef.term.get
    val unknownType = NameDef.unknownDef.ty.get
    val mapping = qModule.mapping ++
      Map(unknownTerm -> Annot.Missing, unknownType -> Annot.Missing)

    (exports, libAllocator, mapping)
  }

  def fromProject(
      modules: Vector[GModule],
      baseCtx: ModuleExports,
      resolved: Map[ProjectPath, ModuleExports],
      allocator: PNodeAllocator,
      pathMapping: PathMapping,
      devDependencies: Set[ProjectPath],
      errorHandler: ErrorHandler,
  ): Vector[QModule] = {
    // first, merge all .d.ts files in the project
    val dPath = RelPath("projectDeclarations")
    val declarations = {
      val m = GModule(
        dPath,
        modules
          .filter(_.isDeclarationFile)
          .flatMap(_.stmts),
        isDeclarationFile = true,
      )
      PLangTranslation.fromGModule(m, allocator)
    }

    val resolved1 = baseCtx.publicNamespaces.map {
      case (k, m) => (k: RelPath) -> m
    } ++ resolved

    val dExports = ImportsResolution.resolveExports(
      ImportsResolution.ProjectInfo(
        Seq(declarations),
        baseCtx,
        resolved1,
        pathMapping,
        defaultPublicMode = true,
        devDependencies,
      ),
      errorHandler = errorHandler,
      allocator.newUnknownDef,
    )(dPath)

    // then, make the modules in the .d.ts files available for import
    val resolved2 = dExports.publicNamespaces.map {
      case (k, m) => RelPath(k.toString()) -> m
    } ++ resolved1

    // resolve the project files
    val modules1 = modules.filterNot(_.isDeclarationFile).map {
      PLangTranslation.fromGModule(_, allocator)
    }
    val exports = ImportsResolution.resolveExports(
      ImportsResolution.ProjectInfo(
        modules1,
        baseCtx,
        resolved2,
        pathMapping,
        defaultPublicMode = false,
        devDependencies,
      ),
      errorHandler = errorHandler,
      allocator.newUnknownDef,
    )

    val projectCtx = baseCtx |+| dExports
    modules1.map { m =>
      SimpleMath.withErrorMessage {
        val gm = modules.find(_.path == m.path).get
        s"GModule source code:\n ${gm.prettyPrint}"
      } {
        fromPModule(m, projectCtx |+| exports(m.path))
      }
    }
  }

  def fromPModule(module: PModule, ctx: ModuleExports): QModule =
    SimpleMath.withErrorMessage(s"In PModule ${module.path}") {

      def translateExpr(
          expr: Surface.GExpr,
      )(implicit ctx: ModuleExports): QExpr = {

        def asTerm(value: Either[ModuleExports, QExpr]): QExpr =
          value.right.get

        def rec(
            expr: Surface.GExpr,
            canBeNamespace: Boolean,
        ): Either[ModuleExports, QExpr] =
          SimpleMath.withErrorMessage(s"In expr: $expr") {
            def processNd(nd: NameDef, canBeNamespace: Boolean) = {
              if (canBeNamespace) {
                nd.namespace
                  .map(Left.apply)
                  .getOrElse {
                    val n = nd.term.getOrElse(
                      throw new Error(
                        s"Empty namedDef encountered in expr: $expr",
                      ),
                    )
                    Right(Var(n))
                  }
              } else {
                nd.term
                  .map(t => Right(Var(t)))
                  .getOrElse(
                    Right(Var(ctx.internalSymbols(undefinedSymbol).term.get)),
                  )
              }
            }

            (expr match {
              case Surface.Var(s) =>
                val nd = ctx.internalSymbols.getOrElse(s, {
                  printWarning(
                    s"Unable to resolve var: ${s.name}",
                  )
                  NameDef.unknownDef
                })
                processNd(nd, canBeNamespace)
              case a @ Surface.Access(e, l) =>
                rec(e, canBeNamespace = true) match {
                  case Left(ModuleExports.empty) =>
                    Right(Var(NameDef.unknownDef.term.get)) // todo: might need to double check this
                  case Left(ex) =>
                    val nd = ex.internalSymbols.getOrElse(l, {
                      if (l != 'prototype) {
                        printWarning(s"Unable to resolve namespace access: $a")
                      }
                      return Right(Var(NameDef.unknownDef.term.get))

                    })
                    processNd(nd, canBeNamespace)
                  case Right(e1) =>
                    Right(Access(e1, l))
                }
              case c: Surface.Const =>
                Right(constToVar(c, ctx))
              case Surface.FuncCall(f, args) =>
                Right(
                  FuncCall(
                    asTerm(rec(f, canBeNamespace = false)),
                    args
                      .map(a => asTerm(rec(a, canBeNamespace = false)))
                      .toVector,
                  ),
                )
              case Surface.Cast(e, ty) =>
                Right(
                  Cast(asTerm(rec(e, canBeNamespace = false)), resolveType(ty)),
                )
              case Surface.ObjLiteral(fields) =>
                Right(
                  ObjLiteral(
                    fields
                      .mapValuesNow(
                        p => asTerm(rec(p, canBeNamespace = false)),
                      ),
                  ),
                )
              case Surface.IfExpr(cond, e1, e2) =>
                Right(
                  IfExpr(
                    asTerm(rec(cond, canBeNamespace = false)),
                    asTerm(rec(e1, canBeNamespace = false)),
                    asTerm(rec(e2, canBeNamespace = false)),
                  ),
                )
            }).tap {
              case Right(e) =>
                e.tyAnnot = expr.tyAnnot
                  .getOrElse(Annot.Missing)
                  .map(resolveType(_))
              case _ =>
            }
          }

        asTerm(rec(expr, canBeNamespace = false))
      }

      def collectDefs(
          stmts: Vector[PLang.PStmt],
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
          case a: PLang.NamespaceAliasStmt
              if ctx.getNamespaceOpt(a.rhs).nonEmpty =>
            a.name -> NameDef.namespaceDef(ctx.getNamespace(a.rhs))
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
          stmt: PLang.PStmt,
      )(implicit ctx: ModuleExports): Vector[QStmt] =
        SimpleMath.withErrorMessage(s"failed to translate stmt: $stmt") {

          stmt match {
            case PLang.VarDef(_, node, init, isConst, _) =>
              mapNode(node)
              init.map{ i =>
                VarDef(node, translateExpr(i), isConst)
              }.toVector
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
                  groupInBlock(translateStmt(branch2)),
                ),
              )
            case PLang.WhileStmt(cond, body) =>
              Vector(
                WhileStmt(
                  translateExpr(cond),
                  groupInBlock(translateStmt(body)),
                ),
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
                  groupInBlock(translateStmt(body)(ctx1)),
                ),
              )
            case PLang.ClassDef(
                _,
                classNode,
                thisNode,
                superTypes,
                vars,
                funcDefs,
                _,
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
                        .asInstanceOf[FuncDef],
                  )
                  .toMap
              Vector(
                ClassDef(
                  classNode,
                  superTypes.map(
                    t => resolveType(TyVar(t)).asInstanceOf[PTyVar],
                  ),
                  vars,
                  funcDefs1,
                ),
              )
            case a: PLang.TypeAliasStmt =>
              mapNode(a.node)
              if (a.superTypes.nonEmpty) {
                val superTypes = a.superTypes.map(
                  t => resolveType(TyVar(t)).asInstanceOf[PTyVar],
                )
                Vector(ClassDef(a.node, superTypes, Map(), Map()))
              } else Vector()
            case PLang.Namespace(name, block, _) =>
              val ctx1 = ctx |+| ctx.internalSymbols(name).namespace.get
              block.stmts.flatMap(s => translateStmt(s)(ctx1))
            case _: PLang.PImport            => Vector()
            case _: PLang.PExport            => Vector()
            case _: PLang.NamespaceAliasStmt => Vector()
          }
        }

      val ctx1 = collectDefs(module.stmts)(ctx)
      val stmts1 = module.stmts.flatMap(s => translateStmt(s)(ctx1))
      QModule(module.path, stmts1, newMapping.toMap)
    }

  private def constToVar(c: Surface.Const, ctx: ModuleExports): Var =
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
    'array -> 'Array,
    'bigint -> 'BigInt,
  )

  private def resolveType(ty: GType)(implicit ctx: ModuleExports): PType =
    SimpleMath.withErrorMessage(s"Failed to resolve type: $ty") {
      ty match {
        case AnyType => PAny
        case TyVar(n) =>
          def cantResolve() = {
            printWarning(s"Unable to resolve type: ${n.name}")
            NameDef.unknownDef
          }
          if(n.name == "...") // some weird corner case
            return PAny

          val nameSegs = n.name.split("\\.").map(Symbol.apply).toVector
          val tyOpt = if (nameSegs.length == 1) {
            val renamed = basicTypeRenaming.getOrElse(n, n)
            ctx.internalSymbols.getOrElse(renamed, cantResolve())
          } else {
            assert(nameSegs.length > 1, s"empty name segs?: $nameSegs")
            val newCtx = nameSegs.init.foldLeft(Option(ctx)) {
              case (Some(c), seg) =>
                c.internalSymbols.get(seg).flatMap(_.namespace)
              case (None, _) => None
            }

            newCtx
              .flatMap(_.internalSymbols.get(nameSegs.last))
              .getOrElse(cantResolve())
          }
          tyOpt.ty.getOrElse(cantResolve().ty.get).pipe(PTyVar)
        case FuncType(from, to) =>
          PFuncType(from.map(resolveType).toVector, resolveType(to))
        case ObjectType(fields) =>
          PObjectType(fields.mapValuesNow(resolveType))
      }
    }

}
