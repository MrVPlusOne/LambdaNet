package lambdanet.translation

import ammonite.ops
import ammonite.ops.RelPath
import funcdiff.SimpleMath
import lambdanet.{ExportLevel, ImportStmt, ProjectPath, ReferencePath}
import lambdanet.translation.PLang._
import lambdanet.translation.PredicateGraph.{PNode, PNodeAllocator}
import funcdiff.SimpleMath.Extensions._
import lambdanet.ExportStmt._
import lambdanet.ImportStmt._
import lambdanet.utils.PrefixMap

import scala.collection.mutable

object ImportsResolution {
  import cats.Monoid
  import cats.instances.all._
  import cats.syntax.foldable._
  import cats.syntax.monoid._

  trait PathMapping {
    def aliases: Map[ProjectPath, ProjectPath]

    private type PMap = PrefixMap[String, ProjectPath]
    private lazy val aliasMap: PMap = {
      val prefixMap = new PrefixMap[String, ProjectPath]()
      aliases.foreach {
        case (path, mappedTo) =>
          prefixMap.update(path.segments.toList, mappedTo)
      }
      prefixMap
    }

    /** If the path contains aliases, transforms it into
      * the actual path */
    def alias(path: ProjectPath): ProjectPath = {
      @scala.annotation.tailrec
      def rec(tree: PMap, path: List[String]): Option[ProjectPath] =
        path match {
          case List() => tree.value
          case h :: path1 =>
            tree.suffixes.get(h) match {
              case None =>
                tree.value.map { _ / path }
              case Some(tree1) => rec(tree1, path1)
            }
        }
      rec(aliasMap, path.segments.toList).getOrElse(path)
    }

    def map(currentDir: ProjectPath, pathToResolve: ProjectPath): ProjectPath
  }

  object PathMapping {
    def empty: PathMapping = new PathMapping {
      def map(
          currentDir: ProjectPath,
          pathToResolve: ProjectPath
      ): ProjectPath = {
        throw new Error(
          s"Could not resolve path $pathToResolve in directory '$currentDir'"
        )
      }

      def aliases: Map[ProjectPath, ProjectPath] = Map()
    }
  }

  /** Type and term definitions that are associated with a symbol */
  @SerialVersionUID(1)
  case class NameDef(
      term: Option[PNode],
      ty: Option[PNode],
      namespace: Option[ModuleExports]
  ) {
    term.foreach(t => assert(t.isTerm))
    ty.foreach(t => assert(t.isType))

    def nonEmpty: Boolean = term.nonEmpty || ty.nonEmpty || namespace.nonEmpty
    def +(p: PNode): NameDef = {
      if (p.isType) copy(ty = Some(p))
      else copy(term = Some(p))
    }

    override def toString: String = {
      Seq(
        term.map("term: " + _),
        ty.map("ty: " + _),
        namespace.map(_ => "namespace")
      ).map(_.getOrElse("")).mkString("[", ", ", "]")
    }

  }

  object NameDef {

    def makeUnknownDef(allocator: PNodeAllocator): NameDef = {
      require(allocator.forLib)
      NameDef(
        Some(allocator.newNode(None, isType = false)),
        Some(allocator.newNode(None, isType = true)),
        None
      )
    }

    // fixme: handle type mapping
    val unknownDef: NameDef = {
      val libAllocator = new PNodeAllocator(forLib = true)
      libAllocator.unknownDef
    }

    val empty: NameDef = NameDef(None, None, None)

    def fromPNode(n: PNode): NameDef = {
      if (n.isType) typeDef(n) else termDef(n)
    }

    def termDef(n: PNode) = NameDef(Some(n), None, None)
    def typeDef(n: PNode) = NameDef(None, Some(n), None)
    def namespaceDef(namespace: ModuleExports) =
      NameDef(None, None, Some(namespace))

    import lambdanet.combineOption
    implicit val NameDefMonoid: Monoid[NameDef] = new Monoid[NameDef] {
      def empty: NameDef = NameDef.empty

      def combine(x: NameDef, y: NameDef): NameDef =
        NameDef(
          combineOption(x.term, y.term),
          combineOption(x.ty, y.ty),
          x.namespace |+| y.namespace
        )
    }
  }

  @SerialVersionUID(1)
  case class ModuleExports(
      defaultDefs: NameDef = NameDef.empty,
      publicSymbols: Map[Symbol, NameDef] = Map(),
      internalSymbols: Map[Symbol, NameDef] = Map()
  ) {
    lazy val publicNamespaces: Map[Symbol, ModuleExports] =
      publicSymbols.collect {
        case (s, NameDef(_, _, Some(m))) =>
          s -> m
      }

    def getNamespace(qualifiedName: Vector[Symbol]): ModuleExports = {
      getNamespaceOpt(qualifiedName).get
    }

    def getNamespaceOpt(
        qualifiedName: Vector[Symbol]
    ): Option[ModuleExports] = {
      if (qualifiedName.isEmpty) Some(this)
      else
        publicSymbols
          .get(qualifiedName.head)
          .flatMap(_.namespace.flatMap {
            _.getNamespaceOpt(qualifiedName.tail)
          })
    }

    override def toString: String = {
      val all = publicSymbols ++ internalSymbols
      all.keys
        .map { k =>
          val prefix =
            if (publicSymbols.contains(k))
              if (internalSymbols.contains(k)) "public"
              else "public but not internal"
            else if (internalSymbols.contains(k)) "internal"
            else throw new Error("not possible")
          s"($prefix)$k ${all(k)}"
        }
        .mkString("ModuleExports(", "; ", ")")
    }

  }

  object ModuleExports {
    import cats.Monoid
    import cats.implicits._

    implicit val ModuleExportsMonoid: Monoid[ModuleExports] =
      (
        Monoid[NameDef],
        Monoid[Map[Symbol, NameDef]],
        Monoid[Map[Symbol, NameDef]]
      ).imapN(ModuleExports.apply)(
        ModuleExports.unapply(_).get
      )

    val empty: ModuleExports = ModuleExportsMonoid.empty

  }

  sealed trait ResolutionError extends Error
  case class SourceFileMissingError(path: ProjectPath, ctx: ModuleExports)
      extends ResolutionError {
    override def toString: String =
      s"Source file missing for path: $path, ctx: $ctx"
  }
  case class ImportSingleNotResolved(
      s: ImportStmt,
      inside: ProjectPath,
      exports: ModuleExports
  ) extends ResolutionError {
    override def toString: String =
      s"Import single not resolved inside module '$inside', statement: $s, exports available: $exports"
  }

  trait ErrorHandler {
    def sourceFileMissing(path: ProjectPath, ctx: ModuleExports): ModuleExports

    def importSymbolsNotResolved(
        error: ImportSingleNotResolved
    ): Unit

  }

  object ErrorHandler {
    def recoveryHandler() = new RecoveryHandler()

    def throwError(): ErrorHandler = new ErrorHandler {
      def sourceFileMissing(
          path: ProjectPath,
          ctx: ModuleExports
      ): ModuleExports = {
        throw SourceFileMissingError(path, ctx)
      }

      def importSymbolsNotResolved(error: ImportSingleNotResolved): Unit =
        throw error
    }
  }

  class RecoveryHandler extends ErrorHandler {
    val errors: mutable.HashSet[ResolutionError] = mutable.HashSet()

    def sourceFileMissing(
        path: ProjectPath,
        ctx: ModuleExports
    ): ModuleExports = {
      errors += SourceFileMissingError(path, ctx)
      ModuleExports.empty
    }

    def importSymbolsNotResolved(error: ImportSingleNotResolved): Unit = {
      errors += error
    }
  }

  /** `resolvedModules` should also include the namespaces in the baseCtx */
  def resolveExports(
      modulesToResolve: Seq[PModule],
      resolvedModules: Map[ProjectPath, ModuleExports],
      pathMapping: PathMapping,
      defaultPublicMode: Boolean,
      errorHandler: ErrorHandler,
      devDependencies: Set[ProjectPath],
      maxIterations: Int = 10
  ): Map[ProjectPath, ModuleExports] = {

    def collectTopLevelDefs(
        stmts: Vector[PStmt]
    ): ModuleExports = {
      var defaults = NameDef.empty
      var publics = Map[Symbol, NameDef]()
      var all = Map[Symbol, NameDef]()

      def record(
          node: PNode,
          level: ExportLevel.Value
      ): Unit = {
        val name = node.nameOpt.get
        all |+|= Map(name -> NameDef.fromPNode(node))
        level match {
          case ExportLevel.Unspecified =>
            if (defaultPublicMode)
              publics |+|= Map(name -> NameDef.fromPNode(node))
          case ExportLevel.Public =>
            publics |+|= Map(name -> NameDef.fromPNode(node))
          case ExportLevel.Default =>
            defaults += node
        }
      }

      stmts.foreach {
        case vd: VarDef =>
          record(vd.node, vd.exportLevel)
        case fd: FuncDef =>
          record(fd.funcNode, fd.exportLevel)
        case cd: ClassDef =>
          record(cd.classNode, cd.exportLevel)
        case ts: TypeAliasStmt =>
          record(ts.node, ts.exportLevel)
        case Namespace(name, block, level) =>
          val nd = NameDef.namespaceDef(collectTopLevelDefs(block.stmts))
          val rhs =
            Map(name -> nd)
          all |+|= rhs
          level match {
            case ExportLevel.Unspecified =>
              if (defaultPublicMode)
                publics |+|= rhs
            case ExportLevel.Public =>
              publics |+|= rhs
            case ExportLevel.Default =>
              defaults |+|= nd
          }
        case _ =>
      }

      ModuleExports(defaults, publics, all)
    }

    def linkIndexFiles(
        exports: Map[ProjectPath, ModuleExports]
    ): Map[ProjectPath, ModuleExports] = {
      exports ++ exports.keys.filter(_.last == "index").map { path =>
        (path / ops.up) -> exports(path)
      }
    }

    /** Assumes `linkIndexFiles` is already called on `exports` */
    def propagateExports(
        exports: Map[ProjectPath, ModuleExports],
        shouldReportError: Boolean
    ): Map[ProjectPath, ModuleExports] = {
      import ModuleExports._

      def failToResolve(
          s: ImportStmt,
          inside: ProjectPath,
          exports: ModuleExports
      ): Unit = {
        if (shouldReportError)
          errorHandler.importSymbolsNotResolved(
            ImportSingleNotResolved(s, inside, exports)
          )
      }

      val exports1 = modulesToResolve.map { module =>
        val thisPath = module.path
        val thisExports = exports(thisPath)
        def resolvePath(ref: ReferencePath): ModuleExports = {
          val path =
            if (ref.isRelative)
              pathMapping.alias(thisPath / ops.up / ref.path)
            else {
              ref.path.segments match {
                case Vector(name) =>
                  // could also import from a namespace defined in the current module
                  thisExports.internalSymbols.get(Symbol(name)).collect {
                    case d if d.namespace.nonEmpty =>
                      return d.namespace.get
                  }
                  if (devDependencies.contains(RelPath(name))) {
                    return ModuleExports.empty
                  }
                case _ =>
              }
              resolvedModules.get(ref.path).foreach(return _)
              pathMapping.map(thisPath / ops.up, ref.path)
            }
          exports.getOrElse(
            path,
            errorHandler.sourceFileMissing(path, thisExports)
          )
        }

        def collectImports(stmt: PStmt): ModuleExports =
          SimpleMath.withErrorMessage(s"In import stmt: $stmt") {
            stmt match {
              case PImport(content) =>
                val exports = resolvePath(content.path)
                content match {
                  case i: ImportSingle =>
//                    def tryExportEquals =
//                      for {
//                        exEq <- exports.publicSymbols.get('$ExportEquals)
//                        ns <- exEq.namespace
//                        r <- ns.publicSymbols.get(i.oldName)
//                      } yield r

                    exports.publicSymbols.get(i.oldName) match {
                      case Some(defs) =>
                        ModuleExports(
                          internalSymbols = Map(i.newName -> defs)
                        )
                      case None =>
                        if (exports != ModuleExports.empty)
                          failToResolve(i, thisPath, exports)
                        ModuleExports(
                          internalSymbols = Map(i.newName -> NameDef.unknownDef)
                        )
                    }
                  case i: ImportModule =>
                    ModuleExports(
                      internalSymbols =
                        Map(i.newName -> NameDef.namespaceDef(exports))
                    )
                  case i: ImportDefault =>
                    ModuleExports(
                      internalSymbols = Map(i.newName -> exports.defaultDefs)
                    )
                }
              case Namespace(name, block, _) =>
                val df =
                  NameDef.namespaceDef(
                    Monoid.combineAll(block.stmts.map(collectImports))
                  )
                ModuleExports(internalSymbols = Map(name -> df))
              case _ => ModuleExports.empty
            }
          }

        def collectExports(ctx: ModuleExports, stmt: PStmt): ModuleExports = {
          val thisExports = ctx
          stmt match {
            case PExport(content) =>
              content match {
                case ExportSingle(oldName, newName, from) =>
                  from
                    .map(resolvePath(_).publicSymbols)
                    .getOrElse(thisExports.internalSymbols)
                    .get(oldName)
                    .map(
                      defs =>
                        ModuleExports(publicSymbols = Map(newName -> defs))
                    )
                    .combineAll
                case ExportOtherModule(from) =>
                  val toExport = resolvePath(from).publicSymbols
                  ModuleExports(publicSymbols = toExport)
                case ExportDefault(newName, Some(s)) =>
                  val defs = resolvePath(s).defaultDefs
                  newName match {
                    case Some(n) =>
                      ModuleExports(publicSymbols = Map(n -> defs))
                    case None =>
                      ModuleExports(defaultDefs = defs)
                  }
                case ExportDefault(newName, None) =>
                  val name = newName.get
                  ModuleExports(
                    defaultDefs = thisExports.internalSymbols(name)
                  )
              }
            case Namespace(name, block, _) =>
              val ctx1 = thisExports |+|
                thisExports
                  .internalSymbols(name)
                  .namespace
                  .get
              val df =
                NameDef.namespaceDef(
                  block.stmts.map(s => collectExports(ctx1, s)).combineAll
                )
              ModuleExports(internalSymbols = Map(name -> df))
            case NamespaceAliasStmt(Symbol("$ExportEquals"), rhs) =>
              ctx
                .getNamespaceOpt(rhs)
                .getOrElse(ModuleExports.empty)
            case _ => ModuleExports.empty
          }
        }

        SimpleMath.withErrorMessage(s"In module '${module.path}'") {
          val imported = thisExports |+|
            module.stmts.map(collectImports).combineAll

          val result = imported |+|
            module.stmts.map(s => collectExports(imported, s)).combineAll
          thisPath -> result
        }
      }.toMap

      linkIndexFiles(exports1)
    }

    var i = 0
    Iterator
      .iterate(
        linkIndexFiles(modulesToResolve.map { m =>
          m.path -> collectTopLevelDefs(m.stmts)
        }.toMap)
      ) { ex =>
        i += 1
        propagateExports(ex, i == maxIterations)
      }
      .drop(maxIterations)
      .next()
  }

}
