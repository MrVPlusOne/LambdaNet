package lambdanet.translation

import ammonite.ops
import funcdiff.SimpleMath
import lambdanet.{ExportLevel, ProjectPath}
import lambdanet.translation.PLang._
import lambdanet.translation.PredicateGraph.{PNode, PNodeAllocator}
import funcdiff.SimpleMath.Extensions._
import lambdanet.ExportStmt._
import lambdanet.ImportStmt._
import lambdanet.surface.{GModule, GStmt, JSExamples}
import lambdanet.translation.PredicatesGeneration.PContext
import lambdanet.utils.ProgramParsing
import lambdanet.utils.ProgramParsing.DeclarationModule

import scala.collection.mutable

object ImportsResolution {

  trait PathMapping {
    def map(currentPath: ProjectPath, pathToResolve: ProjectPath): ProjectPath
  }

  object PathMapping {
    def identity: PathMapping =
      (currentPath: ProjectPath, pathToResolve: ProjectPath) => {
        currentPath / pathToResolve
      }
  }

  /** Type and term definitions that are associated with a symbol */
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
  }

  object NameDef {
    import cats.Monoid

    val empty: NameDef = NameDef(None, None, None)

    def fromPNode(n: PNode): NameDef = {
      if (n.isType) typeDef(n) else termDef(n)
    }

    def termDef(n: PNode) = NameDef(Some(n), None, None)
    def typeDef(n: PNode) = NameDef(None, Some(n), None)
    def namespaceDef(namespace: ModuleExports) =
      NameDef(None, None, Some(namespace))

    implicit val NameDefMonoid: Monoid[NameDef] = new Monoid[NameDef] {
      def empty: NameDef = NameDef.empty

      def combine(x: NameDef, y: NameDef): NameDef =
        NameDef(
          combineOption(x.term, y.term),
          combineOption(x.ty, y.ty),
          combineOption(x.namespace, y.namespace)
        )
    }

    private def combineOption[T](x: Option[T], y: Option[T]) = {
      y match {
        case Some(v) => Some(v)
        case None    => x
      }
    }
  }

  case class ModuleExports(
      defaultDefs: NameDef,
      publicSymbols: Map[Symbol, NameDef],
      internalSymbols: Map[Symbol, NameDef]
  )

  object ModuleExports {
    import cats.Monoid
    import cats.implicits._

    val empty = ModuleExports(NameDef.empty, Map(), Map())

    implicit val ModuleExportsMonoid: Monoid[ModuleExports] =
      new Monoid[ModuleExports] {
        val empty: ModuleExports = ModuleExports.empty

        def combine(x: ModuleExports, y: ModuleExports): ModuleExports = {
          ModuleExports(
            x.defaultDefs |+| y.defaultDefs,
            x.publicSymbols |+| y.publicSymbols,
            x.internalSymbols |+| y.internalSymbols
          )
        }
      }

  }

  def resolveExports(
      modulesToResolve: Map[ProjectPath, PModule],
      resolvedModules: Map[ProjectPath, ModuleExports],
      pathMapping: PathMapping,
      maxIterations: Int = 10
  ): Map[ProjectPath, ModuleExports] = {

    def collectTopLevelDefs(
        stmts: Vector[PStmt]
    ): ModuleExports = {
      import cats.implicits._

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
            case ExportLevel.Public =>
              publics |+|= rhs
            case ExportLevel.Default =>
              defaults |+|= nd
          }
        case _ =>
      }

      ModuleExports(defaults, publics, all)
    }

    def propagateExports(
        exports: Map[ProjectPath, ModuleExports]
    ): (Map[ProjectPath, ModuleExports], Set[Symbol]) = {
      import cats.implicits._

      var unresolved = Set[Symbol]()
      exports.map {
        case (thisPath, thisExports) =>
          def resolvePath(relPath: ProjectPath): ModuleExports = {
            findExports(exports, pathMapping, resolvedModules)(
              thisPath,
              relPath
            )
          }

          var newDefaults = thisExports.defaultDefs
          var newPublics = thisExports.publicSymbols
          var newInternals = thisExports.internalSymbols

          val module = modulesToResolve(thisPath)
          SimpleMath.withErrorMessage(s"In module '${module.path}'") {
            module.imports.foreach {
              case ImportSingle(oldName, relPath, newName) =>
                val exports = resolvePath(relPath)
                exports.publicSymbols
                  .get(oldName) match {
                  case Some(defs) =>
                    newInternals = newInternals |+| Map(newName -> defs)
                  case None => unresolved += oldName
                }
              case ImportModule(path, newName) =>
                val exports = resolvePath(path)
                newInternals |+|= Map(newName -> NameDef.namespaceDef(exports))
              case ImportDefault(path, newName) =>
                val exports = resolvePath(path)
                newInternals |+|= Map(newName -> exports.defaultDefs)
            }

            module.exportStmts.foreach {
              case ExportSingle(oldName, newName, from) =>
                from match {
                  case Some(s) =>
                    resolvePath(s).publicSymbols
                      .get(oldName)
                      .foreach(defs => {
                        newPublics = newPublics |+| Map(newName -> defs)
                      })
                  case None =>
                    thisExports.internalSymbols
                      .get(oldName)
                      .foreach(defs => newPublics |+|= Map(newName -> defs))
                }
              case ExportOtherModule(from) =>
                val toExport = resolvePath(from).publicSymbols
                newPublics = newPublics |+| toExport
              case ExportDefault(newName, Some(s)) =>
                val defs = resolvePath(s).defaultDefs
                newName match {
                  case Some(n) =>
                    newPublics |+|= Map(n -> defs)
                  case None =>
                    newDefaults |+|= defs
                }
              case ExportDefault(newName, None) =>
                val name = newName.get
                newDefaults = thisExports.internalSymbols(name)
            }
            thisPath -> ModuleExports(newDefaults, newPublics, newInternals)
          }
      } -> unresolved
    }

    var lastUnresolved = Set[Symbol]()
    val r = Iterator
      .iterate(
        modulesToResolve.map {
          case (p, m) =>
            p -> collectTopLevelDefs(m.stmts)
        }
      ) { ex =>
        val (ex1, unresolved) = propagateExports(ex)
        lastUnresolved = unresolved
        ex1
      }
      .drop(maxIterations)
      .next()
    assert(lastUnresolved.isEmpty, s"Unresolved symbols: $lastUnresolved")
    r
  }

  private def findExports(
      exports: Map[ProjectPath, ModuleExports],
      pathMapping: PathMapping,
      resolvedModules: Map[ProjectPath, ModuleExports]
  )(currentPath: ProjectPath, pathToResolve: ProjectPath): ModuleExports = {
    val path = pathMapping.map(currentPath / ops.up, pathToResolve)
    resolvedModules.getOrElse(
      path,
      exports.getOrElse(
        path,
        throw new Error(
          s"Cannot find source file: '${currentPath / ops.up / pathToResolve}'."
        )
      )
    )
  }

}
