package lambdanet.translation

import ammonite.ops
import lambdanet.{ExportLevel, ProjectPath}
import lambdanet.surface._
import lambdanet.translation.PredicateGraph.{PVar}
import funcdiff.SimpleMath.Extensions._
import lambdanet.ExportStmt.{ExportDefault, ExportOtherModule, ExportSingle}
import lambdanet.ImportStmt._
import lambdanet.translation.PredicateGeneration.PContext

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
  case class NameDef(term: Option[PVar], ty: Option[PVar]) {
    term.foreach(t => assert(t.isTerm))
    ty.foreach(t => assert(t.isType))

    def +(p: PVar): NameDef = {
      if (p.isType) copy(ty = Some(p))
      else copy(term = Some(p))
    }
  }

  object NameDef {
    import cats.Monoid

    val empty: NameDef = NameDef(None, None)

    private def combineOption[T](x: Option[T], y: Option[T]) = {
      y match {
        case Some(v) => Some(v)
        case None    => x
      }
    }

    implicit val NameDefMonoid = new Monoid[NameDef] {
      def empty: NameDef = empty

      def combine(x: NameDef, y: NameDef): NameDef =
        NameDef(combineOption(x.term, y.term), combineOption(x.ty, y.ty))
    }
  }

  case class ModuleExports(
      defaultDefs: NameDef,
      publicSymbols: Map[Symbol, NameDef],
      internalSymbols: Map[Symbol, NameDef],
      nameSpaces: Map[Symbol, ModuleExports]
  )

  def moduleExportsToPContext(exports: ModuleExports): PContext = {
    val namespaces = exports.nameSpaces.mapValuesNow(moduleExportsToPContext)
    val types = exports.publicSymbols.collect {
      case (v, d) if d.ty.nonEmpty => v -> d.ty.get
    }
    val terms = exports.publicSymbols.collect {
      case (v, d) if d.term.nonEmpty => IR.Var(Right(v)) -> d.term.get
    }

    PContext(terms, types, namespaces)
  }

  def resolveImports(
      pVarAllocator: PVar.PVarAllocator,
      modules: Map[ProjectPath, GModule],
      pathMapping: PathMapping,
      maxPropagations: Int = 10
  ): Map[ProjectPath, PContext] = {

    def collectTopLevelDefs(
        stmts: Vector[GStmt]
    ): ModuleExports = {
      var defaults = NameDef.empty
      val publics = mutable.Map[Symbol, NameDef]()
      val privates = mutable.Map[Symbol, NameDef]()
      val nameSpaces = mutable.HashMap[Symbol, ModuleExports]()

      def record(
          name: Symbol,
          level: ExportLevel.Value,
          isTerm: Boolean
      ): Unit = {
        val v = pVarAllocator.newVar(Some(name), !isTerm)
        level match {
          case ExportLevel.Unspecified =>
            privates(name) = privates.getOrElse(name, NameDef.empty) + v
          case ExportLevel.Public =>
            privates(name) = publics.getOrElse(name, NameDef.empty) + v
          case ExportLevel.Default =>
            defaults += v
        }
      }

      stmts.foreach {
        case vd: VarDef =>
          record(vd.name, vd.exportLevel, isTerm = true)
        case fd: FuncDef =>
          record(fd.name, fd.exportLevel, isTerm = true)
        case cd: ClassDef =>
          record(cd.name, cd.exportLevel, isTerm = true)
          record(cd.name, cd.exportLevel, isTerm = false)
        case ts: TypeAliasStmt =>
          record(ts.name, ts.exportLevel, isTerm = false)
        case Namespace(name, block) =>
          nameSpaces(name) = collectTopLevelDefs(block.stmts)
        case _ =>
      }

      ModuleExports(
        defaults,
        publics.toMap,
        (publics ++ privates).toMap,
        nameSpaces.toMap
      )
    }

    def findExports(
        exports: Map[ProjectPath, ModuleExports]
    )(currentPath: ProjectPath, pathToResolve: ProjectPath): ModuleExports = {
      exports.getOrElse(
        pathMapping.map(currentPath / ops.up, pathToResolve),
        throw new Error(
          s"Cannot find source file: '${currentPath / ops.up / pathToResolve}'."
        )
      )
    }

    def propagateExports(
        exports: Map[ProjectPath, ModuleExports]
    ): Map[ProjectPath, ModuleExports] = {
      import cats.implicits._

      exports.map {
        case (thisPath, thisExports) =>
          def resolvePath(relPath: ProjectPath): ModuleExports = {
            findExports(exports)(thisPath, relPath)
          }

          var newDefaults = thisExports.defaultDefs
          var newPublics = thisExports.publicSymbols
          var newInternals = thisExports.internalSymbols
          val module = modules(thisPath)
          module.imports.foreach {
            case ImportSingle(oldName, relPath, newName) =>
              val exports = resolvePath(relPath)
              exports.publicSymbols
                .get(oldName)
                .foreach(defs => {
                  newInternals = newInternals |+| Map(newName -> defs)
                })
            case _ =>
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
                  val defs = thisExports.internalSymbols(oldName)
                  newPublics = newPublics |+| Map(newName -> defs)
              }
            case ExportOtherModule(from) =>
              val toExport = resolvePath(from).publicSymbols
              newPublics = newPublics |+| toExport
            case ExportDefault(newName, Some(s)) =>
              val defs = resolvePath(s).defaultDefs
              newName match {
                case Some(n) =>
                  newPublics = newPublics |+| Map(n -> defs)
                case None =>
                  newDefaults = newDefaults |+| defs
              }
            case ExportDefault(newName, None) =>
              val name = newName.get
              newDefaults = thisExports.internalSymbols(name)
          }
          thisPath -> ModuleExports(
            newDefaults,
            newPublics,
            newInternals,
            thisExports.nameSpaces
          )
      }
    }

    val exports = Iterator
      .iterate(
        modules.mapValuesNow(m => collectTopLevelDefs(m.stmts))
      )(propagateExports)
      .drop(maxPropagations)
      .next()

    modules.values.map { m =>
      def resolvePath(relPath: ProjectPath): ModuleExports = {
        findExports(exports)(m.path, relPath)
      }

      val terms = mutable.HashMap[IR.Var, PVar]()
      val types = mutable.HashMap[Symbol, PVar]()
      val namespaces = mutable.HashMap[Symbol, PContext]()

      def addDefs(name: Symbol, defs: NameDef): Unit = {
        defs.term.foreach { node =>
          val v = IR.Var(Right(name))
          terms(v) = node
        }
        defs.ty.foreach(t => types(name) = t)
      }

      m.imports.foreach {
        case ImportDefault(path, newName) =>
          addDefs(newName, resolvePath(path).defaultDefs)
        case ImportSingle(oldName, relPath, newName) =>
          val ex = resolvePath(relPath)
          ex.publicSymbols.get(oldName) match {
            case Some(defs) => addDefs(newName, defs)
            case None =>
              namespaces(newName) =
                moduleExportsToPContext(ex.nameSpaces(oldName))
          }
        case ImportModule(path, newName) =>
          namespaces(newName) = moduleExportsToPContext(resolvePath(path))
      }

      m.path -> PContext(terms.toMap, types.toMap, namespaces.toMap)
    }.toMap
  }

}
