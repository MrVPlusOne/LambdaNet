package infer

import funcdiff.SimpleMath
import funcdiff.SimpleMath.Extensions._
import funcdiff.SimpleMath.{LabeledGraph, wrapInQuotes}
import gtype.ExportStmt.{ExportDefault, ExportOtherModule, ExportSingle}
import gtype.GModule.ProjectPath
import gtype.GStmt.{TypeAnnotation, TypeHoleContext}
import gtype.ImportStmt._
import gtype.{
  AnyType,
  ExportStmt,
  FuncType,
  GModule,
  GStmt,
  GTHole,
  GType,
  JSExamples,
  ObjectType,
  TyVar
}
import infer.IR._
import infer.IRTranslation.TranslationEnv
import infer.PredicateGraph._

import collection.mutable
import PredicateGraphConstruction._

/** Encodes the relationships between different type variables */
object PredicateGraph {

  /**
    * @param path the path of this module related to the project root
    * @param predicates all the predicates ([[TyVarPredicate]]) generated by this module
    * @param typeLabels all the user-annotated type annotations, resolved as [[IRType]]s
    */
  case class PredicateModule(
    path: ProjectPath,
    predicates: Vector[TyVarPredicate],
    newTypes: Map[IRType, TypeName],
    typeLabels: Map[IRTypeId, TypeLabel]
  ) {
    def display: String = {
      s"""=== Module: $path ===
         |* predicates:
         |${predicates.mkString("\n")}
       """.stripMargin
    }
  }

  /** The desired type annotations that the inference algorithm should
    * produce, used for supervision */
  sealed trait TypeLabel
  case object OutOfScope extends TypeLabel
  case class LibraryType(ty: GType) extends TypeLabel
  case class ProjectType(ty: IRType) extends TypeLabel

  sealed trait TyVarPredicate

  case class EqualityRel(v1: IRType, v2: IRType) extends TyVarPredicate
  case class FreezeType(v: IRType, ty: GType) extends TyVarPredicate
  case class HasName(v: IRType, name: Symbol) extends TyVarPredicate
  case class IsLibraryType(v: IRType, name: Symbol) extends TyVarPredicate
  case class SubtypeRel(sub: IRType, sup: IRType) extends TyVarPredicate
  case class AssignRel(lhs: IRType, rhs: IRType) extends TyVarPredicate
  case class UsedAsBoolean(tyVar: IRType) extends TyVarPredicate
  case class InheritanceRel(child: IRType, parent: IRType) extends TyVarPredicate
  case class DefineRel(v: IRType, expr: TypeExpr) extends TyVarPredicate

  sealed trait TypeExpr
  case class FuncTypeExpr(argTypes: List[IRType], returnType: IRType) extends TypeExpr
  case class CallTypeExpr(f: IRType, args: List[IRType]) extends TypeExpr
  case class ObjLiteralTypeExpr(fields: Map[Symbol, IRType]) extends TypeExpr
  case class FieldAccessTypeExpr(objType: IRType, field: Symbol) extends TypeExpr

  def predicateCategory(p: TyVarPredicate): Symbol = p match {
    case _: EqualityRel    => 'equality
    case _: FreezeType     => 'freeze
    case _: IsLibraryType  => 'isLibType
    case _: HasName        => 'hasName
    case _: SubtypeRel     => 'subtype
    case _: AssignRel      => 'assign
    case _: UsedAsBoolean  => 'usedAsBool
    case _: InheritanceRel => 'inheritance
    case DefineRel(_, et) =>
      et match {
        case _: FuncTypeExpr        => Symbol("define-func")
        case _: CallTypeExpr        => Symbol("define-call")
        case _: ObjLiteralTypeExpr  => Symbol("define-object")
        case _: FieldAccessTypeExpr => Symbol("define-access")
      }
  }

  def displayPredicateGraph(
    correctNodes: Seq[IRType],
    wrongNodes: Seq[IRType],
    predicates: Seq[TyVarPredicate],
    typeHoleMap: Map[IRTypeId, GTHole]
  ): LabeledGraph = {
    def typeInfo(t: IR.IRType): String = {
      val holeInfo = typeHoleMap.get(t.id).map(h => s";Hole: ${h.id}").getOrElse("")
      wrapInQuotes(t.toString.replace("\uD835\uDCAF", "") + holeInfo)
    }

    var nodeId = 0
    def newNode(): Int = {
      nodeId -= 1
      nodeId
    }

    val graph = new SimpleMath.LabeledGraph()

    def newPredicate(
      shortName: String,
      fullName: String,
      connections: Seq[(Int, String)]
    ): Unit = {
      val n = newNode()
      graph.addNode(n, shortName, wrapInQuotes(fullName), "Blue")
      connections.foreach {
        case (id, label) =>
          graph.addEdge(n, id, wrapInQuotes(label))
      }
    }

    correctNodes.foreach(n => {
      graph.addNode(n.id, n.id.toString, typeInfo(n), "Green")
    })
    wrongNodes.foreach(n => {
      graph.addNode(n.id, n.id.toString, typeInfo(n), "Red")
    })

    predicates.foreach {
      case EqualityRel(v1, v2) =>
        newPredicate("=", "Equality", Seq(v1.id -> "L", v2.id -> "R"))
      case FreezeType(v, ty) =>
        newPredicate(s"=$ty", s"Freeze to $ty", Seq(v.id -> ""))
      case IsLibraryType(v, name) =>
        newPredicate(s"[${name.name}]", s"Is library type: $name", Seq(v.id -> ""))
      case HasName(v, name) =>
        newPredicate(s"{${name.name}}", s"Has name $name", Seq(v.id -> ""))
      case SubtypeRel(sub, sup) =>
        newPredicate("<:", "Subtype", Seq(sub.id -> "sub", sup.id -> "sup"))
      case AssignRel(lhs, rhs) =>
        newPredicate(":=", "Assign", Seq(lhs.id -> "lhs", rhs.id -> "rhs"))
      case UsedAsBoolean(tyVar) =>
        newPredicate("~bool", "Use as bool", Seq(tyVar.id -> ""))
      case InheritanceRel(child, parent) =>
        newPredicate(
          "extends",
          "Extends",
          Seq(child.id -> "child", parent.id -> "parent")
        )
      case DefineRel(v, expr) =>
        val (short, long, connections) = expr match {
          case FuncTypeExpr(argTypes, returnType) =>
            val conn = argTypes.zipWithIndex.map { case (a, i) => a.id -> s"arg$i" } :+ (returnType.id -> "return")
            ("Func", "FuncTypeExpr", conn)
          case CallTypeExpr(f, args) =>
            val conn = args.zipWithIndex.map { case (a, i) => a.id -> s"arg$i" } :+ (f.id -> "f")
            ("Call", "CallTypeExpr", conn)
          case ObjLiteralTypeExpr(fields) =>
            val conn = fields.toList.map { case (label, t) => t.id -> label.toString() }
            ("Obj", "ObjLiteralTypeExpr", conn)
          case FieldAccessTypeExpr(objType, field) =>
            (s"_.${field.name}", "FieldAccessTypeExpr", Seq(objType.id -> ""))
        }
        newPredicate(short, long, connections :+ (v.id -> "="))
    }

    graph
  }

  val returnVar: Var = namedVar(gtype.GStmt.returnSymbol)
}

import ammonite.ops._
import gtype.parsing.ProgramParsing

object PredicateGraphConstruction {

  trait PathMapping {
    def map(currentPath: ProjectPath, pathToResolve: ProjectPath): ProjectPath
  }

  object PathMapping {
    def identity: PathMapping =
      (currentPath: ProjectPath, pathToResolve: ProjectPath) => {
        currentPath / pathToResolve
      }
  }

  object LibraryContext {
    def libIdForVar(s: Symbol): Symbol = {
      Symbol("LIB$" + s.name)
    }
  }

  class LibraryContext(
    val transEnv: TranslationEnv,
    val libraryVars: mutable.HashMap[VarName, IRType] = mutable.HashMap(),
    val libraryTypeFreq: mutable.HashMap[GType, Int] = mutable.HashMap(),
    val libraryTypes: mutable.HashMap[GType, IRType] = mutable.HashMap()
  ) {
    def newLibVar(name: VarName, ty: Option[GType])(
      implicit tyVars: Set[Symbol]
    ): IRType = {
      val n = LibraryContext.libIdForVar(name)
      val tv = transEnv.newTyVar(
        None,
        Some(name),
        ty.map(t => TypeAnnotation(t, needInfer = false)),
        Some(n)
      )
      libraryVars(n) = tv
      tv
    }

    def getVarType(
      v: Var
    )(implicit predCtx: PredicateContext, tyVars: Set[Symbol]): IRType = {
      predCtx.varTypeMap.getOrElse(
        v, {
          val name = v.nameOpt.get
          libraryVars.getOrElse(name, newLibVar(name, None))
        }
      )
    }

    def newLibType(ty0: GType)(implicit tyVars: Set[Symbol]): IRType = {
      val ty = IRTranslation.translateType(ty0)
      val nameOpt = ty match {
        case TyVar(n) => Some(n)
        case _        => None
      }
      transEnv.newTyVar(
        None,
        nameOpt,
        None,
        libId = Some(Symbol(ty.toString))
      ) //todo: improve to use type structure
    }

    def getLibType(ty: GType)(implicit tyVars: Set[Symbol]): IRType = {
      val t = libraryTypes.getOrElseUpdate(ty, newLibType(ty))
      registerLibType(IRTranslation.translateType(ty))
      t
    }

    def registerLibType(ty: GType): Unit = {
      libraryTypeFreq.getOrElseUpdate(ty, 0)
      libraryTypeFreq(ty) += 1
      ty match {
        case FuncType(from, to) =>
          (from :+ to).foreach(registerLibType)
        case ObjectType(fields) =>
          fields.foreach { p =>
            registerLibType(p._2)
          }
        case _ =>
      }
    }
  }

  case class PredicateContext(
    varTypeMap: Map[Var, IRType],
    newTypeMap: Map[TypeName, IRType],
    packageNames: Set[Symbol]
  )

  def qualifiedName(`package`: Symbol, name: Symbol): Symbol = {
    Symbol(`package`.name + "." + name.name)
  }

  object PredicateContext {

    def jsCtx(libCtx: LibraryContext): PredicateContext = {
      implicit val tyVars: Set[Symbol] = Set()

      val typeMap = JSExamples.exprContext.varAssign.map {
        case (s, t) =>
          val irType = libCtx.newLibVar(s, Some(t))
          namedVar(s) -> irType
      }

      PredicateContext(typeMap, Map(), Set())
    }
  }

  case class ParsedProject(
    projectName: String,
    libCtx: LibraryContext,
    irModules: Vector[IRModule],
    predModules: Vector[PredicateModule],
    predCtx: PredicateContext
  )

  def fromModules(
    projectName: String,
    modules: Seq[GModule],
    libraryTypes: Set[GType],
    pathMapping: PathMapping
  ): ParsedProject = {
    val env = new TranslationEnv()
    val libCtx = new LibraryContext(env)
    val irModules = modules.map(m => IRTranslation.translateModule(m)(env)).toVector

    val ctx = PredicateContext.jsCtx(libCtx)

    val pModules =
      new PredicateGraphConstruction(libCtx).encodeModules(irModules, ctx, pathMapping)

    ParsedProject(projectName, libCtx, irModules, pModules, ctx)
  }

  def fromSourceFiles(
    root: Path,
    libraryFiles: Set[ProjectPath] = Set(),
    libraryTypes: Set[GType] = JSExamples.libraryTypes.map(TyVar),
    excludeIndexFile: Boolean = false,
    pathMapping: PathMapping = PathMapping.identity
  ): ParsedProject = {
    val indexFileNames = Set("index.ts", "public_api.ts")

    val sources = ls
      .rec(root)
      .filter { f =>
        if (f.last.endsWith(".d.ts")) {
          throw new Error(s".d.ts file encountered: $f")
        }
        f.ext == "ts"
      }
      .filterNot(f => excludeIndexFile && indexFileNames.contains(f.last))
      .map(_.relativeTo(root))
    val parser = new ProgramParsing()
    val modules = parser.parseModulesFromFiles(
      sources,
      libraryFiles,
      root
    )

    fromModules(root.toString(), modules, libraryTypes, pathMapping)
  }

  def resolveImports(
    module: IRModule,
    baseCtx: PredicateContext,
    allModules: Map[ProjectPath, IRModule],
    pathMapping: PathMapping
  ): PredicateContext = {

    var varTypeMap = baseCtx.varTypeMap
    var newTypeMap = baseCtx.newTypeMap
    var packageNames = Set[Symbol]()
    val currentDir = module.path / ammonite.ops.up

    def getExports(path: ProjectPath): ModuleExports = {
      allModules
        .getOrElse(
          pathMapping.map(currentDir, path),
          throw new Error(s"Cannot find source file: '${currentDir / path}'.")
        )
        .exports
    }

    module.imports.foreach {
      case ImportSingle(oldName, path, newName) =>
        val exports = getExports(path)
        var resolved = false
        for ((t, exported) <- exports.terms.get(oldName) if exported) {
          resolved = true
          varTypeMap = varTypeMap.updated(Var(Right(newName)), t)
        }
        for ((t, exported) <- exports.typeAliases.get(oldName) if exported) {
          resolved = true
          newTypeMap = newTypeMap.updated(newName, t)
        }
        for ((t, exported) <- exports.classes.get(oldName) if exported) {
          resolved = true
          newTypeMap = newTypeMap.updated(newName, t)
        }

        if (!resolved) {
          throw new Error(s"Unresolved import single: $oldName from '$path'.")
        }
      case ImportModule(path, newName) =>
        val exports = getExports(path)
        varTypeMap = varTypeMap ++ exports.terms.collect {
          case (n, (t, true)) =>
            Var(Right(qualifiedName(newName, n))) -> t
        }
        newTypeMap ++= exports.typeAliases.collect {
          case (n, (t, true)) => qualifiedName(newName, n) -> t
        }
        newTypeMap ++= exports.classes.collect {
          case (n, (t, true)) => qualifiedName(newName, n) -> t
        }
        packageNames += newName
      case im @ ImportDefault(path, newName) =>
        var resolved = false
        getExports(path).defaultType.foreach { p =>
          resolved = true
          newTypeMap += newName -> p._2
        }
        getExports(path).defaultVar.foreach {
          case (_, t) =>
            resolved = true
            varTypeMap += (Var(Right(newName)) -> t)
        }
        if (!resolved)
          throw new Error(im + " not resolved!")
    }

    PredicateContext(varTypeMap, newTypeMap, packageNames)
  }

  def encodeUnaryPredicates(
    vars: Iterable[IRType]
  ): Vector[TyVarPredicate] = {
    import collection.mutable
    var newPredicates = mutable.ListBuffer[TyVarPredicate]()
    vars.foreach { tv =>
      tv.annotation.foreach(a => if (!a.needInfer) newPredicates += FreezeType(tv, a.ty))
      tv.name.foreach(n => newPredicates += HasName(tv, n))
      tv.libId.foreach(libId => newPredicates += IsLibraryType(tv, libId))
    }
    newPredicates.toVector
  }
}

class PredicateGraphConstruction(val libraryContext: LibraryContext) {

  def encodeModules(
    modules: Seq[IRModule],
    baseCtx: PredicateContext,
    pathMapping: PathMapping,
    exportIterations: Int = 20
  ): Vector[PredicateModule] = {

    /** Try to turn export statements into export definitions, may need multiple
      * iteration of this function to achieve fixed-point */
    def propagateExports(
      allModules: Map[ProjectPath, IRModule]
    ): Map[ProjectPath, IRModule] = {
      def getExports(currentDir: ProjectPath, path: ProjectPath): ModuleExports = {
        allModules
          .getOrElse(
            pathMapping.map(currentDir, path),
            throw new Error(s"Cannot find source file: '${currentDir / path}'.")
          )
          .exports
      }

      allModules.map {
        case (modulePath, md) =>
          val dir = modulePath / ammonite.ops.up
          var newDefs = md.exports.definitions
          var newDefaultVar = md.exports.defaultVar
          var newDefaultType = md.exports.defaultType

          SimpleMath.withErrorMessage(s"when parsing $modulePath") {
            md.imports.foreach {
              case ImportSingle(oldName, path, newName) =>
                val exports = getExports(modulePath / up, path)

                for ((t, exported) <- exports.terms.get(oldName) if exported) {
                  newDefs = newDefs.updated((newName, ExportCategory.Term), (t, false))
                }
                for ((t, exported) <- exports.typeAliases.get(oldName) if exported) {
                  newDefs =
                    newDefs.updated((newName, ExportCategory.TypeAlias), (t, false))
                }
                for ((t, exported) <- exports.classes.get(oldName) if exported) {
                  newDefs = newDefs.updated((newName, ExportCategory.Class), (t, false))
                }
              case _ =>
            }

            md.exportStmts.foreach {
              case ExportSingle(oldName, newName, source) =>
                val ex = source match {
                  case Some(from) =>
                    allModules(pathMapping.map(dir, from)).exports
                  case None => md.exports
                }

                def export(
                  map: Map[Symbol, (IRType, Exported)],
                  category: ExportCategory.Value
                ): Unit = {
                  map.get(oldName).foreach {
                    case (tv, exported) =>
                      if (source.isEmpty) {
                        newDefs = newDefs.updated((newName, category), (tv, true))
                      } else if (exported) {
                        newDefs = newDefs.updated((newName, category), (tv, exported))
                      }
                  }
                }

                export(ex.terms, ExportCategory.Term)
                export(ex.typeAliases, ExportCategory.TypeAlias)
                export(ex.classes, ExportCategory.Class)
              case ExportOtherModule(from) =>
                val ex = allModules(pathMapping.map(dir, from)).exports
                newDefs = newDefs ++ ex.definitions
              case ExportDefault(newName, from) =>
                from match {
                  case Some(path) =>
                    val ex = allModules(pathMapping.map(dir, path)).exports
                    ex.defaultType.foreach {
                      case (name, t) =>
                        newDefaultType = Some(newName.getOrElse(name) -> t)
                    }
                    ex.defaultVar.foreach {
                      case (v, t) =>
                        val newVar = newName.map(n => Var(Right(n))).getOrElse(v)
                        newDefaultVar = Some(newVar -> t)
                    }
                  case None =>
                    val name = newName.get
                    md.exports.terms.get(name).foreach {
                      case (tv, _) =>
                        newDefaultVar = Some(Var(Right(name)) -> tv)
                    }
                    md.exports.classes.get(name).foreach {
                      case (tv, _) =>
                        newDefaultType = Some(name -> tv)
                    }
                    md.exports.typeAliases.get(name).foreach {
                      case (tv, _) =>
                        newDefaultType = Some(name -> tv)
                    }
                }
            }
          }
          modulePath -> md.copy(
            exports = md.exports.copy(
              definitions = newDefs,
              defaultType = newDefaultType,
              defaultVar = newDefaultVar
            )
          )
      }
    }

    val irModules = {
      val init = modules.map(m => m.path -> m).toMap
      Vector.iterate(init, exportIterations)(propagateExports).last
    }

    irModules.toVector.zipWithIndex.map {
      case ((path, module), idx) =>
        SimpleMath.withErrorMessage(
          s"[$idx parsed] Predicate Graph Construction failed for module: '$path'"
        ) {
          val (predicates, ctx1, labels) =
            encodeStmts(
              module.stmts,
              resolveImports(module, baseCtx, irModules, pathMapping)
            )
          val newTypes = ctx1.newTypeMap.map(_.swap)
          PredicateModule(module.path, predicates, newTypes, labels)
        }
    }
  }

  def encodeStmts(
    stmts: Vector[IRStmt],
    ctx: PredicateContext
  ): (Vector[TyVarPredicate], PredicateContext, Map[IRTypeId, TypeLabel]) = {
    import collection.mutable

    val typeLabels = mutable.HashMap[IRTypeId, TypeLabel]()
    val relations = mutable.ListBuffer[TyVarPredicate]()

    def add(rel: TyVarPredicate): Unit = {
      relations += rel
    }

    /** Collect the variable, function, and class definitions declared in a
      * block and return a new context */
    def collectDefinitions(
      stmts: Vector[IRStmt]
    )(implicit ctx: PredicateContext): PredicateContext = {
      val typeDefs = stmts.collect {
        case c: ClassDef        => c.name -> c.classT
        case a: TypeAliasIRStmt => a.name -> a.aliasT
      }

      val defs = stmts.collect {
        case d: VarDef  => d.v -> d.mark
        case f: FuncDef => namedVar(f.name) -> f.funcT
      }

      ctx.copy(
        varTypeMap = ctx.varTypeMap ++ defs,
        newTypeMap = ctx.newTypeMap ++ typeDefs
      )
    }
    def getTypeFromName(ctx: PredicateContext, name: TypeName): IRType = {
      ctx.newTypeMap.getOrElse(name, libraryContext.getLibType(TyVar(name))(Set()))
    }

    def encodeStmt(stmt: IRStmt)(implicit ctx: PredicateContext): Unit = {

      import ctx._
      def resolveLabel(ty: GType): TypeLabel = {
//        def outOfScope(): OutOfScope.type = {
//          System.err.println(s"[warn] out of scope type label: $ty")
//          OutOfScope
//        }

        ty match {
          case AnyType => LibraryType(AnyType)
          case TyVar(v) if ctx.newTypeMap.contains(v) =>
            ProjectType(ctx.newTypeMap(v))
          case _ =>
            libraryContext.registerLibType(ty)
            LibraryType(ty)
        }
      }

      def recordLabel(tv: IRType): Unit = {
        tv.annotation.foreach(
          a =>
            if (a.needInfer) {
              typeLabels(tv.id) = resolveLabel(a.ty)
            }
        )
      }

      SimpleMath.withErrorMessage(s"--->\n${stmt.prettyPrint()}") {
        stmt match {
          case d: VarDef =>
            // don't need to modify ctx here, collect definitions when processing blocks
            val tv = varTypeMap(d.v)
            assert(tv == d.mark, s"mark = ${d.mark}, tv = $tv")
            recordLabel(tv)
            d.rhs match {
              case v1: Var =>
                add(EqualityRel(tv, varTypeMap(v1)))
              case Const(_, ty) =>
                add(FreezeType(tv, ty))
              case FuncCall(f, args) =>
                add(DefineRel(tv, CallTypeExpr(varTypeMap(f), args.map(varTypeMap))))
              case ObjLiteral(fields) =>
                add(DefineRel(tv, ObjLiteralTypeExpr(fields.mapValuesNow(varTypeMap))))
              case FieldAccess(receiver, label) =>
                if (receiver.nameOpt.exists(n => packageNames.contains(n))) {
                  add(
                    EqualityRel(
                      tv,
                      varTypeMap(Var(Right(qualifiedName(receiver.nameOpt.get, label))))
                    )
                  )
                } else {
                  add(DefineRel(tv, FieldAccessTypeExpr(varTypeMap(receiver), label)))
                }
              case IfExpr(cond, e1, e2) =>
                add(SubtypeRel(varTypeMap(e1), tv))
                add(SubtypeRel(varTypeMap(e2), tv))
                add(UsedAsBoolean(varTypeMap(cond)))
            }
          case Assign(lhs, rhs) =>
            add(AssignRel(varTypeMap(lhs), varTypeMap(rhs)))
          case ReturnStmt(v) =>
            add(SubtypeRel(varTypeMap(v), ctx.varTypeMap(returnVar)))
          case IfStmt(cond, e1, e2) =>
            add(UsedAsBoolean(varTypeMap(cond)))
            encodeStmt(e1)
            encodeStmt(e2)
          case WhileStmt(cond, body) =>
            add(UsedAsBoolean(varTypeMap(cond)))
            encodeStmt(body)
          case block: BlockStmt =>
            val innerCtx = collectDefinitions(block.stmts)
            //          println(s"Inner context: $innerCtx")
            block.stmts.foreach(s => encodeStmt(s)(innerCtx))
          case FuncDef(_, args, newReturnType, body, funcT, _) =>
            args.foreach(p => recordLabel(p._2))
            recordLabel(newReturnType)

            val ctx1 = ctx.copy(
              varTypeMap = (ctx.varTypeMap ++ args) + (returnVar -> newReturnType)
            )
            add(DefineRel(funcT, FuncTypeExpr(args.map(_._2), newReturnType)))
            encodeStmt(body)(ctx1)
          case ClassDef(_, superType, vars, funcDefs, classT, _, _) =>
            vars.values.foreach(recordLabel)

            val superMap = superType.map { n =>
              val parentType = getTypeFromName(ctx, n)
              add(InheritanceRel(classT, parentType))
              ClassDef.superVar -> parentType
            }
            val methods = funcDefs.map(f => f.name -> f.funcT)
            val objExpr = ObjLiteralTypeExpr(vars ++ methods)

            add(DefineRel(classT, objExpr))

            val innerCtx =
              ctx.copy(
                varTypeMap = ctx.varTypeMap + (ClassDef.thisVar -> classT) ++ superMap.toList
              )
            funcDefs.foreach(s => encodeStmt(s)(innerCtx))
          case _: TypeAliasIRStmt => //do nothing
        }
      }
    }

    encodeStmt(BlockStmt(stmts))(ctx)
    (relations.toVector, collectDefinitions(stmts)(ctx), typeLabels.toMap)
  }
}
