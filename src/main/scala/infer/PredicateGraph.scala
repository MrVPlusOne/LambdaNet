package infer

import funcdiff.SimpleMath
import funcdiff.SimpleMath.Extensions._
import funcdiff.SimpleMath.{LabeledGraph, wrapInQuotes}
import gtype.GModule.ProjectPath
import gtype.ImportStmt._
import gtype.{GTHole, GType, JSExamples}
import infer.IR._
import infer.IRTranslation.TranslationEnv
import infer.PredicateGraph._

/** Encodes the relationships between different type variables */
object PredicateGraph {

  case class PredicateModule(
    path: ProjectPath,
    predicates: Vector[TyVarPredicate],
    newTypes: Map[ClassName, IRType]
  ) {
    def display: String = {
      s"""=== Module: $path ===
         |* newTypes:
         |${newTypes.mkString("\n")}
         |* predicates:
         |${predicates.mkString("\n")}
       """.stripMargin
    }
  }

  sealed trait TyVarPredicate

  case class EqualityRel(v1: IRType, v2: IRType) extends TyVarPredicate
  case class FreezeType(v: IRType, ty: GType) extends TyVarPredicate
  case class HasName(v: IRType, name: Symbol) extends TyVarPredicate
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

object PredicateGraphConstruction {
  import ammonite.ops._
  import gtype.parsing.ProgramParsing

  case class PredicateContext(
    varTypeMap: Map[Var, IRType],
    newTypeMap: Map[ClassName, IRType]
  )

  object PredicateContext {
    val empty: PredicateContext =
      PredicateContext(Map(), Map())

    def jsCtx(env: TranslationEnv): PredicateContext = {
      implicit val tyVars: Set[Symbol] = Set()
      val typeMap = JSExamples.exprContext.varAssign.map {
        case (s, t) => namedVar(s) -> env.newTyVar(None, Some(s), Some(t))
      }
      //      val objDef = JSExamples.typeContext.typeUnfold.keys
      //        .map(s => s -> env.newTyVar(None, Some(s), Some(TyVar(s))))
      //        .toMap
      PredicateContext(typeMap, Map())
    }
  }

  def fromSourceFiles(
    root: Path,
    libraryFiles: Set[RelPath] = Set(),
    excludeIndexFile: Boolean = true
  ): Vector[PredicateModule] = {
    val sources = ls
      .rec(root)
      .filter(f => f.ext == "ts")
      .filterNot(f => excludeIndexFile && f.last == "index.ts")
      .map(_.relativeTo(root))
    val modules = ProgramParsing.parseModulesFromFiles(
      sources,
      libraryFiles,
      root
    )

    val env = new TranslationEnv()
    val irModules = modules.map(m => IRTranslation.translateModule(m)(env))

    val ctx = PredicateContext.jsCtx(env)
    val pModules = PredicateGraphConstruction.encodeModules(irModules, ctx)

    pModules
  }

  def encodeModules(
    modules: Seq[IRModule],
    baseCtx: PredicateContext
  ): Vector[PredicateModule] = {
    val irModules = modules.map(m => m.path -> m).toMap

    //todo: properly handle export statements, currently ignored
    def resolveImports(module: IRModule): PredicateContext = {
      var varTypeMap = baseCtx.varTypeMap
      var newTypeMap = baseCtx.newTypeMap
      val currentDir = module.path / ammonite.ops.up

      def getExports(path: ProjectPath): ModuleExports = {
        irModules
          .getOrElse(
            currentDir / path,
            throw new Error(s"Cannot find source file: '${currentDir / path}'.")
          )
          .exports
      }

      import gtype.ClassDef.{constructorName, isConstructor}

      module.imports.foreach {
        case ImportSingle(oldName, path, newName) =>
          val exports = getExports(path)
          val v = Var(Right(oldName))
          exports.terms.get(v) match {
            case Some(t) => varTypeMap = varTypeMap.updated(Var(Right(newName)), t)
            case None =>
              newTypeMap = newTypeMap.updated(newName, exports.types(oldName))
              val constructorT = exports.terms(Var(Right(constructorName(oldName))))
              varTypeMap += (Var(Right(constructorName(newName))) -> constructorT)
          }
        case ImportModule(path, newName) =>
          val exports = getExports(path)
          varTypeMap = varTypeMap ++ exports.terms.map {
            case (n, t) =>
              Var(Right(Symbol(newName.name + "." + n.nameOpt.get.name))) -> t
          }
          newTypeMap ++= exports.types.map {
            case (n, t) => Symbol(newName.name + "." + n.name) -> t
          }
        case ImportDefault(path, newName) =>
          getExports(path).defaultType.foreach { p =>
            newTypeMap += p
          }
          getExports(path).defaultVar.foreach {
            case (v, t) =>
              val name1 =
                if (isConstructor(v.nameOpt.get)) constructorName(newName) else newName
              varTypeMap += (Var(Right(name1)) -> t)
          }
      }

      PredicateContext(varTypeMap, newTypeMap)
    }

    modules.toVector.map { module =>
      val (predicates, ctx1) = encodeIR(module.stmts, resolveImports(module))
      PredicateModule(module.path, predicates, ctx1.newTypeMap)
    }
  }

  def encodeIR(
    stmts: Vector[IRStmt],
    ctx: PredicateContext
  ): (Vector[TyVarPredicate], PredicateContext) = {
    import collection.mutable

    val relations = mutable.ListBuffer[TyVarPredicate]()

    def add(rel: TyVarPredicate): Unit = {
      relations += rel
    }

    /** Collect the variable, function, and class definitions declared in a
      * block and return a new context */
    def collectDefinitions(
      stmts: Vector[IRStmt]
    )(implicit ctx: PredicateContext): PredicateContext = {
      val classDefs = stmts.collect {
        case c: ClassDef => c.name -> c.classT
      }

      val defs = stmts.collect {
        case d: VarDef   => d.v -> d.mark
        case f: FuncDef  => namedVar(f.name) -> f.funcT
        case c: ClassDef => namedVar(c.constructor.name) -> c.constructor.funcT
      }

      ctx.copy(
        varTypeMap = ctx.varTypeMap ++ defs,
        newTypeMap = ctx.newTypeMap ++ classDefs
      )
    }

    def encodeStmt(stmt: IRStmt)(implicit ctx: PredicateContext): Unit = {
      import ctx._

      SimpleMath.addMessagesForExceptions(s"--->\n${stmt.prettyPrint()}") {
        stmt match {
          case d: VarDef =>
            // don't need to modify ctx here, collect definitions when processing blocks
            val tv = varTypeMap(d.v)
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
                add(DefineRel(tv, FieldAccessTypeExpr(varTypeMap(receiver), label)))
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
            val innerCtx =
              collectDefinitions(body)(
                ctx.copy(
                  varTypeMap = (ctx.varTypeMap ++ args) + (returnVar -> newReturnType)
                )
              )
            add(DefineRel(funcT, FuncTypeExpr(args.map(_._2), newReturnType)))
            body.foreach(s => encodeStmt(s)(innerCtx))
          case ClassDef(_, superType, constructor, vars, funcDefs, classT, _) =>
            superType.foreach { n =>
              val parentType = ctx.newTypeMap(n)
              add(InheritanceRel(classT, parentType))
            }
            val methods = funcDefs.map(f => f.name -> f.funcT)
            val objExpr = ObjLiteralTypeExpr(vars ++ methods)

            add(DefineRel(classT, objExpr))

            val innerCtx =
              ctx.copy(
                varTypeMap = ctx.varTypeMap + (ClassDef.thisVar -> classT)
              )
            (constructor +: funcDefs).foreach(s => encodeStmt(s)(innerCtx))
        }
      }
    }

    encodeStmt(BlockStmt(stmts))(ctx)
    relations.toVector -> collectDefinitions(stmts)(ctx)
  }

  def encodeUnaryPredicates(vars: Iterable[IRType]): Vector[TyVarPredicate] = {
    import collection.mutable
    var newPredicates = mutable.ListBuffer[TyVarPredicate]()
    vars.foreach { tv =>
      tv.freezeToType.foreach(t => newPredicates += FreezeType(tv, t))
      tv.name.foreach(n => newPredicates += HasName(tv, n))
    }
    newPredicates.toVector
  }
}
