package lambdanet.translation

import ammonite.ops.{Path, up}
import funcdiff.SimpleMath
import lambdanet.PrepareRepos.{ParsedProject, libDefsFile, parseProject}
import lambdanet.Surface.GStmt
import lambdanet.translation.ImportsResolution.{ErrorHandler, NameDef}
import lambdanet.translation.PredicateGraph.PSyntheticCall.signatureOpt
import lambdanet.translation.PredicateGraph._
import lambdanet.{IdAllocator, _}

import scala.collection.{breakOut, mutable}
import scala.reflect.ClassTag

/**
  * A predicate graph (aka. type dependency graph) consists of type variables
  * ([[PNode]]s) and hyperedges ([[TyPredicate]]s), together they encodes
  * type-related aspects of the original program.
  *
  * @param userAnnotations The user type annotations that are already encoded as hyperedges.
  * @param typeMap a map that maps structured types into [[PNode]].
  */
@SerialVersionUID(1)
case class PredicateGraph(
    nodes: Set[PNode],
    predicates: Set[TyPredicate],
    userAnnotations: Map[ProjNode, PType],
    typeMap: Map[PType, PNode],
) extends Serializable {
  def showSizes: String =
    s"{nodes = ${nodes.size}, predicates = ${predicates.size}}"

  lazy val projNodes: Set[PNode] = nodes.filter(_.fromProject)

  /**
    * Adds the given user type annotations to the graph. This will also create
    * new predicates.
    */
  def addUserAnnotations(newAnnots: Map[ProjNode, PType]): PredicateGraph = {
    val maxId = nodes.map(_.getId).max
    val allocator = new PNodeAllocator(forLib = false, state = maxId + 1)
    val trans = new PredicateGraphTranslation.Translator(allocator)
    for((k, v) <- typeMap) {
      trans.typeMap(k) = v
    }
    import trans.{addRel, encodePType}

    newAnnots.foreach {
      case (n, ty) =>
        val rel =
          BinaryRel(lhs = n.n, rhs = encodePType(ty), category = BinaryRelCat.userAnnotation)
        addRel(rel)
    }

    PredicateGraph(
      nodes = nodes ++ trans.predicates.flatMap(_.allNodes),
      predicates = predicates ++ trans.predicates,
      userAnnotations = userAnnotations ++ newAnnots,
      typeMap = trans.typeMap.toMap,
    )
  }

  def mergeEqualities: (PredicateGraph, Map[PNode, PNode]) = {
    val substitution = predicates
      .collect {
        case DefineRel(v, v1: PNode) =>
          if (v1.nameOpt.isEmpty) v1.nameOpt = v.nameOpt
          if (v1.srcSpan == null || v1.srcSpan.isEmpty) v1.srcSpan = v.srcSpan
          (v, v1)
      }
      .foldLeft(Map[PNode, PNode]()) { (map, pair) =>
        val (v0, v1) = pair
        map.mapValuesNow { v =>
          if (v == v0) v1 else v
        } + pair
      }
    val equalities = predicates.collect {
      case d @ DefineRel(_, _: PNode) => d
    }
    def substF(n: PNode) = substitution.getOrElse(n, n)

    val nameList = Set(thisSymbol, superSymbol, 'CONSTRUCTOR)
    val predicates1 = (predicates -- equalities)
      .map { _.substitute(substF) }
      .filter {
        case HasName(_, name) if nameList contains name =>
          false
        case _ => true
      }

    val annotations1 = userAnnotations.map { case (k, v) => (ProjNode(substF(k.n)), v) }

    val typeMap1 = typeMap.map { case (k, v) => (k.substitute(substF), substF(v)) }

    val g = PredicateGraph(
      nodes.map { substF },
      predicates1,
      annotations1,
      typeMap1,
    ).tap { p1 =>
      val diff = p1.predicates.flatMap(_.allNodes).diff(p1.nodes)
      assert(diff.isEmpty, s"predicates contain un-captured nodes: $diff")
      println(
        s"before merging equalities: ${this.showSizes}; after: ${p1.showSizes}"
      )
    }
    (g, substitution)
  }
}

object PredicateGraph {

  object PNode {
    def apply(
        id: Int,
        nameOpt: Option[Symbol],
        isType: Boolean,
        fromLib: Boolean
    ): PNode = { new PNode(id, nameOpt, isType, fromLib, None) }

    def unapply(n: PNode): Option[(Int, Option[Symbol], Boolean, Boolean)] = {
      Some((n.id, n.nameOpt, n.isType, n.fromLib))
    }
  }

  type NodeSubstitution = Map[PNode, PNode]
  type NodeMapping = Map[PNode, PAnnot]
  def substituteMapping(
      mapping: NodeMapping,
      subst: NodeSubstitution
  ): NodeMapping = {
    val mergerUserKeys = subst.keySet.collect {
      case k if k.fromProject => k
    }
    (mapping.keySet -- mergerUserKeys).map { k =>
      k -> mapping(k).map(_.substitute(n => subst.getOrElse(n, n)))
    }.toMap
  }

  @SerialVersionUID(1)
  class PNode(
      protected val id: Int,
      var nameOpt: Option[Symbol],
      val isType: Boolean,
      val fromLib: Boolean,
      var srcSpan: Option[SrcSpan],
  ) extends PExpr
      with Serializable {
    def isTerm: Boolean = !isType

    def fromProject: Boolean = !fromLib

    override def toString: String = {
      val namePart = nameOpt.map(n => s"{${n.name}}").getOrElse("")
      val tyPart = if (isType) "[ty]" else ""
      val prefix = if (fromLib) "𝓛" else "𝓟"
      s"$prefix$tyPart$id$namePart"
    }

    override def equals(obj: Any): Boolean = obj match {
      case p: PNode =>
        (id, fromLib) == (p.id, p.fromLib)
      case _ => false
    }

    override def hashCode(): Int = {
      (id, fromLib).hashCode()
    }

    def allNodes: Set[PNode] = Set(this)

    val symbol: Symbol = Symbol(id.toString)

    def allLabels: Set[Symbol] = Set()

    def name: String = nameOpt.get.name

    def nameOr(default: String): String = nameOpt.map(_.name).getOrElse(default)

    def getId: Int = id
  }

  @SerialVersionUID(1)
  class PNodeAllocator(val forLib: Boolean, state: Int = 0)
      extends IdAllocator[PNode]
      with Serializable {
    val anyNode: PNode = newNode(Some('ANY), isType = true, None)
    val unknownDef: NameDef =
      if (forLib) {
        NameDef.makeUnknownDef(this)
      } else null
    setState(state)

    def newNode(
        nameOpt: Option[Symbol],
        isType: Boolean,
        srcSpan: Option[SrcSpan],
    ): PNode = {
      if (!forLib) {
        nameOpt.foreach { name =>
          if (isType && QLang.basicTypes.contains(name)) {
            throw new Error(s"Trying to override basic type definitions: $name")
          }
        }
      }
      useNewId(id => new PNode(id, nameOpt, isType, forLib, srcSpan))
    }

    val namedUnknownDefs: mutable.HashMap[Symbol, NameDef] = mutable.HashMap()

    def newUnknownDef(nameOpt: Option[Symbol]): NameDef = {
      // todo: might want to map the srcSpan of import statements
      val srcSpan: Option[SrcSpan] = None
      nameOpt match {
        case None =>
          NameDef(
            Some(newNode(None, isType = false, srcSpan)),
            NameDef.unknownDef.ty,
            None
          )
        case Some(name) =>
          namedUnknownDefs.getOrElseUpdate(
            name,
            NameDef(
              Some(newNode(nameOpt, isType = false, srcSpan)),
              NameDef.unknownDef.ty,
              None
            )
          )
      }
    }
  }

  case class ProjNode(n: PNode) {
    assert(n.fromProject, s"$n should be project node")
  }
  case class LibNode(n: PNode) {
    assert(n.fromLib, s"$n should be library node")
  }
  case class LibTermNode(n: LibNode) {
    assert(n.n.isTerm, s"$n should be LibTermNode")
  }
  case class LibTypeNode(n: LibNode) {
    assert(n.n.isType, s"$n should be LibTypeNode")
  }

  @SerialVersionUID(0)
  sealed trait PType extends Serializable {
    val madeFromLibTypes: Boolean

    def allLabels: Set[Symbol]

    def allNodes: Set[PNode]

    def pPrint[S](
        showNode: PNode => S
    )(implicit convert: String => S, aggregate: Vector[S] => S): S = {
      def rec(envPriority: Int)(ty: PType): S = {
        def wrap(priority: Int)(content: S): S = {
          if (priority < envPriority) aggregate(Vector("(", content, ")"))
          else content
        }

        ty match {
          case PAny      => "any"
          case PTyVar(n) => showNode(n)
          case PFuncType(from, to) =>
            wrap(0)(
              aggregate(
                SM.joinWithSep[S](from.map(rec(1)), "(", ",", ")") :+
                  convert("->") :+ rec(0)(to)
              )
            )
          case PObjectType(fields) =>
            fields
              .map {
                case (l, t) =>
                  aggregate(Vector[S](l.name, ": ", rec(0)(t)))
              }
              .toVector
              .pipe(SM.joinWithSep[S](_, "{", ", ", "}"))
        }
      }

      rec(0)(this)
    }

    def substitute(f: PNode => PNode): PType = this match {
      case PAny         => PAny
      case PTyVar(node) => PTyVar(f(node))
      case PFuncType(args, to) =>
        PFuncType(args.map(_.substitute(f)), to.substitute(f))
      case PObjectType(fields) =>
        PObjectType(fields.mapValuesNow(_.substitute(f)))
    }

    override def toString: String = pPrint(_.toString)(identity, _.mkString(""))

    def showSimple: String = {
      def showNode(n: PNode): String = {
        val prefix = if (n.fromLib) "L" else "P"
        n.nameOpt match {
          case Some(name) =>
            val idPart = if (n.fromProject) s":${n.getId}" else ""
            s"${name.name}$idPart"
          case None =>
            s"$prefix${n.getId}"
        }
      }
      pPrint(showNode)(identity, _.mkString(""))
    }
  }

  @SerialVersionUID(0L)
  case object PAny extends PType {
    val madeFromLibTypes = true

    val allLabels: Set[Symbol] = Set()

    val allNodes: Set[PNode] = Set()
  }

  @SerialVersionUID(0L)
  case class PTyVar(node: PNode) extends PType {
    assert(node.isType, s"$node is not type")

    val madeFromLibTypes: Boolean = node.fromLib

    def allLabels: Set[Symbol] = Set()

    def allNodes: Set[PNode] = Set(node)
  }

  @SerialVersionUID(0L)
  case class PFuncType(args: Vector[PType], to: PType) extends PType {
    val madeFromLibTypes: Boolean =
      args.forall(_.madeFromLibTypes) && to.madeFromLibTypes

    def allLabels: Set[Symbol] =
      to.allLabels ++ args.toSet.flatMap((_: PType).allLabels)

    def allNodes: Set[PNode] = to.allNodes ++ args.flatMap((_: PType).allNodes)
  }

  @SerialVersionUID(0L)
  case class PObjectType(fields: Map[Symbol, PType]) extends PType {
    val madeFromLibTypes: Boolean = fields.forall(_._2.madeFromLibTypes)

    def allLabels: Set[Symbol] = {
      fields.keySet ++ fields.values.toSet.flatMap((x: PType) => x.allLabels)
    }

    def allNodes: Set[PNode] = fields.values.toSet.flatMap((_: PType).allNodes)
  }

  /** Note that only a subset of these predicates are needed in a traditional,
    * rule-based type checking/inference algorithm. */
  @SerialVersionUID(0)
  sealed trait TyPredicate {
    def allNodes: Set[PNode]

    def substitute(f: PNode => PNode): TyPredicate = this match {
      case HasName(n, name)          => HasName(f(n), name)
      case UsedAsBool(n)             => UsedAsBool(f(n))
      case BinaryRel(lhs, rhs, name) => BinaryRel(f(lhs), f(rhs), name)
      case DefineRel(v, expr)        => DefineRel(f(v), expr.substitute(f))
    }
  }

  case class HasName(n: PNode, name: Symbol) extends TyPredicate {
    val allNodes: Set[PNode] = Set(n)
  }

  //noinspection TypeAnnotation
  @SerialVersionUID(8655428000256169439L)
  object BinaryRelCat extends Enumeration {

    /** `a` is a subtype of `b` */
    val subtype = Value

    /** A special case of subtype, only generated by assignment statements (but
      * not variable definitions). We separate
      * these from `subtype` to give the GNN more information. */
    val assign = Value

    /** A special case of subtype, only generated by variable definitions. */
    val equal = Value

    /** class `a` extends type `b`, only generated by class definitions. */
    val inheritance = Value

    /** `a` should have type `b`. This can appear in cast statements or used to fix the
      * the return type of class constructors. */
    val fixType = Value

    /** Similar to `fixType`, but is generated by [[Annot.Fixed]]. This arises in cases
      * like `this` and `super` keyword.  */
    val fixAnnotation = Value

    /** `a` is annotated by the user to have type `b`. */
    val userAnnotation = Value
  }

  case class BinaryRel(lhs: PNode, rhs: PNode, category: BinaryRelCat.Value) extends TyPredicate {
    val allNodes: Set[PNode] = Set(lhs, rhs)
  }

  case class UsedAsBool(n: PNode) extends TyPredicate {
    val allNodes: Set[PNode] = Set(n)
  }

  /** Used to encode structural relations between type variables. e.g.,
    * <code>v1</code> equals to the function type <code>(v2, v3) -> v4</code>. */
  case class DefineRel(v: PNode, expr: PExpr) extends TyPredicate {

    val allNodes: Set[PNode] = expr.allNodes + v
  }

  sealed trait PExprBase {
    def allNodes: Set[PNode]

    def allLabels: Set[Symbol]
  }

  // @formatter:off
  /**
    * A type-level expression
    * e :=                  [[PExpr]]
    *   | n                 [[PNode]]
    *   | (n, ..., n) => n  [[PFunc]]
    *   | n(n, ..., n)      [[PCall]]
    *   | {l: n, ..., l: n} [[PObject]]
    *   | n.l               [[PAccess]]
    */
  // @formatter:on
  @SerialVersionUID(0)
  sealed trait PExpr extends PExprBase {
    def substitute(f: PNode => PNode): PExpr = this match {
      case node: PNode             => f(node)
      case PFunc(args, returnType) => PFunc(args.map(f), f(returnType))
      case PCall(f0, args)         => PCall(f(f0), args.map(f))
      case PObject(fields)         => PObject(fields.mapValuesNow(f))
      case PAccess(obj, label)     => PAccess(f(obj), label)
    }
  }

  case class PFunc(args: Vector[PNode], returnType: PNode) extends PExpr {
    val allNodes: Set[PNode] = args.toSet + returnType

    def allLabels: Set[Symbol] = Set()
  }

  case class PCall(f: PNode, args: Vector[PNode]) extends PExpr {
    val allNodes: Set[PNode] = args.toSet + f

    def allLabels: Set[Symbol] = Set()
  }

  case class PObject(fields: Map[Symbol, PNode]) extends PExpr {
    val allNodes: Set[PNode] = fields.values.toSet

    def allLabels: Set[Symbol] = fields.keySet
  }

  case class PAccess(obj: PNode, label: Symbol) extends PExpr {
    val allNodes: Set[PNode] = Set(obj)

    def allLabels: Set[Symbol] = Set(label)
  }

  sealed trait PSyntheticExpr extends PExprBase

  sealed trait PSyntheticCall extends PSyntheticExpr {
    def ret: PNode
    def f: PNode
    def args: Vector[PNode]
    def signature: Either[PFunc, PFuncType]
  }

  object PSyntheticCall {
    def unapply(
        call: PSyntheticCall
    ): Option[(PNode, PNode, Vector[PNode], Either[PFunc, PFuncType])] =
      Some((call.ret, call.f, call.args, call.signature))

    def signatureOpt(
        libDefs: LibDefs,
        funcs: Map[PNode, PFunc],
        f: PNode
    ): Option[Either[PFunc, PFuncType]] = {
      libDefs.nodeMapping.get(f).flatMap(_.typeOpt).orElse(funcs.get(f)).flatMap {
        case p: PFunc     => Some(Left(p))
        case p: PFuncType => Some(Right(p))
        // TODO: Handle cases like StringConstructor, which is a PTyVar
        case _ => None
      }
    }
  }

  case class PMethodCall(
      ret: PNode,
      obj: PNode,
      label: Symbol,
      field: PNode,
      f: PNode,
      args: Vector[PNode],
      signature: Either[PFunc, PFuncType]
  ) extends PSyntheticCall {
    val allNodes: Set[PNode] = Set(obj, field, f) ++ args

    def allLabels: Set[Symbol] = Set(label)
  }

  object PMethodCall {
    def generate(defineRels: Set[DefineRel], libDefs: LibDefs): Set[PMethodCall] = {
      def definitions[T <: PExpr: ClassTag]: Map[PNode, T] =
        defineRels.collect {
          case DefineRel(v, expr: T) => v -> expr
        }(collection.breakOut)

      val objects = definitions[PObject]
      val accesses = definitions[PAccess]
      val calls = definitions[PCall]
      val funcs = definitions[PFunc]
      calls
        .map {
          case (ret, PCall(f, args)) =>
            for {
              PAccess(instance, label) <- accesses.get(f)
              PCall(constructor, initArgs) <- calls.get(instance)
              PFunc(initParams, obj) <- funcs.get(constructor)
              _ = assert(
                initArgs.size == initParams.size,
                s"mismatched number of arguments to construct $instance of class $obj, found: $initArgs, required: $initParams"
              )
              PObject(fields) <- objects.get(obj)
              field <- fields.get(label)
              signature <- signatureOpt(libDefs, funcs, f)
            } yield PMethodCall(ret, obj, label, field, f, args, signature)
        }
        .collect {
          case Some(x) => x
        }(collection.breakOut)
    }
  }

  case class PFuncCall(
      ret: PNode,
      f: PNode,
      args: Vector[PNode],
      signature: Either[PFunc, PFuncType]
  ) extends PSyntheticCall {
    val allNodes: Set[PNode] = {
      val signatureNodes = signature match {
        case Left(PFunc(args, to)) => args :+ to
        case _                     => Vector.empty
      }
      Set(ret, f) ++ args ++ signatureNodes
    }

    def allLabels: Set[Symbol] = Set.empty
  }

  object PFuncCall {
    def generate(defineRels: Set[DefineRel], libDefs: LibDefs): Set[PFuncCall] = {
      // fixme: DRY in PMethodCall
      def definitions[T <: PExpr: ClassTag]: Map[PNode, T] =
        defineRels.collect {
          case DefineRel(v, expr: T) => v -> expr
        }(collection.breakOut)

      val calls = definitions[PCall]
      val funcs = definitions[PFunc]
      val maybeFuncCalls = for {
        (ret, PCall(f, args)) <- calls
      } yield {
        signatureOpt(libDefs, funcs, f).map(signature => PFuncCall(ret, f, args, signature))
      }
      maybeFuncCalls.collect {
        case Some(x) => x
      }(breakOut)
    }
  }
}

/** Utilities to convert IRs into Predicate Graphs */
object PredicateGraphTranslation {
  import IR._

  class UsageCounter {
    import cats.implicits._

    var usages: Map[PNode, Int] = Map()

    def use(node: PNode): Unit = {
      usages |+|= Map(node -> 1)
    }
  }

  class Translator(allocator: PNodeAllocator) {
    val nodeForAny: PNode = NameDef.anyType.node
    val predicates: mutable.Set[TyPredicate] = mutable.Set[TyPredicate]()
    val typeMap: mutable.HashMap[PType, PNode] = mutable.HashMap[PType, PNode]()
    val fixedReturnType: mutable.HashMap[PNode, PNode] = mutable.HashMap[PNode, PNode]()
    val allClasses: mutable.HashSet[ClassDef] = mutable.HashSet[ClassDef]()

    def addRel(pred: TyPredicate): Unit = {
      pred match {
        case BinaryRel(l, r, _) if l == r =>
        case _ =>
          predicates += pred
      }
    }

    def useCond(cond: PNode): Unit = {
      addRel(UsedAsBool(cond))
    }

    def encodePType(t: PType): PNode =
      typeMap.getOrElseUpdate(
        t,
        t match {
          case PTyVar(node) => node
          case PAny         => nodeForAny
          case PFuncType(args, to) =>
            val args1 = args.map(encodePType)
            val to1 = encodePType(to)
            val n = allocator.newNode(None, isType = true, None)
            addRel(DefineRel(n, PFunc(args1, to1)))
            n
          case PObjectType(fields) =>
            val fields1 = fields.mapValuesNow(encodePType)
            val n = allocator.newNode(None, isType = true, None)
            addRel(DefineRel(n, PObject(fields1)))
            n
        }
      )

    def encodeStmt(stmt: IRStmt): Unit =
      SimpleMath.withErrorMessage(s"Error in IRStmt: $stmt") {
        import BinaryRelCat._

        stmt match {
          case d: VarDef =>
            val lhs = d.node

            def define(rhs: PExpr): Unit = {
              addRel(DefineRel(lhs, rhs))
            }

            d.rhs match {
              case v1: Var =>
                addRel(BinaryRel(lhs, v1.node, equal))
              case FuncCall(f, args) =>
                define(PCall(f, args))
              case ObjLiteral(fields) =>
                define(PObject(fields))
              case IfExpr(cond, e1, e2) =>
                addRel(BinaryRel(e1, lhs, subtype))
                addRel(BinaryRel(e2, lhs, subtype))
                useCond(cond)
              case FieldAccess(receiver, label) =>
                define(PAccess(receiver, label))
              case Cast(expr, node) =>
                addRel(BinaryRel(node, expr, subtype))
                addRel(BinaryRel(lhs, node, fixType))
            }
          case AssignStmt(lhs, rhs) =>
            addRel(BinaryRel(lhs, rhs, assign))
          case ReturnStmt(v, ret) =>
            addRel(BinaryRel(v, ret, assign))
          case IfStmt(cond, e1, e2) =>
            useCond(cond)
            encodeStmt(e1)
            encodeStmt(e2)
          case WhileStmt(cond, body) =>
            useCond(cond)
            encodeStmt(body)
          case block: BlockStmt =>
            block.stmts.foreach(encodeStmt)
          case f: FuncDef =>
            // record return type for constructors
            if (f.funcNode.nameOpt.contains(GStmt.constructorName)) {
              fixedReturnType(f.funcNode) = f.returnType
              assert(
                f.returnType.fromProject,
                s"constructor $f does not return a project type (${f.returnType})"
              )
            }
            addRel(DefineRel(f.funcNode, f.pType))
            encodeStmt(f.body)
          case c: ClassDef =>
            c.superTypes.foreach(
              tv => addRel(BinaryRel(c.classNode, tv.node, inheritance))
            )
            addRel(DefineRel(c.classNode, c.pType))
            allClasses += c
            c.funcDefs.values.foreach(encodeStmt)
        }
      }
  }

  def fromIRModules(
      fixedAnnotations: Map[PNode, PType],
      allocator: PNodeAllocator,
      modules: Vector[IRModule]
  ): PredicateGraph = {
    val trans = new Translator(allocator)
    import trans.{encodeStmt, encodePType, addRel, predicates, fixedReturnType}
    modules.foreach(_.stmts.foreach(encodeStmt))

    // type alias of the form 'type A = B' is used as equality;
    // otherwise, allocate a new tVar for 'A'
    fixedAnnotations.foreach {
      case (n, PTyVar(n1)) =>
        addRel(DefineRel(n, n1))
      case (n, t) =>
        val tn = encodePType(t)
        addRel(BinaryRel(n, tn, BinaryRelCat.fixAnnotation))
    }

    predicates.collect {
      case DefineRel(n, PCall(f, _)) if fixedReturnType.contains(f) =>
        addRel(BinaryRel(n, fixedReturnType(f), BinaryRelCat.fixType))
    }

    val totalMapping = modules.foldLeft(Map[PNode, PAnnot]())(_ ++ _.mapping)
    totalMapping.keySet.foreach { n =>
      n.nameOpt.foreach { name =>
        addRel(HasName(n, name))
      }
    }

    val libInMapping = totalMapping.filter(_._1.fromLib)
    assert(libInMapping.isEmpty, s"lib node in totalMapping: $libInMapping")

    val predSet = predicates.toSet
    val allNodes = totalMapping.keySet ++ predSet.flatMap(_.allNodes)

    PredicateGraph(
      allNodes,
      predSet,
      userAnnotations = Map(),
      typeMap = trans.typeMap.toMap,
    )
  }

}

object PredicateGraphLoader {
  lazy val libDefs: LibDefs =
    announced(s"loading library definitions from $libDefsFile...") {
      SimpleMath.readObjectFromFile[LibDefs](libDefsFile.toIO)
    }

  def load(dir: Path): ParsedProject =
    load(dir, libDefs)

  def load(dir: Path, libDefs: LibDefs): ParsedProject =
    parseProject(
      libDefs,
      dir / up,
      dir,
      skipSet = Set(),
      errorHandler = ErrorHandler(ErrorHandler.StoreError, ErrorHandler.StoreError),
      shouldPrintProject = true,
      predictAny = true,
    ).mergeEqualities
}
