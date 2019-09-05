package lambdanet.translation

import lambdanet.IdAllocator
import PredicateGraph._
import funcdiff.SimpleMath
import lambdanet.Surface.GStmt
import lambdanet.translation.ImportsResolution.NameDef

import scala.collection.{mutable}
import lambdanet._

@SerialVersionUID(1)
case class PredicateGraph(
    nodes: Set[PNode],
    predicates: Set[TyPredicate]
) extends Serializable {
  def showSizes: String =
    s"{nodes = ${nodes.size}, predicates = ${predicates.size}}"

  def mergeEqualities: (PredicateGraph, Map[PNode, PNode]) = {
    val substitution = predicates
      .collect {
        case DefineRel(v, v1: PNode) =>
          if (v1.nameOpt.isEmpty) v1.nameOpt = v.nameOpt
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

    val g = PredicateGraph(
      nodes.map { substF },
      predicates1
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
    def toTuple(n: PNode): (Int, Option[Symbol], Boolean, Boolean) =
      (n.id, n.nameOpt, n.isType, n.fromLib)
    def fromTuple(t: (Int, Option[Symbol], Boolean, Boolean)): PNode =
      new PNode(t._1, t._2, t._3, t._4)

    def apply(
        id: Int,
        nameOpt: Option[Symbol],
        isType: Boolean,
        fromLib: Boolean
    ): PNode = { new PNode(id, nameOpt, isType, fromLib) }

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
      val fromLib: Boolean
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

    def getId: Int = id
  }

  @SerialVersionUID(1)
  class PNodeAllocator(val forLib: Boolean)
      extends IdAllocator[PNode]
      with Serializable {

    val unknownDef: NameDef =
      if (forLib) NameDef.makeUnknownDef(this) else null

    def newNode(
        nameOpt: Option[Symbol],
        isType: Boolean
    ): PNode = {
      if (!forLib) {
        nameOpt.foreach { name =>
          if (isType && QLang.basicTypes.contains(name)) {
            throw new Error(s"Trying to override basic type definitions: $name")
          }
        }
      }
      useNewId(id => new PNode(id, nameOpt, isType, forLib))
    }

    def newDef(nameOpt: Option[Symbol]) =
      NameDef(
        Some(newNode(nameOpt, isType = false)),
        Some(newNode(nameOpt, isType = true)),
        None
      )

    val namedUnknownDefs: mutable.HashMap[Symbol, NameDef] = mutable.HashMap()

    def newUnknownDef(nameOpt: Option[Symbol]): NameDef = {
      nameOpt match {
        case None =>
          NameDef(
            Some(newNode(None, isType = false)),
            NameDef.unknownDef.ty,
            None
          )
        case Some(name) =>
          namedUnknownDefs.getOrElseUpdate(
            name,
            NameDef(
              Some(newNode(nameOpt, isType = false)),
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
  sealed trait PType {
    val madeFromLibTypes: Boolean

    def allLabels: Set[Symbol]

    def allNodes: Set[PNode]

    def pPrint[S](
        showNode: PNode => S
    )(implicit convert: String => S, aggregate: Vector[S] => S): S = {
      def rec(envPriority: Int)(ty: PType): S = {
        def wrap(priority: Int)(content: S): S = {
          if (priority < envPriority) aggregate(Vector("(", content, ")")) else content
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
              }.toVector
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
    assert(node.isType)

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

  object BinaryRelCat extends Enumeration {
    val subtype, assign, equal, inheritance, fixType = Value
  }

  case class BinaryRel(lhs: PNode, rhs: PNode, category: BinaryRelCat.Value)
      extends TyPredicate {
    val allNodes: Set[PNode] = Set(lhs, rhs)
  }

  case class UsedAsBool(n: PNode) extends TyPredicate {
    val allNodes: Set[PNode] = Set(n)
  }

  case class DefineRel(v: PNode, expr: PExpr) extends TyPredicate {

    val allNodes: Set[PNode] = expr.allNodes + v
  }

  // @formatter:off
  /**
    * e :=                  [[PExpr]]
    *   | n                 [[PNode]]
    *   | (n, ..., n) => n  [[PFunc]]
    *   | n(n, ..., n)
    *   | {l: n, ..., l: n}
    *   | n.l
    */
  // @formatter:on
  @SerialVersionUID(0)
  sealed trait PExpr {
    def allNodes: Set[PNode]

    def allLabels: Set[Symbol]

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
}

object PredicateGraphTranslation {
  import IR._

  class UsageCounter {
    import cats.implicits._

    var usages: Map[PNode, Int] = Map()

    def use(node: PNode): Unit = {
      usages |+|= Map(node -> 1)
    }
  }

  def fromIRModules(
      fixedAnnotations: Map[PNode, PType],
      allocator: PNodeAllocator,
      nodeForAny: PNode,
      modules: Vector[IRModule]
  ): PredicateGraph = {
    val predicates = mutable.Set[TyPredicate]()
    val typeMap = mutable.HashMap[PType, PNode]()
    val fixedReturnType = mutable.HashMap[PNode, PNode]()

    def add(pred: TyPredicate): Unit = {
      predicates += pred
    }

    def useCond(cond: PNode): Unit = {
      add(UsedAsBool(cond))
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
            val n = allocator.newNode(None, isType = true)
            add(DefineRel(n, PFunc(args1, to1)))
            n
          case PObjectType(fields) =>
            val fields1 = fields.mapValuesNow(encodePType)
            val n = allocator.newNode(None, isType = true)
            add(DefineRel(n, PObject(fields1)))
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
              add(DefineRel(lhs, rhs))
            }

            d.rhs match {
              case v1: Var =>
                add(BinaryRel(lhs, v1.node, equal))
              case FuncCall(f, args) =>
                define(PCall(f, args))
              case ObjLiteral(fields) =>
                define(PObject(fields))
              case IfExpr(cond, e1, e2) =>
                add(BinaryRel(e1, lhs, subtype))
                add(BinaryRel(e2, lhs, subtype))
                useCond(cond)
              case FieldAccess(receiver, label) =>
                define(PAccess(receiver, label))
              case Cast(expr, node) =>
                add(BinaryRel(node, expr, subtype))
                add(BinaryRel(lhs, node, fixType))
            }
          case AssignStmt(lhs, rhs) =>
            add(BinaryRel(lhs, rhs, assign))
          case ReturnStmt(v, ret) =>
            add(BinaryRel(v, ret, assign))
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
            add(DefineRel(f.funcNode, f.pType))
            encodeStmt(f.body)
          case c: ClassDef =>
            c.superTypes.foreach(
              tv => add(BinaryRel(c.classNode, tv.node, inheritance))
            )
            add(DefineRel(c.classNode, c.pType))
            c.funcDefs.values.foreach(encodeStmt)
        }
      }

    modules.foreach(_.stmts.foreach(encodeStmt))

    fixedAnnotations.foreach {
      case (n, t) =>
        val tn = encodePType(t)
        add(DefineRel(n, tn))
    }

    predicates.collect {
      case DefineRel(n, PCall(f, _)) if fixedReturnType.contains(f) =>
        add(BinaryRel(n, fixedReturnType(f), BinaryRelCat.fixType))
    }

    val totalMapping = modules.foldLeft(Map[PNode, PAnnot]())(_ ++ _.mapping)
    totalMapping.keySet.foreach { n =>
      n.nameOpt.foreach(name => add(HasName(n, name)))
    }

    val libInMapping = totalMapping.filter(_._1.fromLib)
    assert(libInMapping.isEmpty, s"lib node in totalMapping: $libInMapping")

    val predSet = predicates.toSet
    val allNodes = totalMapping.keySet ++ predSet.flatMap(_.allNodes)

    PredicateGraph(
      allNodes,
      predSet
    )
  }

}
