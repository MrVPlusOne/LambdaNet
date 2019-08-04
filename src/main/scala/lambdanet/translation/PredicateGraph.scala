package lambdanet.translation

import lambdanet.IdAllocator
import PredicateGraph._
import funcdiff.SimpleMath
import lambdanet.translation.ImportsResolution.NameDef

import scala.collection.{GenTraversableOnce, mutable}

@SerialVersionUID(1)
case class PredicateGraph(
    nodes: Set[PNode],
    predicates: Set[TyPredicate],
) extends Serializable {
  def showSizes: String =
    s"{nodes = ${nodes.size}, predicates = ${predicates.size}}"

  def mergeEqualities: (PredicateGraph, Map[PNode, PNode]) = {
    val substitution = predicates
      .collect {
        case DefineRel(v, v1: PNode) => (v, v1)
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

    val g = PredicateGraph(
      nodes.map{substF},
      (predicates -- equalities).map { _.substitute(substF) }
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
        fromLib: Boolean,
    ): PNode = { new PNode(id, nameOpt, isType, fromLib) }

    def unapply(n: PNode): Option[(Int, Option[Symbol], Boolean, Boolean)] = {
      Some((n.id, n.nameOpt, n.isType, n.fromLib))
    }

  }

  @SerialVersionUID(1)
  class PNode(
      protected val id: Int,
      val nameOpt: Option[Symbol],
      val isType: Boolean,
      val fromLib: Boolean,
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
        isType: Boolean,
    ): PNode = {
      useNewId(id => new PNode(id, nameOpt, isType, forLib))
    }

    def newDef(nameOpt: Option[Symbol]) =
      NameDef(
        Some(newNode(nameOpt, isType = false)),
        Some(newNode(nameOpt, isType = true)),
        None,
      )

    def newUnknownDef(nameOpt: Option[Symbol]) =
      NameDef(
        Some(newNode(nameOpt, isType = false)),
        NameDef.unknownDef.ty,
        None,
      )
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

    def pPrint(envPriority: Int): String = {
      def wrap(priority: Int)(content: String) = {
        if (priority < envPriority) s"($content)" else content
      }

      this match {
        case PAny      => "any"
        case PTyVar(n) => n.toString
        case PFuncType(from, to) =>
          wrap(0)(
            from.map(_.pPrint(1)).mkString("(", ",", ")") + "->" + to.pPrint(0),
          )
        case PObjectType(fields) =>
          fields
            .map {
              case (l, t) => s"${l.name}: ${t.pPrint(0)}"
            }
            .mkString("{", ", ", "}")
      }
    }

    def substitute(f: PNode => PNode): PType = this match {
      case PAny         => PAny
      case PTyVar(node) => PTyVar(f(node))
      case PFuncType(args, to) =>
        PFuncType(args.map(_.substitute(f)), to.substitute(f))
      case PObjectType(fields) =>
        PObjectType(fields.mapValuesNow(_.substitute(f)))
    }

    override def toString: String = pPrint(0)
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
      case HasName(n, name)              => HasName(f(n), name)
      case SubtypeRel(sub, sup)          => SubtypeRel(f(sub), f(sup))
      case UsedAsBool(n)                 => UsedAsBool(f(n))
      case AssignRel(lhs, rhs)           => AssignRel(f(lhs), f(rhs))
      case InheritanceRel(child, parent) => InheritanceRel(f(child), f(parent))
      case DefineRel(v, expr)            => DefineRel(f(v), expr.substitute(f))
    }
  }

  case class HasName(n: PNode, name: Symbol) extends TyPredicate {
    val allNodes: Set[PNode] = Set(n)
  }

  case class SubtypeRel(sub: PNode, sup: PNode) extends TyPredicate {
    val allNodes: Set[PNode] = Set(sub, sup)
  }

  case class AssignRel(lhs: PNode, rhs: PNode) extends TyPredicate {
    val allNodes: Set[PNode] = Set(lhs, rhs)
  }

  case class UsedAsBool(n: PNode) extends TyPredicate {
    val allNodes: Set[PNode] = Set(n)
  }

  case class InheritanceRel(child: PNode, parent: PNode) extends TyPredicate {
    val allNodes: Set[PNode] = Set(child, parent)
  }

  case class DefineRel(v: PNode, expr: PExpr) extends TyPredicate {
//    expr match {
//      case n: PNode => assert(v.isType == n.isType)
//      case _ =>
//    }

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
      modules: Vector[IRModule],
  ): PredicateGraph = {
    val predicates = mutable.Set[TyPredicate]()
    val typeMap = mutable.HashMap[PType, PNode]()

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
        },
      )

    def encodeStmt(stmt: IRStmt): Unit =
      SimpleMath.withErrorMessage(s"Error in IRStmt: $stmt") {
        stmt match {
          case d: VarDef =>
            val lhs = d.node

            def define(rhs: PExpr): Unit = {
              add(DefineRel(lhs, rhs))
            }

            d.rhs match {
              case v1: Var =>
                add(AssignRel(lhs, v1.node))
              case FuncCall(f, args) =>
                define(PCall(f, args))
              case ObjLiteral(fields) =>
                define(PObject(fields))
              case IfExpr(cond, e1, e2) =>
                add(SubtypeRel(e1, lhs))
                add(SubtypeRel(e2, lhs))
                useCond(cond)
              case FieldAccess(receiver, label) =>
                define(PAccess(receiver, label))
              case Cast(expr, node) =>
                add(SubtypeRel(node, expr))
                define(node)
            }
          case AssignStmt(lhs, rhs) =>
            add(AssignRel(lhs, rhs))
          case ReturnStmt(v, ret) =>
            add(SubtypeRel(v, ret))
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
            add(DefineRel(f.funcNode, f.pType))
            encodeStmt(f.body)
          case c: ClassDef =>
            c.superTypes.foreach(
              tv => add(InheritanceRel(c.classNode, tv.node)),
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
