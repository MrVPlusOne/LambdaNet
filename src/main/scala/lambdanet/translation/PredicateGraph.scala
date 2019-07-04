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
) extends Serializable {}

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
      val prefix = if (fromLib) "𝓛"else "𝓟"
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

    override def toString: String = pPrint(0)
  }

  case object PAny extends PType {
    val madeFromLibTypes = true

    val allLabels: Set[Symbol] = Set()

    val allNodes: Set[PNode] = Set()
  }

  case class PTyVar(node: PNode) extends PType {
    assert(node.isType)

    val madeFromLibTypes: Boolean = node.fromLib

    def allLabels: Set[Symbol] = Set()

    def allNodes: Set[PNode] = Set(node)
  }

  case class PFuncType(args: Vector[PType], to: PType) extends PType {
    val madeFromLibTypes: Boolean =
      args.forall(_.madeFromLibTypes) && to.madeFromLibTypes

    def allLabels: Set[Symbol] =
      to.allLabels ++ args.toSet.flatMap((_: PType).allLabels)

    def allNodes: Set[PNode] = to.allNodes ++ args.flatMap((_: PType).allNodes)
  }

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
  }

  case class HasName(n: PNode, name: Symbol) extends TyPredicate {
    val allNodes: Set[PNode] = Set(n)
  }

  case class FixedToType(n: PNode, ty: PType) extends TyPredicate {
    val allNodes: Set[PNode] = Set(n) ++ ty.allNodes
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
      modules: Vector[IRModule],
  ): PredicateGraph = {
    val predicates = mutable.Set[TyPredicate]()

    def add(pred: TyPredicate): Unit = {
      predicates += pred
    }

    def useCond(cond: PNode): Unit = {
      add(UsedAsBool(cond))
    }

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
                define(v1.node)
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
            c.superType.foreach(
              tv => add(InheritanceRel(c.classNode, tv.node)),
            )
            add(DefineRel(c.classNode, c.pType))
            c.funcDefs.values.foreach(encodeStmt)
        }
      }

    modules.foreach(_.stmts.foreach(encodeStmt))

    fixedAnnotations.foreach {
      case (n, t) =>
        add(FixedToType(n, t))
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
      predSet,
    )
  }

}
