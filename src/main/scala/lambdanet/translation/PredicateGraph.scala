package lambdanet.translation

import lambdanet.IdAllocator
import PredicateGraph._
import funcdiff.SimpleMath
import lambdanet.translation.ImportsResolution.NameDef

import scala.collection.{GenTraversableOnce, mutable}

case class PredicateGraph(
    nodes: Set[PNode],
    predicates: Set[TyPredicate],
    libraryTypes: Map[PNode, PType]
) {
  import cats.instances.all._
  import cats.Monoid
  import cats.syntax.either._
  type ObjNode = PNode

  /** which label is accessed on which variable as what */
  val fieldUsages: Map[Symbol, Set[(ObjNode, PNode)]] =
    Monoid.combineAll(predicates.collect {
      case DefineRel(v, PAccess(objType, l)) =>
        Map(l -> Set(objType -> v))
    })

  /** which label is defined in which class as what */
  val fieldDefs: Map[Symbol, Set[(ObjNode, Either[PType, PNode])]] =
    Monoid.combineAll(
      predicates.toVector.collect {
        case DefineRel(v, PObject(fields)) =>
          fields.flatMap {
            case (l, t) => Map(l -> Set(v -> t.asRight[PType]))
          }
      } ++ libraryTypes.collect {
        case (node, PObjectType(fields)) =>
          fields.flatMap {
            case (l, t) => Map(l -> Set(node -> t.asLeft[PNode]))
          }
      }
    )

  lazy val allNodes: Set[PNode] = {
    nodes ++ predicates.flatMap(_.allNodes)
  }

  def printStat(): Unit = {
    val nodeNum = nodes.size
    val predicatesNum = predicates.size
    println(s"Stats{nodeNum: $nodeNum, predicates: $predicatesNum}")
  }
}

object PredicateGraph {

  @SerialVersionUID(1)
  class PNode(
      protected val id: Int,
      val nameOpt: Option[Symbol],
      val isType: Boolean,
      val fromLib: Boolean
  ) extends PExpr
      with Serializable {
    def isTerm: Boolean = !isType

    def showDetails: String = {
      val parts = nameOpt.map(n => s"{${n.name}}").toList
      s"𝒯$id${parts.mkString}"
    }

    override def toString: String = {
      val namePart = nameOpt.map(n => s"{${n.name}}").getOrElse("")
      s"𝒯$id$namePart"
    }

    override def equals(obj: Any): Boolean = obj match {
      case p: PNode =>
        (id, isType) == (p.id, p.isType)
      case _ => false
    }

    override def hashCode(): Int = {
      (id, isType).hashCode()
    }

    def allNodes: Set[PNode] = Set(this)
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
      useNewId(id => new PNode(id, nameOpt, isType, forLib))
    }
  }

  sealed trait PType {
    val madeFromLibTypes: Boolean
  }

  case object PAny extends PType {
    val madeFromLibTypes = true
  }

  case class PTyVar(node: PNode) extends PType {
    assert(node.isType)

    val madeFromLibTypes: Boolean = node.fromLib
  }

  case class PFuncType(args: Vector[PType], to: PType) extends PType {
    val madeFromLibTypes
        : Boolean = args.forall(_.madeFromLibTypes) && to.madeFromLibTypes
  }

  case class PObjectType(fields: Map[Symbol, PType]) extends PType {
    val madeFromLibTypes: Boolean = fields.forall(_._2.madeFromLibTypes)
  }

  sealed trait TyPredicate {
    def allNodes: Set[PNode]

  }

  case class HasLibType(v: PNode, ty: PType) extends TyPredicate {
    val allNodes: Set[PNode] = Set(v)
    assert(ty.madeFromLibTypes)
  }

  case class SubtypeRel(sub: PNode, sup: PNode) extends TyPredicate{
    val allNodes: Set[PNode] = Set(sub, sup)
  }

  case class AssignRel(lhs: PNode, rhs: PNode) extends TyPredicate{
    val allNodes: Set[PNode] = Set(lhs, rhs)
  }

  case class UsedAsBool(n: PNode) extends TyPredicate{
    val allNodes: Set[PNode] = Set(n)
  }

  case class InheritanceRel(child: PNode, parent: PNode) extends TyPredicate{
    val allNodes: Set[PNode] = Set(child, parent)
  }

  case class DefineRel(v: PNode, expr: PExpr) extends TyPredicate{
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
  sealed trait PExpr{
    def allNodes: Set[PNode]
  }

  case class PFunc(args: Vector[PNode], returnType: PNode) extends PExpr{
    val allNodes: Set[PNode] = args.toSet + returnType
  }

  case class PCall(f: PNode, args: Vector[PNode]) extends PExpr{
    val allNodes: Set[PNode] = args.toSet + f
  }

  case class PObject(fields: Map[Symbol, PNode]) extends PExpr{
    val allNodes: Set[PNode] = fields.values.toSet
  }

  case class PAccess(obj: PNode, label: Symbol) extends PExpr{
    val allNodes: Set[PNode] = Set(obj)
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
      modules: Vector[IRModule]
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
              tv => add(InheritanceRel(c.classNode, tv.node))
            )
            add(DefineRel(c.classNode, c.pType))
            c.funcDefs.values.foreach(encodeStmt)
        }
      }

    modules.foreach(_.stmts.foreach(encodeStmt))

    val totalMapping = modules.foldLeft(Map[PNode, PAnnot]())(_ ++ _.mapping)
    val libInMapping = totalMapping.filter(_._1.fromLib)
    assert(libInMapping.isEmpty, s"lib node in totalMapping: $libInMapping")

    // todo: compute a complete node set

    val libTypes = for {
      (n, annot) <- totalMapping if n.fromLib && n.isType
      ty <- annot.typeOpt
    } yield n -> ty //fixme: this is incomplete, should include all usages
    PredicateGraph(totalMapping.keySet, predicates.toSet, libTypes)
  }

}
