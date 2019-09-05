package lambdanet.utils

import lambdanet._
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph._
import lambdanet.utils.GraphVisualization.MamElement

import scala.collection.mutable
import scala.language.implicitConversions

object PredicateGraphVisualization {
  def asMamGraph(
      libDefs: LibDefs,
      userAnnots: Map[ProjNode, PType],
      graphLayout: String,
      graph: PredicateGraph
  ): String = {
    val g = new GraphVisualization.LabeledGraph()

    val mapping = mutable.HashMap[Either[TyPredicate, PNode], MamElement]()
    implicit def convert1(n: PNode): MamElement =
      mapping.getOrElseUpdate(Right(n), g.newId())
    implicit def convert2(p: TyPredicate): MamElement =
      mapping.getOrElseUpdate(Left(p), g.newId())
    implicit def stringElement(s: String): MamElement =
      MamElement(s""""$s"""")

    graph.nodes.foreach { n =>
      val nodeColor = if (n.fromLib) "Orange" else "Green"
      val nameStr = n.toString
      g.addNode(n, nameStr, nameStr, nodeColor)
    }

    def name(p: TyPredicate): String = p match {
      case HasName(_, _) =>
        "Name"
      case BinaryRel(_, _, name) =>
        name match {
          case BinaryRelCat.subtype => " <: "
          case BinaryRelCat.assign => "  ⃪ "
          case BinaryRelCat.equal => " := "
          case BinaryRelCat.inheritance => "extends"
          case BinaryRelCat.fixType => "fixed"
        }
      case UsedAsBool(_) =>
        "BOOL"
      case DefineRel(_, expr) =>
        expr match {
          case _: PNode      => "Equality"
          case PFunc(_, _)   => "Function"
          case PCall(_, _)   => "Call"
          case PObject(_)    => "Object"
          case PAccess(_, _) => "Access"
        }
    }

    graph.predicates.foreach { p =>
      g.addNode(p, name(p), p.toString, "Blue")
      p match {
        case HasName(n, _) =>
          g.addEdge(p, n, "name")
        case BinaryRel(lhs, rhs, _) =>
          g.addEdge(p, lhs, "lhs")
          g.addEdge(p, rhs, "rhs")
        case UsedAsBool(n) =>
          g.addEdge(p, n, "boolean")
        case DefineRel(v, expr) =>
          g.addEdge(p, v, "defined")
          expr match {
            case node: PNode =>
              g.addEdge(p, node, " = ")
            case PFunc(args, ret) =>
              args.zipWithIndex.foreach {
                case (a, i) => g.addEdge(p, a, s"arg$i")
              }
              g.addEdge(p, ret, s"return")
            case PCall(f, args) =>
              args.zipWithIndex.foreach {
                case (a, i) => g.addEdge(p, a, s"arg$i")
              }
              g.addEdge(p, f, s"called")
            case PObject(fields) =>
              fields.foreach {
                case (l, f) => g.addEdge(p, f, s"'${l.name}'")
              }
            case PAccess(obj, label) =>
              g.addEdge(p, obj, s".'${label.name}'")
          }
      }
    }

    g.toMamFormat(graphLayout, directed = true)
      .replace("\uD835\uDCDF", "P")
      .replace("\uD835\uDCDB", "L")
  }
}
