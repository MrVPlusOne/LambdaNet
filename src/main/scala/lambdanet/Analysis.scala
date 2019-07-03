package lambdanet

import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph._

object Analysis {
  object Reasoning extends Enumeration {
    val Constructor, Local, Naming, FieldAccess = Value
  }

  case class GraphAnalysis(distanceToConstNode: PNode => Int)

  val Inf = 1000

  def analyzeGraph(g: PredicateGraph): GraphAnalysis = {
    import collection.mutable

    val predicates: Map[PNode, Set[TyPredicate]] = {
      g.predicates
        .flatMap(p => p.allNodes.map(n => n -> p))
        .groupBy(_._1)
        .map { case (k, v) => k -> v.map(_._2) }
    }

    def neighbours(n: PNode): Set[PNode] = {
      predicates.getOrElse(n, Set()).flatMap(_.allNodes) - n
    }

    // dijkstra's algorithm
    val constDis = mutable.HashMap[PNode, Int]()

    g.nodes.foreach(n => {
      val dis =
        if (n.fromLib || predicates(n).any(_.isInstanceOf[UsedAsBool])) 0
        else Inf
      constDis(n) = dis
    })

    val frontier = mutable.PriorityQueue(g.nodes.toSeq: _*)(
      (x: PNode, y: PNode) => -constDis(x).compare(constDis(y)),
    )

    val frontierSet = mutable.HashSet(g.nodes.toSeq: _*)

    while (frontier.nonEmpty) {
      val n = frontier.dequeue()
      frontierSet -= n
      neighbours(n).intersect(frontierSet).foreach { v =>
        val alt = constDis(n) + 1
        constDis(v) = alt min constDis(v)
      }
    }

    GraphAnalysis(
      distanceToConstNode = constDis.apply,
    )
  }

}
