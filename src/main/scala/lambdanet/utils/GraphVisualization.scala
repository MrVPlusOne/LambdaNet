package lambdanet.utils

import lambdanet.translation.PredicateGraph.PNode

import scala.collection.mutable

object GraphVisualization {
  class LabeledGraph() {
    case class Edge(from: PNode, to: PNode, info: String)
    case class Node(name: String, info: String)

    val nodeInfo = mutable.HashMap[PNode, Node]()
    val edges = mutable.ListBuffer[Edge]()
    val nodeStyleMap = mutable.HashMap[PNode, String]()

    def setNodeStyle(id: PNode, style: String): Unit = {
      nodeStyleMap(id) = style
    }

    def addNode(id: PNode, name: String, info: String, style: String): Unit = {
      nodeInfo(id) = Node(name, info)
      setNodeStyle(id, style)
    }

    def addEdge(from: PNode, to: PNode, info: String): Unit = {
      edges += Edge(from, to, info: String)
    }

    def toMamFormat(graphLayout: String, directed: Boolean): String = {
      val arrow = if (directed) "->" else "\\[UndirectedEdge]"

      val nodeList = nodeInfo
        .map {
          case (id, Node(name, info)) =>
            s"""Labeled[Tooltip[$id,$info],"$name"]"""
        }
        .mkString("{", ",", "}")

      val edgeList = edges
        .map {
          case Edge(from, to, info) =>
            if (info.isEmpty) s"$from$arrow$to"
            else s"""Labeled[$from$arrow$to, $info]"""
        }
        .mkString("{", ",", "}")

      val stylePart =
        nodeStyleMap.map { case (id, s) => s"$id->$s" }.mkString("{", ",", "}")

      s"""Graph[$nodeList,$edgeList,VertexStyle->$stylePart,GraphLayout -> $graphLayout]"""
    }
  }
}
