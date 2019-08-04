package lambdanet.utils

import scala.collection.mutable

object GraphVisualization {
  case class MamElement(content: String){
    override def toString: String = content
  }

  class LabeledGraph() {
    type N = MamElement

    case class Edge(from: N, to: N, info: MamElement)
    case class Node(name: String, info: MamElement)

    val nodeInfo = mutable.HashMap[N, Node]()
    val edges = mutable.ListBuffer[Edge]()
    val nodeStyleMap = mutable.HashMap[N, String]()

    def setNodeStyle(id: N, style: String): Unit = {
      nodeStyleMap(id) = style
    }

    def addNode(id: N, name: String, info: MamElement, style: String): Unit = {
      nodeInfo(id) = Node(name, info)
      setNodeStyle(id, style)
    }

    def addEdge(from: N, to: N, info: MamElement): Unit = {
      edges += Edge(from, to, info)
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
            if (info.content.isEmpty) s"$from$arrow$to"
            else s"""Labeled[$from$arrow$to, ${info.content}]"""
        }
        .mkString("{", ",", "}")

      val stylePart =
        nodeStyleMap.map { case (id, s) => s"$id->$s" }.mkString("{", ",", "}")

      s"""Graph[$nodeList,$edgeList,VertexStyle->$stylePart,GraphLayout -> $graphLayout]"""
    }
  }
}
