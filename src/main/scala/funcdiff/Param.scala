package funcdiff

import java.io._

import botkop.numsca.{Shape, Tensor}
import SimpleMath.Extensions._

trait ParameterAttribute extends Serializable

object ParameterAttribute {
  @SerialVersionUID(0)
  case object NeedRegularization extends ParameterAttribute
}

@SerialVersionUID(0)
class Param(
    var node: ParamNode,
    val attributes: Set[ParameterAttribute] = Set(),
) {
  def path: SymbolPath = node.path

  override def toString: String = {
    s"Param($path, attributes=$attributes, valueShape=${node.shape})"
  }
}

@SerialVersionUID(0)
case class SymbolPath(path: Vector[Symbol]) {
  def /(symbol: Symbol): SymbolPath = SymbolPath(path :+ symbol)

  def ++(other: SymbolPath) = SymbolPath(path ++ other.path)

  override def toString: String = {
    path.mkString("(", "/", ")")
  }
}

object SymbolPath {
  val empty = SymbolPath(Vector())
}

object ParamCollection {

  @SerialVersionUID(0)
  case class SerializableFormat(
      parameterData: List[(SymbolPath, Map[String, Serializable])],
      constantData: List[(SymbolPath, (Shape, Array[Real]))],
  )

  def fromSerializable(data: SerializableFormat): ParamCollection = {
    val SerializableFormat(paramMap, constMap) = data
    val pc = ParamCollection()

    paramMap.foreach {
      case (path, param) =>
        val shape = param("shape").asInstanceOf[Shape]
        val data = param("data").asInstanceOf[Array[Double]]
        val attributes =
          param("attributes").asInstanceOf[List[ParameterAttribute]].toSet
        val p1 = param("path").asInstanceOf[SymbolPath]
        assert(p1 == path, s"path: $path, restored: $p1")

        val value = Tensor(data).reshape(shape)
        pc.getParam(path, attributes) { value }
    }
    constMap.foreach {
      case (path, (shape, data)) =>
        pc.getConst(path) { Tensor(data).reshape(shape) }
    }
    pc
  }

  def fromFile(file: File): ParamCollection = {
    val data = SimpleMath.readObjectFromFile[SerializableFormat](file)
    fromSerializable(data)
  }

  def main(args: Array[String]): Unit = {
    import ammonite.ops._
    import lambdanet.TrainingCenter._
    import funcdiff.Optimizer.Adam

    val pc = new ParamCollection()
    pc.getVar(SymbolPath.empty / 'abc)(Tensor(1, 2, 3))
    val factory = LayerFactory(SymbolPath.empty / 'layers, pc)
    val s = TrainingState(4, 64, factory, Adam(1e-4), 10)

    val file = pwd / "testFile"
    s.saveToFile(file)
    println {
      TrainingState.fromFile(file)
    }
  }
}

case class ParamCollection() {
  import collection.concurrent
  private val _paramMap = concurrent.TrieMap[SymbolPath, Param]()
  private val _constMap = concurrent.TrieMap[SymbolPath, Tensor]()

  def getParam(
      path: SymbolPath,
      attributes: => Set[ParameterAttribute] = Set(),
  )(
      init: => Tensor,
  ): Param =
    _paramMap.getOrElseUpdate(
      path,
      new Param(new ParamNode(init, path), attributes),
    )

  def getVar(path: SymbolPath, attributes: => Set[ParameterAttribute] = Set())(
      init: => Tensor,
  ): ParamNode = {
    val p = getParam(path, attributes)(init)
    p.node
  }

  def getConst(path: SymbolPath)(init: => Tensor): Tensor =
    _constMap.getOrElseUpdate(path, init)

  def allParams: List[Param] = {
    _paramMap.values.toList
  }

  def paramMap: Map[SymbolPath, Param] = _paramMap.toMap

  def constMap: Map[SymbolPath, Tensor] = _constMap.toMap

  def toSerializable: ParamCollection.SerializableFormat = {
    val parameterData: List[(SymbolPath, Map[String, Serializable])] =
      paramMap.mapValuesNow { param =>
        val node = param.node
        val paramData = Map[String, Serializable](
          "shape" -> node.shape,
          "data" -> node.value.data,
          "attributes" -> param.attributes.toList,
          "path" -> param.path,
        )
        paramData
      }.toList

    val constantData: List[(SymbolPath, (Shape, Array[Real]))] =
      constMap.mapValuesNow { t =>
        (t.shape, t.data)
      }.toList

    ParamCollection.SerializableFormat(parameterData, constantData)
  }

  def saveToFile(file: File): Unit = {
    SimpleMath.saveObjectToFile(file)(toSerializable)
  }
}
