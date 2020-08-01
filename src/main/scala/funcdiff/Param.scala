package funcdiff

import java.io._

import botkop.numsca.{Shape, Tensor}
import SimpleMath.Extensions._
import ammonite.ops.Path
import funcdiff.ParamCollection.SerializableFormat
import lambdanet.ChainingSyntax
import org.nd4j.linalg.api.ndarray.INDArray

trait ParameterAttribute extends Serializable

object ParameterAttribute {
  @SerialVersionUID(0)
  case object NeedRegularization extends ParameterAttribute
}

@SerialVersionUID(1)
class Param(
    var node: ParamNode,
    val attributes: Set[ParameterAttribute] = Set()
) extends Serializable {
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
      constantData: List[(SymbolPath, INDArray)]
  )

  def fromSerializable(data: SerializableFormat): ParamCollection =
    ParamCollection().tap{
      _.appendDataFromSerializable(data)
    }

  def fromFile(file: Path): ParamCollection = {
    fromFile(file.toIO)
  }

  def fromFile(file: File): ParamCollection = {
    val data = SimpleMath.readObjectFromFile[SerializableFormat](file)
    fromSerializable(data)
  }
}

case class ParamCollection() {
  import collection.concurrent
  private val _paramMap = concurrent.TrieMap[SymbolPath, Param]()
  private val _constMap = concurrent.TrieMap[SymbolPath, Tensor]()

  def getParam(
      path: SymbolPath,
      attributes: => Set[ParameterAttribute] = Set()
  )(
      init: => Tensor
  ): Param =
    _paramMap.getOrElseUpdate(
      path,
      new Param(new ParamNode(init, path), attributes)
    )

  def getVar(path: SymbolPath, attributes: => Set[ParameterAttribute] = Set())(
      init: => Tensor
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
          "array" -> node.value.array,
          "attributes" -> param.attributes.toList,
          "path" -> param.path
        )
        paramData
      }.toList

    val constantData: List[(SymbolPath, INDArray)] =
      constMap.mapValuesNow { t =>
        t.array
      }.toList

    ParamCollection.SerializableFormat(parameterData, constantData)
  }

  def saveToFile(file: Path): Unit = {
    saveToFile(file.toIO)
  }

  def saveToFile(file: File): Unit = {
    SimpleMath.saveObjectToFile(file)(toSerializable)
  }

  private def appendDataFromSerializable(data: SerializableFormat): Unit = {
    val SerializableFormat(paramMap, constMap) = data
    paramMap.foreach {
      case (path, param) =>
        val data = param("array").asInstanceOf[INDArray]
        val attributes =
          param("attributes").asInstanceOf[List[ParameterAttribute]].toSet
        val p1 = param("path").asInstanceOf[SymbolPath]
        assert(p1 == path, s"path: $path, restored: $p1")

        val value = Tensor(data)
        getParam(path, attributes) { value }
    }
    constMap.foreach {
      case (path, array) =>
        getConst(path) { Tensor(array) }
    }
  }

//  private def readObject(stream: ObjectInputStream): Unit = {
//    val o = stream.readObject()
//    appendDataFromSerializable(o.asInstanceOf[SerializableFormat])
//  }
//
//  private def writeObject(stream: ObjectOutputStream): Unit = {
//    stream.writeObject(toSerializable)
//  }
}
