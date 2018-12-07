package funcdiff

import java.io._

import botkop.numsca.Tensor

trait ParameterAttribute extends Serializable

object ParameterAttribute {
  @SerialVersionUID(0)
  case object NeedRegularization extends ParameterAttribute
}

@SerialVersionUID(0)
class Param(var node: ParamNode, val attributes: Set[ParameterAttribute] = Set()){
  def path: SymbolPath = node.path

  override def toString: String = {
    s"Param($path, attributes=$attributes, valueShape=${TensorExtension.showShape(node.shape)})"
  }
}

@SerialVersionUID(0)
case class SymbolPath(path: List[Symbol]){
  def / (symbol: Symbol): SymbolPath = SymbolPath(symbol +: path)

  override def toString: String = {
    path.reverse.mkString("(", "/", ")")
  }
}

object SymbolPath{
  val empty = SymbolPath(List())
}

object ParamCollection {
  def saveObjectToFile(path: File)(obj: Serializable): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(path))
    try{
      oos.writeObject(obj)
    } finally {
      oos.close()
    }
  }

  def readObjectFromFile[T](path: File): T = {
    val ois = new ObjectInputStream(new FileInputStream(path))
    try{
      val obj = ois.readObject.asInstanceOf[T]
      obj
    } finally {
      ois.close()
    }
  }

  def fromFile(file: File): ParamCollection = {
    val (paramMap, constMap) = readObjectFromFile[
      (List[(SymbolPath, Map[String, Object])], List[(SymbolPath, (Array[Int], Array[Double]))])](file)

    val pc = ParamCollection()

    paramMap.foreach{ case( path, param) =>
      val shape = param("shape").asInstanceOf[Array[Int]]
      val data = param("data").asInstanceOf[Array[Double]]
      val attributes = param("attributes").asInstanceOf[List[ParameterAttribute]].toSet
      val p1 = param("path").asInstanceOf[SymbolPath]
      assert(p1 == path, s"path: $path, restored: $p1")

      val value = Tensor(data).reshape(shape)
      pc.getParam(path, attributes){ value }
    }
    constMap.foreach{ case (path, (shape, data)) =>
      pc.getConst(path){ Tensor(data).reshape(shape) }
    }
    pc
  }
}

case class ParamCollection() {
  import collection.mutable
  private val _paramMap =  mutable.Map[SymbolPath, Param]()
  private val _constMap = mutable.Map[SymbolPath, Tensor]()

  def getParam(path: SymbolPath, attributes: => Set[ParameterAttribute] = Set())(init: => Tensor): Param = synchronized {
    _paramMap.getOrElse(path, {
      val p = new Param(new ParamNode(init, path), attributes)
      _paramMap(path) = p
      p
    })
  }

  def getVar(path: SymbolPath, attributes: => Set[ParameterAttribute] = Set())(init: => Tensor): ParamNode = {
    val p = getParam(path, attributes)(init)
    p.node
  }

  def getConst(path: SymbolPath)(init: => Tensor): Tensor = synchronized {
    _constMap.getOrElse(path, {
      _constMap(path) = init
      init
    })
  }

  def allParams: List[Param] = synchronized{
    _paramMap.values.toList
  }

  def paramMap: Map[SymbolPath, Param] = _paramMap.toMap

  def constMap: Map[SymbolPath, Tensor] = _constMap.toMap

  def saveToFile(file: File): Unit = {
    val parameterData = paramMap.mapValues { param =>
      val node = param.node
      val paramData = Map[String, Serializable](
        "shape" -> node.shape,
        "data" -> node.value.data,
        "attributes" -> param.attributes.toList,
        "path" -> param.path
      )
      paramData
    }.toList

    val constantData = constMap.mapValues{ t =>
      (t.shape, t.data)
    }.toList

    ParamCollection.saveObjectToFile(file)((parameterData, constantData))
  }
}
