package funcdiff

import java.io._
import botkop.numsca.Tensor
import ammonite.ops.Path
import org.nd4j.linalg.api.ndarray.INDArray

trait ParameterAttribute extends Serializable

object ParameterAttribute {
  @SerialVersionUID(0)
  case object NeedRegularization extends ParameterAttribute
}

/**
  * Each [[Param]] contains a mutable [[ParamNode]], representing a trainable parameter.
  *
  * Note that in the future, [[Param]] will not be serializable and should only be provided by
  * [[ParamCollection]] to ensure reference consistency. (currently keep it serializable
  * to be able to load previously trained model)
  *
  * todo: make this not serializable
  */
@SerialVersionUID(1L)
class Param(
    var node: ParamNode,
    val attributes: Set[ParameterAttribute] = Set()
) extends Serializable {
  def path: SymbolPath = node.path

  override def toString: String = {
    s"Param($path, attributes=$attributes, valueShape=${node.shape})"
  }
}

@SerialVersionUID(1)
case class SymbolPath(repr: Symbol) {
  def /(seg: Symbol): SymbolPath =
    SymbolPath.appendCache.getOrElseUpdate((repr, seg), {
      SymbolPath(Symbol(repr.name + "/" + seg.name))
    })

  @inline
  def /(seg: String): SymbolPath = this / Symbol(seg)

  def ++(other: SymbolPath): SymbolPath = this / other.repr

  override def toString: String = {
    repr.name
  }
}

object SymbolPath {
  import collection.concurrent.TrieMap
  private val appendCache = TrieMap.empty[(Symbol, Symbol), SymbolPath]

  val empty = SymbolPath(Symbol(""))

  def parse(pathString: String): SymbolPath =
    SymbolPath(Symbol(pathString))
}

object ParamCollection {

  def fromFile(file: Path): ParamCollection =
    fromFile(file.toIO)

  def fromFile(file: File): ParamCollection =
    SimpleMath.readObjectFromFile[ParamCollection](file)
}

@SerialVersionUID(-751724603565436754L)
case class ParamCollection() {
  import collection.concurrent
  private var _paramMap = concurrent.TrieMap[SymbolPath, Param]()
  private var _constMap = concurrent.TrieMap[SymbolPath, Tensor]()

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

  // version 2: only store param nodes' values without paths
  private def writeObject(os: ObjectOutputStream): Unit = {
    val paramData = _paramMap.toArray.map {
      case (k, v) =>
        (k.toString, v.node.value.array, v.attributes)
    }
    os.writeObject(paramData)
    val constData = _constMap.toArray.map {
      case (k, v) =>
        (k.toString, v.array)
    }
    os.writeObject(constData)
  }

  private def readObject(os: ObjectInputStream): Unit = {
    val params = os
      .readObject()
      .asInstanceOf[Array[(String, INDArray, Set[ParameterAttribute])]]
      .map {
        case (ps, data, attrs) =>
          val path = SymbolPath.parse(ps)
          path -> new Param(new ParamNode(Tensor(data), path), attrs)
      }
    val consts = os
      .readObject()
      .asInstanceOf[Array[(String, INDArray)]]
      .map { case (ps, data) => SymbolPath.parse(ps) -> Tensor(data) }
    this._paramMap = concurrent.TrieMap[SymbolPath, Param](params: _*)
    this._constMap = concurrent.TrieMap[SymbolPath, Tensor](consts: _*)
  }

  def saveToFile(file: Path): Unit = {
    saveToFile(file.toIO)
  }

  def saveToFile(file: File): Unit = {
    SimpleMath.saveObjectToFile(file)(this)
  }
}
