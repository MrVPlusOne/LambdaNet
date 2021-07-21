package lambdanet

import ammonite.{ops => amm}
import amm.{Path, RelPath}
import funcdiff.SimpleMath.readObjectFromFile
import lambdanet.Annot.{Fixed, Missing, User}
import lambdanet.Surface.GModule
import lambdanet.TypeInferenceService.ModelConfig
import lambdanet.train.TopNDistribution
import lambdanet.translation.PredicateGraph.{PNode, PType}
import lambdanet.utils.Js.Null

import scala.io.StdIn

object JavaAPI {
  def pwd: Path = amm.pwd

  def relPath(path: String): RelPath =
    RelPath(path)

  def absPath(path: String): Path =
    Path(path)

  def joinPath(head: Path, tail: String): Path = head / RelPath(tail)

  def defaultModelConfig: ModelConfig = ModelConfig()

  def predictionService(model: Model, numOfThreads: Int, predictTopK: Int) =
    model.PredictionService(numOfThreads, predictTopK)

  def readLine(): String = StdIn.readLine()

  def main(args: Array[String]): Unit = {
    println("This is a test main function.")
  }

  def tuplet[X, Y, Z](x: X, y: Y, z: Z): (X, Y, Z) =
    (x, y, z)

  def pair[X, Y](x: X, y: Y): (X, Y) =
    (x, y)

  def srcSpan(startLine: Int, startIndex: Int, endLine: Int, endIndex: Int, sourcePath: RelPath): SrcSpan =
    SrcSpan((startLine, startIndex),(endLine, endIndex), sourcePath)

  def userAnnotation[T](ty: T, inferred: Boolean): Annot[T] =
    User[T](ty, inferred)

  def fixed[T](ty: T): Annot[T] =
    Fixed[T](ty)

  def missing: Annot[GType] = Missing

  def option[T](v: T):Option[T] = v match {
    case Null => None
    case _    => Some(v)
  }

  def argsCompatibility(args: Vector[(Symbol, Object, Object)]): Vector[(Symbol, TyAnnot, SrcSpan)] =
    args.asInstanceOf[Vector[(Symbol, TyAnnot, SrcSpan)]]

  def annotationCompatibility(value: Object): Annot[GType] = value.asInstanceOf[Annot[GType]]

  def optionSrcSpanCompatibility(value: Object): Option[SrcSpan] = value.asInstanceOf[Option[SrcSpan]]

  def predictWithGModule(model: Model,
                         sourcePath: Path,
                         gModules: Vector[GModule],
                         numOfThreads: Int, predictTopK: Int,
                        ): Map[PNode, TopNDistribution[PType]] = {
    val predictionService = model.PredictionService(numOfThreads, predictTopK)
    predictionService.predictOnProjectWithGModules(sourcePath, gModules, warnOnErrors = false)
  }

  def typeForSrcSpanFromMap(map: Map[PNode, TopNDistribution[PType]], srcSpan: Option[SrcSpan]): String = {
    var variableType = ""
    map.foreach{case(key, value) => {
      val sourceSpan = key.srcSpan.get
      val targetSpan = srcSpan.get
      if(sourceSpan.start._1 == targetSpan.start._1 &&
          sourceSpan.until._1 == targetSpan.until._1 &&
          sourceSpan.start._2 == targetSpan.start._2 &&
          sourceSpan.until._2 == targetSpan.until._2) {
        variableType = value.topValue.toString
      }
    }}
    variableType
  }
}
