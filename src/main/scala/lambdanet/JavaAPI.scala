package lambdanet

import ammonite.{ops => amm}
import amm.{Path, RelPath}
import funcdiff.SimpleMath.readObjectFromFile
import lambdanet.Annot.{Fixed, Missing, User}
import lambdanet.Surface.GModule
import lambdanet.TypeInferenceService.PredictionResults
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

  def mkSrcSpan(start1: Int, start2: Int, until1: Int, until2: Int, srcFile: ProjectPath): SrcSpan =
    SrcSpan((start1, start2), (until1, until2), srcFile)

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
                         numOfThreads: Int,
                         predictTopK: Int,
                        ): PredictionResults = {
    val predictionService = model.PredictionService(numOfThreads, predictTopK)
    predictionService.predictOnProject(
      sourcePath, gModulesOpt = Some(gModules), warnOnErrors = false
    )
  }

  def extractPredictionFromLocation(
    map: Map[PNode, TopNDistribution[PType]], location: SrcSpan
  ): TopNDistribution[PType] = {
    def matchLocation(span: SrcSpan) =
      span.start == location.start && span.until == location.until
    map
      .collectFirst{ case (k, v) if matchLocation(k.srcSpan.get) => v }
      .getOrElse(throw new Error(s"No prediction for location: $location"))
  }

  def typeForSrcSpanFromMap(map: Map[PNode, TopNDistribution[PType]], location: SrcSpan): String =
    extractPredictionFromLocation(map, location).topValue.showSimple

  def loadModel(modelPath: Path): Model = {
    announced("Load model from cache") {
      readObjectFromFile[Model](modelPath.toIO)
    }
  }
}
