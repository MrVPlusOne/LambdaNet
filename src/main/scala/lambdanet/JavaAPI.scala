package lambdanet

import ammonite.{ops => amm}
import amm.{Path, RelPath}

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
}
