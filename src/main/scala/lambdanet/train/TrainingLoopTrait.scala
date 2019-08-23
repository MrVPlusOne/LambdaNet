package lambdanet.train

import lambdanet.utils.FileLogger

trait TrainingLoopTrait {

  def toyMod: Boolean
  def taskName: String
  lazy val resultsDir: ammonite.ops.Path = {
    import ammonite.ops._
    val pathText = read(pwd / "configs" / "resultsDir.txt").trim
    val path = util.Try{
      pwd / RelPath(pathText)
    }.getOrElse(Path(pathText))
    path / taskName
  }
  lazy val fileLogger =
    new FileLogger(resultsDir / "console.txt", printToConsole = true)

  def scaleLearningRate(epoch: Int): Double

  protected def readThreadNumber(): Int = {
    import ammonite.ops._
    val f = pwd / "configs" / "threads.txt"
    if (exists(f)) {
      read(f).trim.toInt
    } else {
      Runtime.getRuntime.availableProcessors() / 2
    }
  }
}
