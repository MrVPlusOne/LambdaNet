package lambdanet.train

import lambdanet.utils.FileLogger

trait TrainingLoopTrait {

  def toyMod: Boolean
  def taskName: String
  lazy val resultsDir = {
    import ammonite.ops._
    pwd / "running-result" / taskName
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
