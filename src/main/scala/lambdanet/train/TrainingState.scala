package lambdanet.train

import ammonite.ops.Path
import funcdiff.{Optimizer, ParamCollection}
import lambdanet._
import lambdanet.utils.{EventLogger, FileLogger}

object TrainingState {
  def fromFile(file: Path): TrainingState = {
    val map = SM
      .readObjectFromFile[List[(String, Any)]](file.toIO)
      .toMap
    val step = map("epoch").asInstanceOf[Int]
    val dimMessage = map("dimMessage").asInstanceOf[Int]
    val optimizer = map("optimizer").asInstanceOf[Optimizer]
    val iterationNum = map("iterationNum").asInstanceOf[Int]
    TrainingState(step, dimMessage, iterationNum, optimizer)
  }

  val iterationNum: Int = 4

  def loadTrainingState(
      resultsDir: Path,
      logger: FileLogger
  ): (TrainingState, ParamCollection, EventLogger) = {
    import ammonite.ops._
    import logger._
    val loggerFile = resultsDir / "log.txt"
    def mkEventLogger(overrideMode: Boolean) = {
      new EventLogger(
        loggerFile,
        printToConsole = true,
        overrideMode = overrideMode
      )
    }

    announced("loadTrainingState") {
      val loadFromFile: Option[Path] =
        TrainingControl(resultsDir).restoreFromFile(consumeFile = true)

      loadFromFile
        .map { p =>
          cp.over(p / "log.txt", loggerFile)
          val s = announced("Loading training from file: " + p) {
            TrainingState.fromFile(p / "state.serialized")
          }
          val pc = ParamCollection.fromFile((p / "params.serialized"))
          (s, pc, mkEventLogger(overrideMode = false))
        }
        .getOrElse {
          val resultsDirEmpty = ls(resultsDir) == Seq(logger.file)
          require(
            resultsDirEmpty,
            s"directory $resultsDir is not empty. Clear or remove it first."
          )
          mkdir(resultsDir / "control")
          val state = TrainingState(
            epoch0 = 0,
            dimMessage = 32,
            optimizer = Optimizer.Adam(learningRate = 1e-3),
            iterationNum = iterationNum
          )
          (state, new ParamCollection(), mkEventLogger(overrideMode = true))
        }
        .tap(println)
    }
  }

}

case class TrainingState(
    epoch0: Int,
    dimMessage: Int,
    iterationNum: Int,
    optimizer: Optimizer
) {
  def saveToFile(file: Path): Unit = {
    val toSave =
      List[(String, Any)](
        "epoch" -> epoch0,
        "dimMessage" -> dimMessage,
        "iterationNum" -> iterationNum,
        "optimizer" -> optimizer
      )
    SM.saveObjectToFile(file.toIO)(toSave)
  }

  override def toString: String = {
    s"""TrainingState:
       |  epoch: $epoch0
       |  dimMessage: $dimMessage
       |  iterationNum: $iterationNum
       |  optimizer: $optimizer
       """.stripMargin
  }
}
