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
    val optimizer = map("optimizer").asInstanceOf[Optimizer]
    TrainingState(step, optimizer)
  }

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
          import concurrent.ExecutionContext.Implicits.global
          import concurrent.{Future, Await}
          import concurrent.duration._
          announced("Loading training from file: " + p) {
            val stateF = Future(TrainingState.fromFile(p / "state.serialized"))
            val pcF =
              Future(ParamCollection.fromFile((p / "params.serialized")))
            Await.result(for {
              s <- stateF
              pc <- pcF
            } yield (s, pc, mkEventLogger(overrideMode = false)), 1.hour)
          }

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
            optimizer = Optimizer.Adam(learningRate = 1e-3),
          ).tap(println)
          (state, new ParamCollection(), mkEventLogger(overrideMode = true))
        }
    }
  }

}

case class TrainingState(
    epoch0: Int,
    optimizer: Optimizer
) {
  def saveToFile(file: Path): Unit = {
    val toSave =
      List[(String, Any)](
        "epoch" -> epoch0,
        "optimizer" -> optimizer
      )
    SM.saveObjectToFile(file.toIO)(toSave)
  }

  override def toString: String = {
    s"""TrainingState:
       |  epoch: $epoch0
       |  optimizer: $optimizer
       """.stripMargin
  }
}
