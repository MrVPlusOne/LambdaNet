package lambdanet.train

import ammonite.ops._

/** Use text files to control the training loop (stop, restore, etc) */
private object TrainingControl {
  val stopFile: Path = pwd / "running-result" / "control" / "stop.txt"
  val restoreFile: Path = pwd / "running-result" / "control" / "restore.txt"

  def shouldStop(consumeFile: Boolean): Boolean = {
    val stop = exists(stopFile)
    if (consumeFile && stop) {
      rm(stopFile)
    }
    stop
  }

  /** If [[restoreFile]] exists, read the path from the file.
    * @param consumeFile if set to true, delete [[restoreFile]] after reading. */
  def restoreFromFile(consumeFile: Boolean): Option[Path] = {
    val restore = exists(restoreFile)
    if (restore) {
      val content = read(restoreFile).trim
      val p = try Path(content)
      catch {
        case _: IllegalArgumentException => pwd / RelPath(content)
      }
      if (consumeFile) {
        rm(restoreFile)
      }
      Some(p / "trainingState.serialized")
    } else None
  }
}
