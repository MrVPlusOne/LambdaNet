package lambdanet

import ammonite.ops._
import lambdanet.Configs.loadOrCreate

import java.io.FileNotFoundException

case class Configs(configDir: Path = pwd / "configs") {
  def numOfThreads(): Int = {
    loadOrCreate(configDir / "threads.txt", _.trim.toInt) {
      Runtime.getRuntime.availableProcessors() / 2
    }
  }

  def resultsDir(): Path = {
    val f = configDir / "resultsDir.txt"
    loadOrCreate(
      f,
      pathText =>
        util
          .Try {
            pwd / RelPath(pathText)
          }
          .getOrElse(Path(pathText))
    ){
      throw new FileNotFoundException(
        s"$f not exits. It should contain the file " +
          s"path that specifies which directory the training results would be saved to."
      )
    }
  }
}

object Configs {
  def loadOrCreate[T](file: Path, parser: String => T, writer: T => String = (x: T) => x.toString)(
      default: => T
  ): T =
    if (exists(file)) {
      parser(read(file))
    } else {
      val v = default
      write.over(file, writer(v))
      v
    }
}
