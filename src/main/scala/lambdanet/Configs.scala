package lambdanet

import ammonite.ops._
import lambdanet.Configs.loadOrCreate

import java.io.FileNotFoundException

case class Configs(configDir: Path = pwd / "configs") {
  def numOfThreads(): Int = {
    loadOrCreate(configDir / "threads.txt", _.toInt) {
      Runtime.getRuntime.availableProcessors() / 2
    }
  }

  def resultsDir(): Path = {
    val f = configDir / "resultsDir.txt"
    loadOrCreate(
      f,
      Configs.readFilePath
    ){
      throw new FileNotFoundException(
        s"$f not exits. It should contain the file " +
          s"path that specifies which directory the training results would be saved to."
      )
    }
  }

  def modelDir(): Path = {
    val f = configDir / "modelPath.txt"
    loadOrCreate(
      f,
      Configs.readFilePath
    ){
      throw new FileNotFoundException(
        s"$f not exits. It should contain the directory of the trained model" +
          s" (which contains the `model.serialized` file)."
      )
    }
  }
}

object Configs {
  def readFilePath(str: String): Path = {
    if(str.startsWith("/")) Path(str)
    else pwd / RelPath(str)
  }

  def loadOrCreate[T](file: Path, parser: String => T, writer: T => String = (x: T) => x.toString)(
      default: => T
  ): T =
    if (exists(file)) {
      parser(read(file).trim)
    } else {
      val v = default
      write.over(file, writer(v))
      v
    }
}
