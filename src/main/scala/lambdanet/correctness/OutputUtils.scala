package lambdanet.correctness

import ammonite.ops.Path
import ammonite.{ops => amm}
import lambdanet.{SM, announced}

object OutputUtils {
  def save(resultsDir: Path, filename: String, obj: Serializable): Unit = {
    if (!amm.exists(resultsDir)) {
      amm.mkdir(resultsDir)
    }
    val path = resultsDir / filename
    announced(s"saving $path") {
      SM.saveObjectToFile(path.toIO)(obj)
    }
  }
}
