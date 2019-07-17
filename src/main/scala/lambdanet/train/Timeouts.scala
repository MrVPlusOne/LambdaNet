package lambdanet.train

import ammonite.ops.{exists, pwd, read}
import lambdanet.{printInfo, printResult, printWarning}
import lambdanet.utils.ProgramParsing

object Timeouts {
  import scala.concurrent.duration._
  type Duration = FiniteDuration

  var restartOnTimeout = true

  var forwardTimeout: Duration = 100.seconds
  var optimizationTimeout: Duration = 100.seconds

  def readFromFile(): Unit = {
    val file = pwd / "configs" / "timeouts.json"
    printInfo(s"read timeouts from '$file'")
    if (!exists(file)) {
      printWarning(
        s"Timeouts file not found under '$file', use default timeout values",
      )
    }
    import ProgramParsing._
    val text = read(file)
    printResult(s"use timeouts: \n$text")

    val js = parseJson(text)

    restartOnTimeout = asBoolean(js("restartOnTimeout"))
    forwardTimeout = asNumber(js("forwardTimeout")).seconds
    optimizationTimeout = asNumber(js("optimizationTimeout")).seconds
  }
}
