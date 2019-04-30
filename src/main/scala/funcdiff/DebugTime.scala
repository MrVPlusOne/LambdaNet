package funcdiff

import funcdiff.SimpleMath.TimeLogger

object DebugTime extends TimeLogger() {

  def doTenTimes[T](f: => T): T = {
    f; f; f; f; f; f; f; f; f; f
  }
}
