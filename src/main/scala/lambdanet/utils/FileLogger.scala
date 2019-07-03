package lambdanet.utils

import lambdanet._
import ammonite.ops._
import botkop.numsca.Tensor
import funcdiff.TensorExtension

class FileLogger(file: Path, printToConsole: Boolean) {

  def append(s: String) = {
    write.append(file, s)
  }

  def println(obj: Any): Unit = {
    val s = obj.toString
    append(s)
    append("\n")
    if (printToConsole) {
      System.out.println(s)
    }
  }

  def print(obj: Any): Unit = {
    val s = obj.toString
    append(s)
    if (printToConsole) {
      System.out.print(s)
    }
  }

  def printSection[A](name: String)(content: => A): A = {
    println(s"[$name]")
    val r = content
    println(s"[End of $name]\n")
    r
  }
}

object EventLogger {
  sealed trait EventValue

  case class DoubleValue(v: Double) extends EventValue

  case class MapValue(v: Vector[(String, Double)]) extends EventValue

  case class Event(name: String, iteration: Double, value: EventValue)

  case class PlotConfig(options: String*)
}

import lambdanet.utils.EventLogger._

class EventLogger(
    file: Path,
    printToConsole: Boolean,
    overrideMode: Boolean,
) {

  if (exists(file) && overrideMode) {
    rm(file)
  }
  private val fLogger = new FileLogger(file, printToConsole = false)

  @deprecated
  def log(name: String, iteration: Int, value: Tensor): Unit = {
    ???
  }

  def logScalar(name: String, iteration: Double, value: Double): Unit = {
    log(Event(name, iteration, DoubleValue(value)))
  }

  def logMap(
      name: String,
      iteration: Int,
      value: Vector[(String, Double)],
  ): Unit = {
    log(Event(name, iteration, MapValue(value)))
  }

  def logOpt(name: String, iteration: Int, value: Option[Double]): Unit = {
    value.foreach(v => logScalar(name, iteration, v))
  }

  def logString(name: String, epoch: Double, str: String) = {
    fLogger.println(
      s"""{"$name", $epoch, $str}""",
    )
    if (printToConsole) {
      println(resultStr(s"[$epoch]$name: $str"))
    }
  }

  def log(event: Event): Unit = {
    import event.{name, iteration}
    import TensorExtension.mamFormat

    val str = event.value match {
      case DoubleValue(v) => mamFormat(v)
      case MapValue(map) =>
        map.map { case (k, v) => s"{$k,$v}" }.mkString("{", ", ", "}")
    }

    logString(name, iteration, str)
  }
}
