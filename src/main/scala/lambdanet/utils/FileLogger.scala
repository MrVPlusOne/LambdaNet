package lambdanet.utils

import lambdanet._
import ammonite.ops._
import botkop.numsca.Tensor
import funcdiff.SimpleMath.prettyPrintTime
import funcdiff.{SimpleMath, TensorExtension}
import lambdanet.train.ConfusionMatrix

class FileLogger(val file: Path, printToConsole: Boolean) {

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

  var shouldWarn = true
  def printWarning(str: String): Unit = {
    if (shouldWarn)
      Console.err.println(warnStr("[warn] " + str))
  }

  def printInfo(a: Any): Unit = {
    println(infoStr(a.toString))
  }

  def printResult(a: Any): Unit = {
    println(resultStr(a.toString))
  }

  def announced[A](actionName: String)(action: => A): A = {
    import SimpleMath.prettyPrintTime

    println(infoStr(s"  [start] $actionName started..."))
    val startTime = System.nanoTime()
    action.tap { _ =>
      val took = prettyPrintTime(System.nanoTime() - startTime, 2)
      println(infoStr(s"  [finish] $actionName finished. (took $took)"))
    }
  }
}

object EventLogger {
  sealed trait EventValue

  case class DoubleValue(v: Double) extends EventValue

  case class MapValue(v: Vector[(String, Double)]) extends EventValue

  case class Event(name: String, iteration: Double, value: EventValue)

  case class PlotConfig(options: String*)

  def showVec[T](xs: Seq[T]): String = xs.mkString("{", ",", "}")
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

  def logString(name: String, epoch: Double, str: String): Unit = {
    fLogger.println(
      s"""{"$name", $epoch, $str}""",
    )
    if (printToConsole) {
      println(resultStr(s"[$epoch]$name: $str"))
    }
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

  def logConfusionMatrix(
      name: String,
      epoch: Int,
      confMat: ConfusionMatrix,
      categories: Int,
  ): Unit = {
    val array = Array.fill(categories)(Array.fill(categories)(0))
    confMat.foreach {
      case ((i, j), n) =>
        array(i)(j) = n
    }
    val str = showVec(array.map(xs => showVec(xs.toSeq)).toSeq)
    logString(name, epoch, str)
  }

  def logOpt(name: String, iteration: Int, value: Option[Double]): Unit = {
    value.foreach(v => logScalar(name, iteration, v))
  }

  def log(event: Event): Unit = {
    import event.{name, iteration}
    import TensorExtension.mamFormat

    val str = event.value match {
      case DoubleValue(v) => mamFormat(v)
      case MapValue(map) =>
        showVec(map.map { case (k, v) => s"{$k,$v}" })
    }

    logString(name, iteration, str)
  }
}
