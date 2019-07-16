package lambdanet.utils

import lambdanet._
import ammonite.ops._
import botkop.numsca.Tensor
import funcdiff.TensorExtension
import lambdanet.train.ConfusionMatrix

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

  def logConfusionMatrix(name: String, epoch: Int, confMat: ConfusionMatrix, categories: Int): Unit = {
    val array = Array.fill(categories)(Array.fill(categories)(0))
    confMat.foreach{ case ((i,j), n) =>
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
