package gtype

import ammonite.ops._
import botkop.numsca.Tensor
import funcdiff.TensorExtension

class FileLogger(file: Path, printToConsole: Boolean) {

  def append(s: String) = {
    write.append(file, s)
  }

  def println(obj: Any): Unit ={
    val s = obj.toString
    append(s)
    append("\n")
    if(printToConsole){
      System.out.println(s)
    }
  }

  def print(obj: Any): Unit ={
    val s = obj.toString
    append(s)
    if(printToConsole){
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

object EventLogger{
  case class Event(name: String, iteration: Int, value: Tensor)
}

import EventLogger._

class EventLogger(file: Path, printToConsole: Boolean, overrideMode: Boolean) {
  if(exists(file) && overrideMode){
    rm(file)
  }
  private val fLogger = new FileLogger(file, printToConsole = false)

  def log(name: String, iteration: Int, value: Tensor): Unit = {
    log(Event(name, iteration, value))
  }

  def log(event: Event): Unit ={
    import event._
    fLogger.println(s"$name, $iteration, ${TensorExtension.mamFormat(value)}")
    if(printToConsole){
      println(s"[$iteration]$name: $value")
    }
  }
}