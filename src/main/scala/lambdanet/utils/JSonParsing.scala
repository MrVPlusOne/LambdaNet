package lambdanet.utils

import funcdiff.SimpleMath.Extensions._

/*
 Modified from [https://github.com/lihaoyi/fastparse/blob/master/fastparse/test/src/fastparse/JsonTests.scala]
 */

object Js {
  sealed trait Val extends Any {
    def value: Any
    def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Val =
      this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  case class Str(value: java.lang.String) extends AnyVal with Val
  case class Obj(value: Map[java.lang.String, Val]) extends AnyVal with Val
  case class Arr(value: Val*) extends AnyVal with Val
  case class Num(value: Double) extends AnyVal with Val
  case object False extends Val {
    def value = false
  }
  case object True extends Val {
    def value = true
  }
  case object Null extends Val {
    def value: Null = null
  }
}

// fixme: replace with a library
object JsonParsing {

  def parseJson(text: String): Js.Val = {
    import io.circe._, io.circe.parser._
    import Js._

    def rec(v: Json): Js.Val = {
      if(v.isString){
        Str(v.asString.get)
      }else if(v.isObject){
        Obj(v.asObject.get.toMap.mapValuesNow(rec))
      } else if (v.isArray){
        Arr(v.asArray.get.map(rec): _*)
      } else if (v.isNumber){
        Num(v.asNumber.get.toDouble)
      } else if (v.isBoolean){
        if(v.asBoolean.get) True else False
      } else if(v.isNull){
        Null
      } else {
        throw new scala.Error("Missing case")
      }
    }

    val v = parser.parse(text).right.get
    rec(v)
  }
}
