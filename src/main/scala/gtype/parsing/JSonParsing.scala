package gtype.parsing

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

object JsonParsing {
  import fastparse._, NoWhitespace._
  def stringChars(c: Char): Boolean = c != '\"' && c != '\\'

  def space[_: P]: P[Unit] = P(CharsWhileIn(" \r\n", 0))
  def digits[_: P]: P[Unit] = P(CharsWhileIn("0-9"))
  def exponent[_: P]: P[Unit] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional[_: P]: P[Unit] = P("." ~ digits)
  def integral[_: P]: P[Unit] = P("0" | CharIn("1-9") ~ digits.?)

  def number[_: P]: P[Js.Num] =
    P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
      x => Js.Num(x.toDouble)
    )

  def `null`[_: P]: P[Js.Null.type] = P("null").map(_ => Js.Null)
  def `false`[_: P]: P[Js.False.type] = P("false").map(_ => Js.False)
  def `true`[_: P]: P[Js.True.type] = P("true").map(_ => Js.True)

  def hexDigit[_: P]: P[Unit] = P(CharIn("0-9a-fA-F"))
  def unicodeEscape[_: P]: P[Unit] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[_: P]: P[Unit] = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))

  def strChars[_: P]: P[Unit] = P(CharsWhile(stringChars))
  def string[_: P]: P[Js.Str] =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)

  def singleQuoteStringChars(c: Char): Boolean = c != '\'' && c != '\\'
  def singleQuoteString[_: P]: P[Js.Str] =
    P(space ~ "'" ~/ (P(CharsWhile(singleQuoteStringChars)) | escape).rep.! ~ "'")
      .map(Js.Str)

  def array[_: P]: P[Js.Arr] =
    P("[" ~/ jsonExpr.rep(sep = ","./) ~ space ~ "]").map(Js.Arr(_: _*))

  def pair[_: P]: P[(String, Js.Val)] = P(string.map(_.value) ~/ ":" ~/ jsonExpr)

  def obj[_: P]: P[Js.Obj] =
    P("{" ~/ pair.rep(sep = ","./) ~ space ~ "}").map(ps => Js.Obj(ps.toMap))

  def jsonExpr[_: P]: P[Js.Val] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )
}
