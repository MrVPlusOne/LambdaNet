package plots

import scala.language.implicitConversions

object MathematicaUtils {
  case class Code(build: StringBuilder => Unit) {
    def call(args: Code*): Code =
      code(this, mkCode(args, ",", "[", "]"))

    def -->(c: Code): Code =
      code(this, "->", c)

    def buildString: String = {
      val b = new StringBuilder()
      build(b)
      b.toString()
    }
  }

  implicit def string2Code(s: String): Code = Code(b => b.append(s))

  implicit def real2Code(real: Real): Code =
    "(%g)".format(real).replace("e", "*10^")

  implicit def int2Code(i: Int): Code =
    i.toString

  implicit def bool2Code(b: Boolean): Code =
    if (b) "True" else "False"

  def code(xs: Code*): Code = Code(b => xs.foreach(_.build(b)))

  def mkCode(xs: Seq[Code], sep: Code, start: Code = "", end: Code = ""): Code =
    Code { b =>
      start.build(b)
      xs.headOption.foreach { y =>
        y.build(b)
      }
      xs.tail.foreach { y =>
        sep.build(b)
        y.build(b)
      }
      end.build(b)
    }

  /** creates a Mathematica string */
  def string(s: String): Code = code("\"", s, "\"")

  implicit def tuple2Code[A, B](
      x: (A, B),
  )(implicit ac: A => Code, bc: B => Code): Code =
    code("{", x._1, ",", x._2, "}")

  implicit def tuple3Code[A, B, C](
      x: (A, B, C),
  )(implicit ac: A => Code, bc: B => Code, cc: C => Code): Code =
    code("{", x._1, ",", x._2, ",", x._3, "}")

  implicit def vec2Code[A](xs: Seq[A])(implicit ac: A => Code): Code =
    mkCode(xs.map(ac), ",", "{", "}")

  implicit def option2Code[A](x: Option[A])(implicit ac: A => Code): Code =
    x match {
      case None    => "None"
      case Some(a) => ac(a)
    }
}
