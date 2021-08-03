package plots

object JSUtils {

  import scala.language.implicitConversions

  case class JSCode(build: StringBuilder => Unit) {
    def call(args: JSCode*): JSCode =
      code(this, mkCode(args, ",", "(", ")"))

    def -->(c: JSCode): JSCode =
      code(this, ":", c)

    def buildString: String = {
      val b = new StringBuilder()
      build(b)
      b.toString()
    }
  }

  implicit def string2Code(s: String): JSCode = JSCode(b => b.append(s))

  implicit def real2Code(real: Real): JSCode =
    "%.5g".format(real)

  implicit def int2Code(i: Int): JSCode =
    i.toString

  implicit def bool2Code(b: Boolean): JSCode =
    b.toString

  def code(xs: JSCode*): JSCode = JSCode(b => xs.foreach(_.build(b)))

  def mkCode(
      xs: Seq[JSCode],
      sep: JSCode,
      start: JSCode = "",
      end: JSCode = "",
  ): JSCode =
    JSCode { b =>
      start.build(b)
      xs.headOption.foreach { y =>
        y.build(b)
      }
      if (xs.nonEmpty)
        xs.tail.foreach { y =>
          sep.build(b)
          y.build(b)
        }
      end.build(b)
    }

  val headerStylesheet: String =
    """
      |body{
      |  padding-top: 60px;
      |}
      |.fixed-header {
      |  width: 100%;
      |  position: fixed;
      |  background: #333;
      |  padding: 10px 0;
      |  color: #fff;
      |  top: 0;
      |  padding-left: 10px;
      |  padding-right: 10px;
      |}
      |""".stripMargin

  /** creates a JS string */
  def string(s: String): JSCode = code("\"", s, "\"")

  def obj(kvs: (String, JSCode)*): JSCode =
    mkCode(kvs.map { case (k, v) => code(k, ":", v) }, ",", "{", "}")

  implicit def tuple2Code[A, B](
      x: (A, B),
  )(implicit ac: A => JSCode, bc: B => JSCode): JSCode =
    code("[", x._1, ",", x._2, "]")

  implicit def tuple3Code[A, B, C](
      x: (A, B, C),
  )(implicit ac: A => JSCode, bc: B => JSCode, cc: C => JSCode): JSCode =
    code("[", x._1, ",", x._2, ",", x._3, "]")

  implicit def vec2Code[A](xs: Seq[A])(implicit ac: A => JSCode): JSCode =
    mkCode(xs.map(ac), ",", "[", "]")

  implicit def option2Code[A](x: Option[A])(implicit ac: A => JSCode): JSCode =
    x match {
      case None    => "null"
      case Some(a) => ac(a)
    }
}
