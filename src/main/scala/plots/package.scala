import scala.collection.mutable

package object plots {

  type Real = Double
  type XYSeq = Seq[(Real, Real)]
  type Path = ammonite.ops.Path

  case class CommonOptions(
      plotSize: (Int, Int) = (600, 400),
      plotName: Option[String] = None,
      axesNames: (Option[String], Option[String]) = (None, None),
      xRange: Option[(Real, Real)] = None,
      yRange: Option[(Real, Real)] = None,
      legends: Option[Seq[String]] = None,
      fontSize: Int = 12,
      backgroundColor: String = "rgba(0,0,0,0)",
  ) {
    def lineNames(n: Int): Seq[String] =
      legends.getOrElse(
        (1 to n).map { i =>
          s"line $i"
        },
      )

    private def maxWith(lines: Seq[XYSeq])(f: ((Real, Real)) => Real): Real =
      lines.map(_.map(f).max).max

    private def minWith(lines: Seq[XYSeq])(f: ((Real, Real)) => Real): Real =
      -maxWith(lines)(p => -f(p))

    def getXRange(lines: Seq[XYSeq]): (Real, Real) = {
      xRange.getOrElse {
        (minWith(lines)(_._1), maxWith(lines)(_._1))
      }
    }

    def getYRange(lines: Seq[XYSeq]): (Real, Real) = {
      yRange.getOrElse {
        (minWith(lines)(_._2), maxWith(lines)(_._2))
      }
    }
  }

  /**
    * Used by Hideable panes to store visibility information.
    */
  class HideableStates() {
    val isVisible: mutable.HashMap[String, Boolean] = mutable.HashMap()
  }

  def mapValues[K, A, B](map: Map[K, A])(f: A => B): Map[K, B] = {
    map.map { case (k, v) => k -> f(v) }
  }

  def mapWithKey[K, A, B](map: Map[K, A])(f: (K, A) => B): Map[K, B] = {
    map.map { case (k, v) => k -> f(k, v) }
  }

  def asDouble(s: String): Option[Double] =
    try Some(s.toDouble)
    catch { case ex: NumberFormatException => None }
}
