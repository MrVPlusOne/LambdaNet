package plots

import ammonite.{ops => amm}

case class PlotlyBackend(vSpacing: Int = -100, hSpacing: Int = 0) extends VisualizationFramework {
  import cats.data.{Chain, RWS}
  import cats.implicits._
  import scalatags.Text.TypedTag
  import scalatags.Text.all._
  import JSUtils._
  import ammonite.ops.Path

  type Plot = E[TypedTag[String]]

  type E[A] = RWS[Unit, Chain[JSCode], Int, A]

  def row(plots: Plot*): Plot =
    plots.toList.sequence.map { div(style := "display: flex;")(_) }

  def column(plots: Plot*): Plot =
    plots.toList.sequence.map(div(style := "text-align: center;")(_))

  def textPlot(
      text: String,
      fontSize: Int,
      maxWidth: Option[Int] = None,
  ): Plot =
    RWS.pure(div(style := s"font-size: $fontSize")(text))

  def namedGroup(name: String)(p: Plot): Plot = {
    column(
      textPlot(name, fontSize = 20),
      p,
    ).map { c =>
      div(
        style := "-fx-border-width: 1px;-fx-border-color: gray;" +
          "margin: 5px; padding: 5px;",
      )(c)
    }
  }

  def linePlot(
      lines: Seq[XYSeq],
      cOpts: CommonOptions = CommonOptions(),
  ): Plot = {

    val data = lines.zip(cOpts.lineNames(lines.length)).map {
      case (l, name) =>
        val (xs, ys) = l.unzip
        obj(
          "x" -> xs,
          "y" -> ys,
          "type" -> string("scatter"),
          "name" -> string(name),
        )
    }
    val (xName, yName) = cOpts.axesNames
    val layout = obj(
      "xaxis" -> obj("title" -> xName.map(string)),
      "yaxis" -> obj("title" -> yName.map(string)),
      "title" -> cOpts.plotName.map(string),
      "font" -> obj("size" -> cOpts.fontSize),
      "paper_bgcolor" -> string(cOpts.backgroundColor),
      "plot_bgcolor" -> string(cOpts.backgroundColor),
    )

    val config = obj(
      //          "displayModeBar" -> false,
      //          "scrollZoom" -> true,
    )

    val (w, h) = cOpts.plotSize
    for {
      i <- newId
      _ <- newCode {
        "Plotly.plot".call(
          "document.getElementById".call(string(i)),
          data,
          layout,
          config,
        )
      }
    } yield div(
      id := i,
      style := s"width:${w}px;height:${h}px;" +
        s"margin-top: $vSpacing; margin-right: $hSpacing",
    )
  }

  private val newId: E[String] =
    for {
      i <- RWS.get: E[Int]
      _ <- RWS.set(i + 1): E[Unit]
    } yield s"id-$i"

  private def newCode(lines: JSCode*): E[Unit] =
    RWS.tell(Chain(lines: _*))

  def toHtmlPage(plot: Plot, title: String): TypedTag[String] = {
    val (lines, _, divs) = plot.run((), 0).value
    val jsCode = lines.map(_.buildString).mkString_(";\n")

    html(
      head(
        meta(charset := "utf-8"),
        tag("title")(title),
        script(src := "https://cdn.plot.ly/plotly-latest.min.js"),
        tag("style")(raw(JSUtils.headerStylesheet)),
      ),
      body(
        div(
          `class` := "fixed-header",
          style := "text-align: left; font-size: 20px;",
        )(title),
        divs,
        script(raw(jsCode)),
      ),
    )
  }

  def save(plot: Plot, path: Path, title: String): Unit = {
    require(
      path.ext == "html",
      "Plotly backend supports only exporting as Html",
    )
    println(s"rendering to $path...")
    amm.write(path, toHtmlPage(plot, title).toString())
  }
}
