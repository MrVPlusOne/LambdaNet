package plots

import ammonite.{ops => amm}
import MathematicaUtils._

case class MathematicaBackend(
    wolframScriptPath: String = "wolframscript",
    imageDpi: Int = 256,
) extends VisualizationFramework {
  var plotCounter = 0

  type Plot = Code

  def linePlot(
      lines: Seq[XYSeq],
      cOpts: CommonOptions = CommonOptions(),
  ): Plot = {
    "ListPlot".call(
      lines,
      "AxesLabel" --> cOpts.axesNames,
      "PlotLabel" --> cOpts.plotName,
      "ImageSize" --> cOpts.plotSize,
      "PlotLegends" --> cOpts.legends,
      "PlotRange" --> "All",
      "Joined" --> true,
    )
  }

  def textPlot(
      text: String,
      fontSize: Int,
      maxWidth: Option[Int] = None,
  ): Plot =
    "Text".call("Style".call(string(text), "FontSize" --> fontSize))

  def row(plots: Code*): Code =
    "Grid".call(Seq(plots))

  def column(plots: Code*): Code =
    "Column".call(plots)

  def namedGroup(name: String)(p: Plot): Plot =
    row(textPlot(name, 20), p)

  def hideable(name: String, initVisible: Boolean = true)(p: Plot): Plot = p

  /** saves the plot to the specified file path */
  def save(plot: Code, path: Path, title: String): Unit = {
    import amm._
    val text = code(
      code("plt=", plot, ";"),
      code(
        "Export".call(
          string(path.toString()),
          "plt",
          "ImageResolution" --> imageDpi,
        ),
      ),
    ).buildString
    val sourcePath = path / up / s"plot_$plotCounter.m"
    amm.write(sourcePath, text)
    println("Running Wolfram Script...")
    amm.%%(wolframScriptPath, "-f", s"plot_$plotCounter.m")(sourcePath / up)
    println(s"""Results saved to "$path"""")
    plotCounter += 1
  }
}
