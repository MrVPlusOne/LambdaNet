package plots

import plots._  // need this line when running outside of this package
import ammonite.{ops => amm}

object Examples {
  def drawSine(vf: VisualizationFramework): vf.Plot ={
    val sine = (0 to 300).map{i =>
      val x = i * 0.01
      (x, math.sin(x))
    }
    val cosine = (0 to 300).map{i =>
      val x = i * 0.01
      (x, math.cos(x))
    }

    vf.linePlot(Seq(sine, cosine),
      CommonOptions(
        // set whichever options you want to use
        plotName = Some("Waves"),
        axesNames = (Some("x"), Some("wave")),
        legends = Some(Seq("sine", "cosine"))
      )
    )
  }

  def main(args: Array[String]): Unit = {
    val exampleDir = amm.pwd/"plots_examples"
    amm.rm(exampleDir)
    amm.mkdir(exampleDir)

    val plotly = PlotlyBackend()
    plotly.save(drawSine(plotly), exampleDir / "waves.html", "waves")

    // multiple plots can be put together
    def fourCopies(vf: VisualizationFramework)(p: vf.Plot): vf.Plot = {
      val p1 = vf.row(p,p)
      vf.column(p1,p1)
    }
    plotly.save(fourCopies(plotly)(drawSine(plotly)),
      exampleDir / "four_copies.html", "Multiple plots can be put together")

    val mam = MathematicaBackend()  // requires Mathematica installed
    mam.save(drawSine(mam), exampleDir / "waves.png", "waves")
  }


}
