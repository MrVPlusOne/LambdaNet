package plots

import ammonite.{ops => amm}
import lambdanet.ChainingSyntax
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property.{DoubleProperty, IntegerProperty}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text.Font

class ScalaFXBackend(
    val rootPane: ScrollPane,
    settings: ScalaFXBackend.Settings,
) extends VisualizationFramework {
  import settings._

  type Plot = Region

  def linePlot(lines: Seq[XYSeq], cOpts: CommonOptions): Plot = {
    val names = cOpts.lineNames(lines.length)
    val series = lines.zip(names).map {
      case (data, nm) =>
        val xyData = data.map {
          case (x, y) => XYChart.Data[Number, Number](x, y)
        }
        new XYChart.Series[Number, Number] {
          name = nm
          data() ++= xyData
        }
    }
    val (xName, yName) = cOpts.axesNames

    val xRange = cOpts.xRange
    val yRange = cOpts.yRange

    def makeAxis(isX: Boolean): NumberAxis = {
      val name = if (isX) xName else yName
      val range = if (isX) xRange else yRange
      new NumberAxis {
        forceZeroInRange = false
        autoRanging = range.isEmpty
        range.foreach { r =>
          lowerBound = r._1
          upperBound = r._2
          tickUnit = (r._2 - r._1) / 10
        }
        name.foreach(label = _)
      }
    }

    new LineChart(makeAxis(isX = true), makeAxis(isX = false)) {
      cache = true
      animated = false
      createSymbols = false
      cOpts.plotName.foreach(title = _)

      series.foreach(data() += _)
    }
  }

  override def dynamicPlot(
      cVariables: Seq[(String, ((Real, Real), Real))],
      dVariables: Seq[(String, (Vector[String], Int))],
      plotF: (Map[String, Real], Map[String, Int]) => Plot,
  ): Plot = {
    val cValueMap = cVariables.map {
      case (name, (_, init)) =>
        name -> DoubleProperty(init)
    }.toMap

    val dValueMap = dVariables.map {
      case (name, (_, init)) =>
        name -> IntegerProperty(init)
    }.toMap

    val cPanel: Seq[Seq[Plot]] = cVariables.map {
      case (name, ((min, max), init)) =>
        val slider = new Slider(min, max, init) {
          prefWidth = 300
        }
        val value = cValueMap(name)
        slider.value <==> value
        val input = new TextField() {
          prefWidth = 60
        }
        input.onAction = _ => {
          asDouble(input.text.value) match {
            case Some(d) if min <= d && d <= max => value() = d
            case None                            => input.text() = value().toString
          }
        }
        input.text() = value().toString
        value.onChange((_, _, v) => input.text() = v.toString)
        Seq(textPlot(name, fontSize = 20), slider, input)
    }
    val dPanel: Seq[Seq[Plot]] = dVariables.map {
      case (name, (labels, init)) =>
        val group = new ToggleGroup()
        val w = 240 / labels.length
        val value = dValueMap(name)
        val buttons = labels.zipWithIndex.map {
          case (l, i) =>
            new RadioButton {
              text = l
              toggleGroup = group
              maxWidth = w
              tooltip = l

              onAction = (_) => value() = i
            }
        }
        buttons(init).selected = true
        Seq(textPlot(name, fontSize = 20), row(buttons: _*))
    }
    val controlPanel = grid(
      cPanel ++ dPanel,
    )
    controlPanel.styleClass += "dynamicPlot-controlPanel"

    val contentPane = new Pane()
    (cValueMap ++ dValueMap).foreach {
      case (_, v) =>
        v.onChange { (_, _, _) =>
          contentPane.children =
            plotF(mapValues(cValueMap)(_.value), mapValues(dValueMap)(_.value))
        }
    }
    contentPane.children =
      plotF(mapValues(cValueMap)(_.value), mapValues(dValueMap)(_.value))

    column(
      controlPanel,
      contentPane,
    ).tap {
      _.styleClass += "dynamicPlot"
    }
  }

  def textPlot(
      text: String,
      fontSize: Int,
      maxWidth: Option[Int] = None,
  ): Plot = {
    new Label(text) {}.tap { l =>
      l.font = new Font(fontName, fontSize)
      l.tooltip = text
      maxWidth.foreach(l.maxWidth = _)
    }
  }

  def namedGroup(name: String)(p: Plot): Plot = {
    new VBox() {
      spacing = colSpacing
      alignment = Pos.Center
      margin = Insets(groupMargin)
      padding = Insets(groupPadding)
      children = Seq(
        new Separator(),
        textPlot(name, 20),
        p,
      )
      styleClass += "namedGroup"
    }
  }

  def row(plots: Plot*): HBox =
    new HBox() {
      spacing = rowSpacing
      alignment = Pos.Center
      margin = Insets(groupMargin)
      padding = Insets(groupPadding)
      children = plots
    }

  def column(plots: Plot*): VBox =
    new VBox() {
      spacing = colSpacing
      alignment = Pos.TopLeft
      margin = Insets(groupMargin)
      padding = Insets(groupPadding)
      children = plots
    }

  def grid(rows: Seq[Seq[Plot]]): Plot = {
    val g = new GridPane() {
      hgap = rowSpacing
      vgap = colSpacing
      margin = Insets(groupMargin)
      padding = Insets(groupPadding)
    }
    for {
      (row, r) <- rows.zipWithIndex
      (content, c) <- row.zipWithIndex
    } {
      GridPane.setConstraints(content, c, r)
      g.children += content
    }
    g
  }

  override def hideable(name: String, states: HideableStates)(p: Plot): Plot = {
    val box = new CheckBox(name)
    val result = column(
      box,
    )
    box.selected.onChange((_, _, _) => {
      val on = box.selected()
      if (on && result.children.length == 1)
        result.children.append(p)
      else if (!on && result.children.length > 1) {
        result.children.trimEnd(1)
      }
      states.isVisible(name) = on
    })
    box.selected() = states.isVisible.getOrElseUpdate(name, true)
    result
  }

  /** saves the plot to the specified file path */
  def save(plot: Plot, path: amm.Path, title: String): Unit = {
    val (mW, mH) = maxWindowSize
    rootPane.content = plot
  }
}

object ScalaFXBackend {
  case class Settings(
      fontName: String = "Verdana",
      rowSpacing: Double = 5,
      colSpacing: Double = 5,
      groupPadding: Double = 5,
      groupMargin: Double = 2,
      maxWindowSize: (Double, Double) = (1000, 800),
  )

  type ProgressReport = Double => Unit

}

abstract class ScalaFxBackendApp extends JFXApp {
  def appName: String
  def windowSize: (Double, Double)

  /** Override to change default settings */
  def settings: ScalaFXBackend.Settings = ScalaFXBackend.Settings()
  def task(vf: ScalaFXBackend): Unit

  val sp = new ScrollPane()
  stage = new JFXApp.PrimaryStage {
    title = appName
    scene = new Scene(windowSize._1, windowSize._2) {
      root = sp
      stylesheets.add("scalafx-style.css")
    }
  }

  task(new ScalaFXBackend(sp, settings))
}
