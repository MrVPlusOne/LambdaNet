package plots

import ammonite.{ops => amm}

trait VisualizationFramework {
  type Plot

  def linePlot(
      lines: Seq[XYSeq],
      cOpts: CommonOptions = CommonOptions(),
  ): Plot

  /**
    *  Note that currently only the ScalaFX backend will display dynamic plots.
    * @param cVariables a sequence of dynamic variable declarations, each of
    *                  the form `(name, (lower bound, upper bound), init value)`
    * @param dVariables a sequence of discrete variable declarations, each of
    *                   the form `(name, (value set, init value index))`
    * @param plotF the plotting function that takes in the dynamic variable
    *              value assignments and returns a [[Plot]].
    * @return An interactive plot that allows the user to manipulate its
    *         dynamic variables.
    */
  def dynamicPlot(
      cVariables: Seq[(String, ((Real, Real), Real))],
      dVariables: Seq[(String, (Vector[String], Int))],
      plotF: (Map[String, Real], Map[String, Int]) => Plot,
  ): Plot = {
    textPlot("dynamic plot not supported in this backend.", 20)
  }

  /**
    * @param cVariables a sequence of dynamic continuous variable declarations,
    *                  each of the form
    *                  `(name, (lower bound, upper bound), init value)`
    * @param plotF the plotting function that takes in a dynamic variable
    *              value assignment and returns a [[Plot]].
    * @return An interactive plot that allows the user to manipulate its
    *         dynamic variables.
    */
  def dynamicPlot(
      cVariables: Seq[(String, ((Real, Real), Real))],
      plotF: Map[String, Real] => Plot,
  ): Plot =
    dynamicPlot(
      cVariables,
      dVariables = Seq(),
      plotF = (x, _) => plotF(x),
    )

  def textPlot(
      text: String,
      fontSize: Int,
      maxWidth: Option[Int] = None,
  ): Plot

  def row(plots: Plot*): Plot

  def column(plots: Plot*): Plot

  def namedGroup(name: String)(p: Plot): Plot

  def hideable(name: String, states: HideableStates)(p: Plot): Plot = p

  //    def grid(rows: Seq[Plot]*): Plot

  /** saves the plot to the specified file path */
  def save(plot: Plot, path: amm.Path, title: String): Unit
}
