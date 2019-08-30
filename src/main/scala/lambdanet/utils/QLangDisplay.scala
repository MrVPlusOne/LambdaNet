package lambdanet.utils

import ammonite.ops._
import funcdiff.SimpleMath
import lambdanet._
import lambdanet.train.TopNDistribution
import lambdanet.translation.ImportsResolution.NameDef
import lambdanet.translation.PAnnot
import lambdanet.translation.PredicateGraph.{PAny, PNode, PTyVar, PType}
import lambdanet.translation.QLang._

import scala.language.implicitConversions

object QLangDisplay {
  import scalatags.Text.all._

  private object Impl {

    lazy val unknownType = PTyVar(NameDef.unknownDef.ty.get)

    type Output = Modifier

    def correct(str: Output): Output =
      span(color := "green")(str)

    def incorrect(str: Output): Output =
      span(color := "red")(str)

    def warning(str: Output): Output =
      span(color := "magenta")(str)

    def key(str: Output): Output = b(str)

    implicit def qExpr2Output(e: QExpr): Output = e.toString

    implicit def qExpr2Output(e: PNode): Output = e.toString

    def code(elements: Output*): Output = {
      span(elements)
    }

    def mkSpan(
        elements: Vector[Output],
        start: Output,
        sep: Output,
        end: Output
    ): Output = {
      val middle = elements
        .zip(Vector.fill(elements.length)(sep))
        .flatMap { case (a, b) => Vector(a, b) }
        .dropRight(1)
      span(start +: middle :+ end)
    }

    def show(
        stmt: QStmt,
        truth: Map[PNode, PAnnot],
        topNPrediction: Map[PNode, TopNDistribution[PType]],
        predSpace: Set[PType],
        indentSpaces: Int = 2
    ): Output = {
      import lambdanet.translation.makeSureInBlockQ

      val prediction: Map[PNode, PType] =
        topNPrediction.mapValuesNow(_.topValue)

      def showAnnot(x: PNode): Output = {
        truth(x) match {
          case Annot.User(t, _) =>
            val annot = prediction.get(x) match {
              case None => warning(s": [miss]$t")
              case Some(p) =>
                val annot =
                  if (t == p) correct(": " + t)
                  else if (!predSpace.contains(t)) {
                    if (p == unknownType) correct(s": [UNKN]$t")
                    else incorrect(s": [OOV]$t")
                  } else {
                    incorrect(s": ($p ≠ $t)")
                  }
                val tooltipText = {
                  topNPrediction(x).distr
                    .map {
                      case (prob, ty) =>
                        s"$ty: %.2f%%".format(prob * 100)
                    }
                    .mkString("    ")
                }
                span(attr("data-toggle") := "tooltip", title := tooltipText)(
                  annot
                )
            }

            val idToDisplay = s"annot-${x.getId}"
            span(id := idToDisplay)(annot)
          case Annot.Fixed(t) => s": [fix]$t"
          case Annot.Missing  => ""
        }
      }

      def rec(indent: Int, stmt: QStmt): Vector[(Int, Output)] = {
        stmt match {
          case VarDef(x, init, isConst) =>
            val keyword = if (isConst) "const" else "let"
            Vector(
              indent ->
                code(key(keyword), " ", x, showAnnot(x), key(" = "), init, ";")
            )
          case AssignStmt(lhs, rhs) =>
            Vector(indent -> code(lhs, key("  ⃪ "), rhs, ";"))
          case ReturnStmt(e, ret) =>
            val str = code(key("return "), ret, key(" = "), e)
            Vector(indent -> str)
          case ExprStmt(e) =>
            Vector(indent -> e.toString)
          case IfStmt(cond, e1, e2) =>
            lazy val elsePart = {
              if (e2 == BlockStmt(Vector())) Vector()
              else Vector(indent -> key("else")) ++ rec(indent, e2)
            }
            Vector(indent -> code(key("if"), " (", cond, ")")) ++
              rec(indent, e1) ++ elsePart
          case WhileStmt(cond, bd) =>
            (indent -> code(key("while"), " (", cond, ")")) +: rec(indent, bd)
          case BlockStmt(stmts) =>
            (indent -> code("{")) +: stmts.flatMap(
              s => rec(indent + 1, s)
            ) :+ (indent -> code("}"))
          case FuncDef(funcName, args, ret, bd) =>
            val argList = args
              .map { n =>
                code(n, showAnnot(n))
              }
              .pipe(xs => mkSpan(xs, "(", ",", ")"))
            Vector(
              indent -> code(
                key("function "),
                funcName,
                " ",
                argList,
                ": (",
                ret,
                showAnnot(ret),
                ")"
              )
            ) ++
              rec(indent, makeSureInBlockQ(bd))
          case ClassDef(n, superType, vars, funcDefs) =>
            val superPart =
              if (superType.nonEmpty)
                mkSpan(
                  superType.map(_.toString: Output).toVector,
                  " extends ",
                  " with ",
                  ""
                )
              else code("")
            Vector(
              indent -> code(key("class "), n, superPart, "{")
            ) ++
              vars.toList.map {
                case (_, field) =>
                  (indent + 1, code(field, showAnnot(field), ";"))
              } ++
              funcDefs.toVector.flatMap { fDef =>
                rec(indent + 1, fDef._2)
              } ++
              Vector(indent -> code("}"))
        }
      }

      rec(0, stmt)
        .map {
          case (dent, text) =>
            span(paddingLeft := s"${2 * dent}em")(
              " " * (dent * indentSpaces),
              text,
              br()
            )
        }
        .pipe(span(_))
    }

  }

  def renderProjectToDirectory(
      projectName: String,
      modules: Vector[QModule],
      predictions: Map[PNode, TopNDistribution[PType]],
      predSpace: Set[PType],
      indentSpaces: Int = 2
  )(dir: Path): Unit = {
    import QLangAccuracy.{top1Accuracy}

    val prediction: Map[PNode, PType] = predictions.mapValuesNow(_.topValue)

    val file = dir / s"$projectName.html"
    val totalMap = modules.flatMap(_.mapping).toMap
    val annots = totalMap.collect {
      case (k, Annot.User(t, _)) if prediction.contains(k) => k -> t
    }

    val numMissing = totalMap.collect {
      case (k, Annot.User(t, _)) if !prediction.contains(k) => ()
    }.size

    val libAccStr = {
      val (acc, yes, total) =
        top1Accuracy(annots.filter(_._2.madeFromLibTypes), prediction, _ => 1)
      s"%.4f=$yes/$total".format(acc)
    }
    val projAccStr = {
      val (acc, yes, total) =
        top1Accuracy(annots.filter(!_._2.madeFromLibTypes), prediction, _ => 1)
      s"%.4f=$yes/$total".format(acc)
    }

    val renderedModules = modules.par.map { m =>
      val outputs = m.stmts
        .map(Impl.show(_, m.mapping, predictions, predSpace))

      div(
        hr(),
        h2(s"Module: ${m.path}"),
        body(margin := "2rem")(
          code(outputs)
        )
      )
    }.seq

    val output = html(
      head(
        h3(s"LibAcc: $libAccStr, ProjAcc: $projAccStr, Missing: $numMissing"),
        meta(charset := "UTF-8")
      ),
      renderedModules
    ).toString()
    write.over(file, output)
  }

  type ProjectName = String
  type AnnotPlace = (PNode, PType, ProjectPath)

  def renderPredictionIndexToDir(
      rightPreds: Set[AnnotPlace],
      wrongPreds: Set[AnnotPlace],
      dir: Path,
      sourcePath: String
  ): Unit = {

    var panelId = 0
    def mkCollapse(title: Modifier, items: Seq[Modifier]) = {
      val pId = synchronized { panelId += 1; panelId }
      div(`class` := "panel-group")(
        div(`class` := "panel panel-default")(
          div(`class` := "panel-heading")(
            h4(`class` := "panel-title")(
              button(
                attr("data-toggle") := "collapse",
                href := s"#collapse$pId"
              )(
                title
              )
            )
          ),
          div(`class` := "panel-collapse collapse", id := s"collapse$pId")(
            ul(`class` := "list-group")(
              items: _*
            )
          )
        )
      )
    }

    Seq(("correct", rightPreds), ("incorrect", wrongPreds)).foreach {
      case (cat, preds) =>
        val types = preds
          .groupBy(_._2)
          .toSeq
          .map(x => x._1.toString -> x._2)
          .sortBy(_._1)
          .map {
            case (ty, rest) =>
              val list = rest.toSeq.sortBy(x => (x._3, x._1.getId)).map { x =>
                val n = x._1
                val name = x._3
                a(href := s"$sourcePath/$name.html/#annot-${n.getId}")(
                  s"$n in $name"
                )
              }
              mkCollapse(s"$ty | ${list.length}", list)
          }
        val text = html(
          head(
            link(rel := "stylesheet", href := "bootstrap/bootstrap.css"),
            script(src := "bootstrap/jQuery.js"),
            script(src := "bootstrap/bootstrap.bundle.js"),
            h3(s"$cat predictions"),
            meta(charset := "UTF-8")
          ),
          body(margin := "2rem")(
            types: _*
          )
        ).toString
        val file = dir / s"$cat.html"
        write.over(file, text)
        cp.over(pwd / "scripts" / "bootstrap", dir / "bootstrap")
    }
  }

  def main(args: Array[String]): Unit = {
    import PrepareRepos._
    import ammonite.ops._

    val libDefs =
      announced(s"loading library definitions from $libDefsFile...") {
        SimpleMath.readObjectFromFile[LibDefs](libDefsFile.toIO)
      }

    val f = pwd / RelPath("data/toy")
    val (g, qModules, _, annts) =
      prepareProject(libDefs, f, skipSet = Set())

    val groundTruth = annts.map { case (k, v) => k.n -> v }
    val prediction = groundTruth.updated(
      PNode(8, None, isType = false, fromLib = false),
      PTyVar(PNode(6, None, isType = true, fromLib = false))
    )
    val predictions = prediction.mapValuesNow(
      ty => TopNDistribution(Vector(0.9 -> ty, 0.1 -> PAny))
    )
    val outDir = pwd / "predictions"
    renderProjectToDirectory("toy", qModules, predictions, annts.values.toSet)(
      outDir
    )
  }
}
