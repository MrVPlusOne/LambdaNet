package lambdanet.utils

import ammonite.ops._
import funcdiff.SimpleMath
import lambdanet._
import lambdanet.translation.ImportsResolution.NameDef
import lambdanet.translation.PAnnot
import lambdanet.translation.PredicateGraph.{PNode, PTyVar, PType}
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
        end: Output,
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
        prediction: Map[PNode, PType],
        predSpace: Set[PType],
        indentSpaces: Int = 2,
    ): Output = {
      import lambdanet.translation.makeSureInBlockQ

      def showAnnot(x: PNode): Output = {
        truth(x) match {
          case Annot.User(t) =>
            prediction.get(x) match {
              case None => warning(s": [miss]$t")
              case Some(p) =>
                if (t == p) correct(": " + t)
                else if (!predSpace.contains(t)) {
                  if (p == unknownType) correct(": OoS")
                  else incorrect(": OoS")
                } else {
                  incorrect(s": ($p ≠ $t)")
                }
            }
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
                code(key(keyword), " ", x, showAnnot(x), key(" = "), init, ";"),
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
              s => rec(indent + 1, s),
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
                ")",
              ),
            ) ++
              rec(indent, makeSureInBlockQ(bd))
          case ClassDef(n, superType, vars, funcDefs) =>
            val superPart = superType
              .map(t => code(key(" extends "), t.toString))
              .getOrElse(code(""))
            Vector(
              indent -> code(key("class "), n, superPart, "{"),
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
              br(),
            )
        }
        .pipe(span(_))
    }

  }

  def renderModuleToDirectory(
      m: QModule,
      prediction: Map[PNode, PType],
      predSpace: Set[PType],
      indentSpaces: Int = 2,
  )(dir: Path): Unit = {
    import QLangAccuracy.{top1Accuracy}

    val file = dir / RelPath(m.path + ".html")
    val data = m.stmts
      .map {
        Impl.show(_, m.mapping, prediction, predSpace)
      }
    val annots = m.mapping.collect { case (k, Annot.User(t)) => k -> t }

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

    val output = html(
      head(
        h2(s"Module: ${m.path}"),
        h4(s"LibAcc: $libAccStr, ProjAcc: $projAccStr"),
        meta(charset := "UTF-8"),
      ),
      body(marginLeft := "2rem")(
        code(data),
      ),
    ).toString()
    write.over(file, output)
  }

  def main(args: Array[String]): Unit = {
    import PrepareRepos._
    import ammonite.ops._

    val libDefs =
      announced(s"loading library definitions from $libDefsFile...") {
        SimpleMath.readObjectFromFile[LibDefs](libDefsFile.toIO)
      }

    val f = pwd / RelPath("data/toy")
    val (g, qModules, annts) = prepareProject(libDefs, f, skipSet = Set(), useInferred = false)

    val groundTruth = annts.map { case (k, v) => k.n -> v }
    val prediction = groundTruth.updated(
      PNode(8, None, isType = false, fromLib = false),
      PTyVar(PNode(6, None, isType = true, fromLib = false)),
    )
    val outDir = pwd / "predictions"
    qModules.foreach { m =>
      renderModuleToDirectory(m, prediction, annts.values.toSet)(
        outDir,
      )
    }
  }
}
