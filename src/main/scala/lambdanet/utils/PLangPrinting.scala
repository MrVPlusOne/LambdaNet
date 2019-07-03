package lambdanet.utils

import funcdiff.SimpleMath
import lambdanet._
import lambdanet.translation.ImportsResolution.NameDef
import lambdanet.translation.PAnnot
import lambdanet.translation.PredicateGraph.{PNode, PTyVar, PType}
import lambdanet.translation.QLang._

object PLangPrinting {

  lazy val unknownType = PTyVar(NameDef.unknownDef.ty.get)

  def show(
      stmt: QStmt,
      truth: Map[PNode, PAnnot],
      prediction: Map[PNode, PType],
      predSpace: Set[PType],
      indentSpaces: Int = 2,
  ) = {
    import lambdanet.translation.makeSureInBlockQ

    def prettyPrintHelper(indent: Int, stmt: QStmt): Vector[(Int, String)] = {
      import Console.{GREEN, RESET, RED, MAGENTA}
      def correct(str: String): String = {
        s"$GREEN[✓]$str$RESET"
      }

      def incorrect(str: String): String = {
        s"$RED[𐄂]$str$RESET"
      }

      def warning(str: String): String = {
        s"$MAGENTA$str$RESET"
      }

      def showAnnot(x: PNode): String = {
        truth(x) match {
          case Annot.User(t) =>
            prediction.get(x) match {
              case None => warning(s": [miss]$t")
              case Some(p) =>
                if (t == p) correct(t.toString)
                else if (!predSpace.contains(t)) {
                  if (p == unknownType) correct(": OoS")
                  else incorrect(": Oos")
                } else {
                  incorrect(s": ($p ≠ $t)")
                }
            }
          case Annot.Fixed(t) =>
            s": [fix]$t"
          case Annot.Missing =>
            ""
        }
      }

      stmt match {
        case VarDef(x, init, isConst) =>
          val keyword = if (isConst) "const" else "let"
          Vector(
            indent -> s"$keyword $x${showAnnot(x)} := $init;",
          )
        case AssignStmt(lhs, rhs) =>
          Vector(indent -> s"$lhs = $rhs;")
        case ReturnStmt(e, ret) =>
          val str = s"return $ret = $e"
          Vector(indent -> str)
        case ExprStmt(e) =>
          Vector(indent -> e.toString)
        case IfStmt(cond, e1, e2) =>
          lazy val elsePart = {
            if (e2 == BlockStmt(Vector())) Vector()
            else Vector(indent -> "else") ++ prettyPrintHelper(indent, e2)
          }
          Vector(indent -> s"if ($cond)") ++
            prettyPrintHelper(indent, e1) ++ elsePart
        case WhileStmt(cond, body) =>
          (indent -> s"while ($cond)") +: prettyPrintHelper(indent, body)
        case BlockStmt(stmts) =>
          (indent -> "{") +: stmts.flatMap(
            s => prettyPrintHelper(indent + 1, s),
          ) :+ (indent -> "}")
        case FuncDef(funcName, args, ret, body) =>
          val argList = args
            .map { n =>
              s"$n${showAnnot(n)}"
            }
            .mkString("(", ", ", ")")
          Vector(
            indent -> s"function $funcName $argList: ($ret${showAnnot(ret)})",
          ) ++
            prettyPrintHelper(indent, makeSureInBlockQ(body))
        case ClassDef(n, superType, vars, funcDefs) =>
          val superPart = superType
            .map(t => s" extends $t")
            .getOrElse("")
          Vector(
            indent -> s"class $n$superPart {",
          ) ++
            vars.toList.map {
              case (_, field) =>
                (indent + 1, s"$field${showAnnot(field)};")
            } ++
            funcDefs.toVector.flatMap { fDef =>
              prettyPrintHelper(indent + 1, fDef._2)
            } ++
            Vector(indent -> "}")
      }
    }

    prettyPrintHelper(0, stmt)
      .map {
        case (dent, text) => " " * (dent * indentSpaces) + text
      }
      .mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    import PrepareRepos._
    import ammonite.ops._

    val libDefs =
      announced(s"loading library definitions from $libDefsFile...") {
        SimpleMath.readObjectFromFile[LibDefs](libDefsFile.toIO)
      }

    val f = pwd / RelPath("data/toy")
    val (g, qModules, annts) = prepareProject(libDefs, f, skipSet = Set())

    val groundTruth = annts.map { case (k, v) => k.n -> v }
    val prediction = groundTruth.updated(
      PNode(8, None, isType = false, fromLib = false),
      PTyVar(PNode(6, None, isType = true, fromLib = false)),
    )
    qModules.foreach { m =>
      println(s"=== ${m.path} ===")
      m.stmts.foreach { show(_, m.mapping, prediction, annts.values.toSet).tap(println) }
    }
  }
}
