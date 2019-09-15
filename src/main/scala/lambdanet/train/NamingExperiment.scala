package lambdanet.train

import lambdanet.PrepareRepos.{ParsedProject, ParsedRepos, parsedReposDir}
import lambdanet.{SM, announced}
import lambdanet.translation.PredicateGraph._

object NamingExperiment {
  def showName(nameOpt: Option[Symbol]): String = {
    nameOpt.map(_.name.toString).getOrElse("$MISSING")
  }

  sealed trait Target {
    def display: String

    override def toString: String = display
  }
  case class LibType(id: Int, nameOpt: Option[Symbol]) extends Target {
    def display: String = s"Lib($id,${showName(nameOpt)})"
  }
  case class ProjectType(nameOpt: Option[Symbol]) extends Target {
    def display: String = s"Project(${showName(nameOpt)})"
  }

  def convert(pType: PType): Target = pType match {
    case _: PFuncType   => LibType(-1, Some('function))
    case _: PObjectType => LibType(-2, Some('object))
    case PAny           => LibType(-3, Some('any))
    case PTyVar(n) if n.fromLib =>
      LibType(n.getId, n.nameOpt)
    case PTyVar(n) if n.fromProject =>
      ProjectType(n.nameOpt)
  }

  def toData(project: ParsedProject): Map[(Symbol, Target), Correct] = {
    import cats.implicits._

    val data = for {
      (n, t) <- project.allUserAnnots
      name <- n.n.nameOpt
    } yield {
      Map((name, convert(t)) -> 1)
    }
    data.toVector.combineAll
  }

  def showResult(result: Map[(Symbol, Target), LibCorrect]): String = {
    result.toVector
      .sortBy { case (k, v) => -v }
      .map { case (k, v) => s"${k._1.name},${k._2},$v" }
      .mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    import ammonite.ops._
    import cats.implicits._

    val repos = announced(s"read data set from: $parsedReposDir") {
      ParsedRepos.readFromDir(parsedReposDir)
    }
    val train = repos.trainSet.map(toData).combineAll
    val dev = repos.devSet.map(toData).combineAll
    val dir = pwd / "data" / "name-data"
    write.over(dir / "train.csv", showResult(train))
    write.over(dir / "test.csv", showResult(dev))
  }

}
