package lambdanet.train

import lambdanet._
import lambdanet.translation.PredicateGraph._
import NewInference._
import lambdanet.PrepareRepos.ParsedRepos
import lambdanet.architecture.{NNArchitecture, SegmentedLabelEncoder}

import scala.collection.parallel.ForkJoinTaskSupport

case class DataSet(
    trainSet: Vector[Datum],
    testSet: Vector[Datum],
) {
  override def toString: String =
    s"train set size: ${trainSet.size}, " +
      s"test set size: ${testSet.size}"
}

object DataSet {
  def loadDataSet(
      taskSupport: ForkJoinTaskSupport,
      architecture: NNArchitecture,
  ): DataSet =
    announced("loadDataSet") {
      import PrepareRepos._
      import util.Random

      val repos @ ParsedRepos(libDefs, projects) =
        announced(s"read data set from file: $parsedRepoPath") {
          SM.readObjectFromFile[ParsedRepos](parsedRepoPath.toIO)
        }

      def libNodeType(n: LibNode) =
        libDefs
          .nodeMapping(n.n)
          .typeOpt
          .getOrElse(PredictionSpace.unknownType)

      val libTypesToPredict: Set[LibTypeNode] =
        selectLibTypes(repos, coverageGoal = 0.9)

      val labelEncoder = announced("create label encoder") {
        SegmentedLabelEncoder(repos, coverageGoal = 0.9, architecture)
      }

      val ALL = 100
      val projectsToUse = ALL
      val testSetRatio = 0.3

      val data = projects
        .pipe(x => new Random(1).shuffle(x))
        .take(projectsToUse)
        .map {
          case (path, g, annotations) =>
            val predictor =
              Predictor(
                g,
                libTypesToPredict,
                libNodeType,
                labelEncoder.encode,
                Some(taskSupport),
              )
            Datum(path, annotations.toMap, predictor)
              .tap(d => printResult(d.showDetail))
        }

      val libAnnots = data.map(_.libAnnots).sum
      val projAnnots = data.map(_.projAnnots).sum
      printResult(s"$libAnnots library targets, $projAnnots project targets.")

      val totalNum = data.length
      val trainSetNum = totalNum - (totalNum * testSetRatio).toInt
      DataSet(data.take(trainSetNum), data.drop(trainSetNum))
        .tap(printResult)
    }

  def selectLibTypes(
      repos: ParsedRepos,
      coverageGoal: Double,
  ): Set[LibTypeNode] = {
    import cats.implicits._
    import repos.{libDefs, graphs => projects}

    val usages: Map[PNode, Int] = projects.par
      .map {
        case (_, _, annots) =>
          annots.collect { case (_, PTyVar(v)) => v -> 1 }.toMap
      }
      .fold(Map[PNode, Int]())(_ |+| _)

    /** sort lib types by their usages */
    val typeFreqs = libDefs.nodeMapping.keys.toVector
      .collect {
        case n if n.isType =>
          (LibTypeNode(LibNode(n)), usages.getOrElse(n, 0))
      }

    val (libTypes, achieved) =
      SM.selectBasedOnFrequency(typeFreqs, coverageGoal)

    printResult(s"Coverages achieved: $achieved")
    printResult(s"Lib types selected (${libTypes.length}): $libTypes")

    libTypes.map(_._1).toSet
  }

}

case class Datum(
    projectName: ProjectPath,
    annotations: Map[ProjNode, PType],
    predictor: Predictor,
) {
  val inPSpaceRatio: Double =
    annotations
      .count(
        _._2.pipe(predictor.predictionSpace.allTypes.contains),
      )
      .toDouble / annotations.size

  val distanceToConsts: PNode => Int = {
    Analysis.analyzeGraph(predictor.graph).distanceToConstNode
  }

  def libAnnots: Int = annotations.count(_._2.madeFromLibTypes)
  def projAnnots: Int = annotations.count(!_._2.madeFromLibTypes)

  def showInline: String = {
    s"{name: $projectName, " +
      s"annotations: ${annotations.size}(L:$libAnnots/P:$projAnnots), " +
      s"predicates: ${predictor.graph.predicates.size}, " +
      s"predictionSpace: ${predictor.predictionSpace.size}, " +
      s"inPSpaceRatio: $inPSpaceRatio}"
  }

  override def toString: String = {
    showInline
  }

  def showDetail: String = {
    s"""$showInline
       |${predictor.predictionSpace}
       |""".stripMargin
  }
}
