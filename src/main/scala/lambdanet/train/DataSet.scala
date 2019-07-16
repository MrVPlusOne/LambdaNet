package lambdanet.train

import lambdanet._
import lambdanet.translation.PredicateGraph._
import NewInference._
import lambdanet.PrepareRepos.ParsedRepos
import lambdanet.architecture.{
  NNArchitecture,
  RandomLabelEncoder,
  SegmentedLabelEncoder,
}
import lambdanet.translation.QLang.QModule
import lambdanet.utils.QLangAccuracy.FseAccuracy

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
      taskSupport: Option[ForkJoinTaskSupport],
      architecture: NNArchitecture,
  ): DataSet =
    announced("loadDataSet") {
      import PrepareRepos._
      import util.Random
      import ammonite.ops._
      import TrainingLoop.toyMod

      printResult(s"Is toy data set? : $toyMod")

      val repos@ParsedRepos(libDefs, trainSet, devSet) =
        if (toyMod) parseRepos(pwd / RelPath("data/toy"), pwd / RelPath("data/toy"))
        else
          announced(s"read data set from: $parsedRepoPath") {
            SM.readObjectFromFile[ParsedRepos](parsedRepoPath.toIO)
          }


      def libNodeType(n: LibNode) =
        libDefs
          .nodeMapping(n.n)
          .typeOpt
          .getOrElse(PredictionSpace.unknownType)

      val libTypesToPredict: Set[LibTypeNode] =
        selectLibTypes(repos, coverageGoal = 0.95) +
          LibTypeNode(LibNode(PredictionSpace.unknownTypeNode))

      val labelEncoder = announced("create label encoder") {
        RandomLabelEncoder(architecture)
//        FiniteRandomLabelEncoder(200, architecture, new Random(1))
      }
      val nameEncoder = announced("create name encoder") {
        SegmentedLabelEncoder(repos, coverageGoal = 0.90, architecture)
      }

      printResult(s"Label encoder: ${labelEncoder.name}")

      val data = (trainSet++devSet).toVector
        .map {
          case ParsedProject(path, g, qModules, annotations) =>
            val predictor =
              Predictor(
                LibTypeNode(LibNode(libDefs.nodeForAny)),
                g,
                libTypesToPredict,
                libNodeType,
                labelEncoder.encode,
                nameEncoder.encode,
                taskSupport,
              )
            Datum(path, annotations, qModules, predictor)
              .tap(printResult)
        }

      val libAnnots = data.map(_.libAnnots).sum
      val projAnnots = data.map(_.projAnnots).sum
      printResult(s"Train set size: ${trainSet.size}, Dev set size: ${devSet.size}")
      printResult(s"$libAnnots library targets, $projAnnots project targets.")

      DataSet(data.take(trainSet.length), data.drop(trainSet.length))
        .tap(printResult)
    }

  def selectLibTypes(
      repos: ParsedRepos,
      coverageGoal: Double,
  ): Set[LibTypeNode] = {
    import cats.implicits._
    import repos.{libDefs, trainSet}

    val usages: Map[PNode, Int] = trainSet.par
      .map { p =>
        p.userAnnots.collect { case (_, PTyVar(v)) => v -> 1 }
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

    printResult(s"Lib types coverage achieved: $achieved")
    printResult(s"Lib types selected (${libTypes.length}): $libTypes")

    libTypes.map(_._1).toSet
  }

}

case class Datum(
    projectName: ProjectPath,
    annotations: Map[ProjNode, PType],
    qModules: Vector[QModule],
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

  val fseAcc: FseAccuracy = FseAccuracy(qModules)
}
