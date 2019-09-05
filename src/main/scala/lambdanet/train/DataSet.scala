package lambdanet.train

import lambdanet._
import lambdanet.translation.PredicateGraph._
import NeuralInference._
import lambdanet.SequenceModel.SeqPredictor
import lambdanet.architecture.NNArchitecture
import lambdanet.translation.ImportsResolution.NameDef
import lambdanet.translation.QLang.QModule
import lambdanet.utils.QLangAccuracy.FseAccuracy

import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.Random

case class DataSet(
    trainSet: Vector[Datum],
    devSet: Vector[Datum],
    testSet: Vector[Datum]
) {
  override def toString: String =
    s"train set size: ${trainSet.size}, " +
      s"test set size: ${testSet.size}"
}

object DataSet {
  def loadDataSet(
      taskSupport: Option[ForkJoinTaskSupport],
      architecture: NNArchitecture,
      toyMod: Boolean
  ): DataSet =
//    announced("loadDataSet") {
    {
      import PrepareRepos._
      import ammonite.ops._

      printResult(s"Is toy data set? : $toyMod")
      val repos @ ParsedRepos(libDefs, trainSet, devSet, testSet) =
        if (toyMod) {
          val base = pwd / up / "lambda-repos" / "small"
          val (libDefs, Seq(trainSet, devSet, testSet)) = parseRepos(
            Seq(base / "trainSet", base / "devSet", base / "testSet"),
            loadFromFile = true,
            inParallel = true
          )
          ParsedRepos(libDefs, trainSet, devSet, testSet)
        } else {
          announced(s"read data set from dir '$parsedReposDir'") {
            ParsedRepos.readFromDir(parsedReposDir)
          }
        }

      val libTypesToPredict: Set[LibTypeNode] =
        selectLibTypes(
          libDefs,
          repos.trainSet.map { _.nonInferredUserAnnots },
          coverageGoal = 0.98
        )

      val nonGenerifyIt = nonGenerify(libDefs)

      var inSpace = Counted(0, 0)
      val data: Vector[Datum] = {
        ((trainSet ++ devSet).zip(Stream.continually(true)) ++
          testSet.zip(Stream.continually(false))).toVector.par.flatMap {
          case (p @ ParsedProject(path, qModules, irModules, g), useInferred) =>
            val predictor =
              Predictor(
                path,
                g,
                libTypesToPredict,
                libDefs,
                taskSupport
              )

            val annots =
              if (useInferred) p.allUserAnnots else p.nonInferredUserAnnots
            val annots1 = annots
              .mapValuesNow(nonGenerifyIt)
              .filter { x =>
                predictor.predictionSpace.allTypes.contains(x._2).tap { in =>
                  import cats.implicits._
                  inSpace |+|= Counted(1, if (in) 1 else 0)
                }
              }
            try {
              Datum(path, annots1, qModules.map { m =>
                m.copy(mapping = m.mapping.mapValuesNow(_.map(nonGenerifyIt)))
              }, predictor)
                .tap(printResult)
                .pipe(Vector(_))
            } catch {
              case EmptyNodesToPredict => Vector()
            }
        }.seq
      }

      toAccuracy(inSpace).tap { r =>
        printResult(s"overall InSpaceRatio = $r")
      }

      val libAnnots = data.map(_.libAnnots).sum
      val projAnnots = data.map(_.projAnnots).sum
      printResult(
        s"Train set size: ${trainSet.size}, Dev set size: ${devSet.size}"
      )
      printResult(s"$libAnnots library targets, $projAnnots project targets.")

      val (n1, n2) = (trainSet.length, devSet.length)
      DataSet(
        data.take(n1),
        data.slice(n1, n1 + n2),
        data.drop(n1 + n2)
      ).tap(printResult)
    }

  def selectLibTypes(
      libDefs: LibDefs,
      annotations: Seq[Map[ProjNode, PType]],
      coverageGoal: Double
  ): Set[LibTypeNode] = {
    import cats.implicits._

    val usages: Map[PNode, Int] = annotations.par
      .map { p =>
        p.toVector.collect { case (_, PTyVar(v)) => Map(v -> 1) }.combineAll
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

  def nonGenerify(libDefs: LibDefs): PType => PType = {
    val funcTypeNode = libDefs.baseCtx.internalSymbols('Function).ty.get
    val objTypeNode = libDefs.baseCtx.internalSymbols('Object).ty.get
    def f(ty: PType): PType = ty match {
      case _: PFuncType   => PTyVar(funcTypeNode)
      case _: PObjectType => PTyVar(objTypeNode)
      case _              => ty
    }
    f
  }

}

case object EmptyNodesToPredict extends Exception

case class Datum(
    projectName: ProjectPath,
    nodesToPredict: Map[ProjNode, PType],
    qModules: Vector[QModule],
    predictor: Predictor
) {
  if (nodesToPredict.isEmpty) {
    throw EmptyNodesToPredict
  }
  require(nodesToPredict.forall {
    case (_, ty) => predictor.predictionSpace.allTypes.contains(ty)
  })

  val libAnnots: Int = nodesToPredict.count(_._2.madeFromLibTypes)
  val projAnnots: Int = nodesToPredict.count(!_._2.madeFromLibTypes)
  val libLabelRatio: Double =
    if (projAnnots == 0) 1.0 else libAnnots.toDouble / projAnnots

  def downsampleLibAnnots(
      maxLibRatio: Double,
      random: Random
  ): Map[ProjNode, PType] = {
    if (libLabelRatio <= maxLibRatio)
      return nodesToPredict
    val maxLibLabels = (projAnnots * maxLibRatio).toInt
    val libLabels = nodesToPredict.toVector
      .filter(_._2.madeFromLibTypes)
      .pipe(random.shuffle(_))
      .take(maxLibLabels)
    nodesToPredict.filter(!_._2.madeFromLibTypes) ++ libLabels
  }

  def showInline: String = {
    val info = s"{name: $projectName, " +
      s"annotations: ${nodesToPredict.size}(L:$libAnnots/P:$projAnnots, ratio:%.4f), "
        .format(libLabelRatio) +
      s"predicates: ${predictor.graph.predicates.size}, " +
      s"predictionSpace: ${predictor.predictionSpace.size}, "
    info
  }

  override def toString: String = {
    showInline
  }

  def showDetail: String = {
    s"""$showInline
       |${predictor.predictionSpace}
       |""".stripMargin
  }

  val fseAcc: FseAccuracy = FseAccuracy(qModules, predictor.predictionSpace)

}
