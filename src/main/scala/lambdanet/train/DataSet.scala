package lambdanet.train

import lambdanet._
import lambdanet.translation.PredicateGraph._
import NeuralInference._
import lambdanet.PrepareRepos.ParsedRepos
import lambdanet.SequenceModel.SeqPredictor
import lambdanet.translation.ImportsResolution.NameDef
import lambdanet.translation.PredicateGraph
import lambdanet.translation.QLang.QModule
import lambdanet.utils.QLangAccuracy.FseAccuracy

import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.Random

case class DataSet(
    libDefs: LibDefs,
    libTypesToPredict: Set[LibTypeNode],
    trainSet: Vector[ProcessedProject],
    devSet: Vector[ProcessedProject],
    testSet: Vector[ProcessedProject]
) {
  override def toString: String =
    s"DataSet(train: ${trainSet.size}, dev: ${devSet.size}, test:${testSet.size})"

  def signalSizeMedian(maxLibRatio: Double): Int = {
    val uselessRandom = new Random()
    val signalSizes = trainSet.map { d =>
      uselessRandom.synchronized {
        d.downsampleLibAnnots(maxLibRatio, uselessRandom).size
      }
    }
    SM.median(signalSizes)
  }
}

object DataSet {
  def loadRepos(toyMode: Boolean): PrepareRepos.ParsedRepos = {
    import PrepareRepos._
    printResult(s"Is toy data set? : $toyMode")
    if (toyMode) {
      val base = reposDir / "small"
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
  }

  def makeDataSet(
      repos: ParsedRepos,
      taskSupport: Option[ForkJoinTaskSupport],
      useSeqModel: Boolean,
      toyMode: Boolean,
      onlyPredictLibType: Boolean,
      testSetUseInferred: Boolean = false
  ): DataSet = {
    import PrepareRepos._

    val ParsedRepos(libDefs, trainSet, devSet, testSet) = repos
    val libTypesToPredict: Set[LibTypeNode] =
      selectLibTypes(
        libDefs,
        repos.trainSet.map { _.nonInferredUserAnnots },
        coverageGoal = 0.98
      )

    val nonGenerifyIt = nonGenerify(libDefs)

    var inSpace = Counted(0, 0)
    val data: Vector[Vector[ProcessedProject]] = {
      ((trainSet ++ devSet).zip(Stream.continually(true)) ++
        testSet.zip(Stream.continually(testSetUseInferred))).toVector.par.map {
        case (p @ ParsedProject(path, qModules, irModules, g), useInferred) =>
          val (predictor, predSpace) = if (useSeqModel) {
            val pSpace = PredictionSpace(
              libTypesToPredict
                .map(_.n.n.pipe(PTyVar): PType) - NameDef.unknownType
            )
            Left(SeqPredictor(irModules, libDefs, pSpace, taskSupport)) -> pSpace
          } else {
            val predictor = Predictor(
              path,
              g,
              libTypesToPredict,
              libDefs,
              taskSupport,
              onlyPredictLibType
            )
            Right(predictor) -> predictor.predictionSpace
          }

          val annots1 =
            (if (useInferred) p.allUserAnnots else p.nonInferredUserAnnots)
              .mapValuesNow(nonGenerifyIt)
              .filter { x =>
                predSpace.allTypes.contains(x._2).tap { in =>
                  import cats.implicits._
                  inSpace.synchronized {
                    inSpace |+|= Counted(1, if (in) 1 else 0)
                  }
                }
              }

          if (annots1.isEmpty) Vector()
          else {
            val d = ProcessedProject(path, annots1, g, qModules.map { m =>
              m.copy(mapping = m.mapping.mapValuesNow(_.map(nonGenerifyIt)))
            }, predictor)
            printResult(d)
            Vector(d)
          }
      }.seq
    }

    val (n1, n2) = (trainSet.length, devSet.length)
    DataSet(
      repos.libDefs,
      libTypesToPredict,
      data.take(n1).flatten,
      data.slice(n1, n1 + n2).flatten,
      data.drop(n1 + n2).flatten
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
      SM.selectBasedOnFrequency(typeFreqs, coverageGoal, selectAtLeast = 100)

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

case class ProcessedProject(
    projectName: ProjectPath,
    nodesToPredict: Map[ProjNode, PType],
    graph: PredicateGraph,
    qModules: Vector[QModule],
    predictor: Either[SeqPredictor, Predictor]
) {
  if (nodesToPredict.isEmpty) {
    throw EmptyNodesToPredict
  }
  val (predictionSpace, predGraphOpt) = predictor match {
    case Left(p)  => (p.predSpace, None)
    case Right(p) => (p.predictionSpace, Some(p.graph))
  }
  require(nodesToPredict.forall {
    case (_, ty) => predictionSpace.allTypes.contains(ty)
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

  def showInline: String =
    s"{name: $projectName, " +
      s"annotations: ${nodesToPredict.size}(L:$libAnnots/P:$projAnnots, ratio:%.4f), "
        .format(libLabelRatio) +
      predGraphOpt
        .map(_.predicates.size.pipe { s =>
          s"predicates: $s,"
        })
        .getOrElse("") +
      s"predictionSpace: ${predictionSpace.size}}"

  override def toString: String = showInline

  def showDetail: String = {
    s"""$showInline
       |${predictionSpace}
       |""".stripMargin
  }

  val fseAcc: FseAccuracy = FseAccuracy(qModules, predictionSpace)

}
