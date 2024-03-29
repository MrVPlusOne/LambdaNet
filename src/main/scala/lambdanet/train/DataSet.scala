package lambdanet.train

import lambdanet._
import lambdanet.translation.PredicateGraph._
import NeuralInference._
import ammonite.ops.pwd
import lambdanet.PrepareRepos.{ParsedProject, ParsedRepos}
import lambdanet.translation.ImportsResolution.NameDef
import lambdanet.translation.PredicateGraph
import lambdanet.translation.QLang.QModule
import lambdanet.utils.QLangAccuracy.{FseAccuracy, top1Accuracy}

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
}

object DataSet {
  def loadRepos(
      toyMode: Boolean,
      predictAny: Boolean
  ): PrepareRepos.ParsedRepos = {
    import PrepareRepos._
    printResult(s"Is toy data set? : $toyMode")
    if (toyMode) {
      val base = pwd / "data" / "toy"
      val (libDefs, Seq(trainSet, devSet, testSet)) = parseRepos(
        Seq(base / "trainSet", base / "devSet", base / "testSet"),
        predictAny = predictAny,
        loadLibDefs = true,
        numThreads = 10,
      )
      ParsedRepos(libDefs, trainSet, devSet, testSet)
    } else {
      val pd = parsedReposDir(predictAny)
      announced(s"read data set from dir '$pd'") {
        ParsedRepos.readFromDir(pd).setPredictAny(predictAny)
      }
    }
  }

  def makeDataSet(
      allRepos: ParsedRepos,
      onlyPredictLibType: Boolean,
      predictAny: Boolean,
      testSetUseInferred: Boolean = false,
      maxProjectSize: Int = 10000,
  ): DataSet = {
    import PrepareRepos._

    def projFilter(p: ParsedProject) = {
      val size = p.pGraph.predicates.size
      (size <= maxProjectSize).tap { small =>
        if (!small)
          printInfo(s"Project ${p.path} ($size predicates) is filtered due to large size.")
      }
    }

    val repos = allRepos.copy(
      trainSet = allRepos.trainSet.filter(projFilter),
      devSet = allRepos.devSet.filter(projFilter),
      testSet = allRepos.testSet.filter(projFilter),
    )

    import repos.{libDefs, trainSet, devSet, testSet}
    val libTypesToPredict: Set[LibTypeNode] =
      selectLibTypes(
        libDefs,
        repos.trainSet.map { _.nonInferredUserAnnots },
        coverageGoal = 0.98
      )

    val nonGenerifyIt = nonGenerify(libDefs)

    val data: Vector[Vector[ProcessedProject]] = {
      ((trainSet ++ devSet).zip(Stream.continually(true)) ++
        testSet.zip(Stream.continually(testSetUseInferred))).toVector.par.map {
        case (p, useInferred) =>
          import p.{path, qModules, pGraph}
          val stats = ProjectStats.computeProjectStats(
            pGraph,
            libTypesToPredict,
            libDefs,
            onlyPredictLibType,
            predictAny
          )
          def processAnnots(annots: Map[ProjNode, PType]) =
            annots
              .mapValuesNow(nonGenerifyIt)
              .filter { case (_, ty) => stats.predictionSpace.allTypes.contains(ty) }

          val (toPredict, visible) =
            (if (useInferred) {
               (p.allUserAnnots, Map[ProjNode, PType]())
             } else {
               (p.nonInferredUserAnnots, p.allUserAnnots -- p.nonInferredUserAnnots.keySet)
             }).pipe { case (l, r) => (processAnnots(l), processAnnots(r)) }

          if (toPredict.isEmpty) Vector()
          else {
            val d = ProcessedProject(
              projectName = path,
              nodesToPredict = toPredict,
              visibleAnnotations = visible,
              graph = pGraph,
              qModules = qModules.map { m =>
                m.copy(mapping = m.mapping.mapValuesNow(_.map(nonGenerifyIt)))
              },
              stats = stats,
              libDefs = libDefs,
            )
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

    val anyNode = NameDef.anyType.node
    val usages: Map[PNode, Int] = annotations.par
      .map { p =>
        p.toVector.collect {
          case (_, PTyVar(v)) => Map(v -> 1)
          case (_, PAny)      => Map(anyNode -> 1)
        }.combineAll
      }
      .fold(Map[PNode, Int]())(_ |+| _)

    assert(libDefs.nodeMapping.contains(anyNode))

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
//    val objTypeNode = libDefs.baseCtx.internalSymbols('Object).ty.get
    def f(ty: PType): PType = ty match {
      case _: PFuncType   => PTyVar(funcTypeNode)
      case _: PObjectType => PAny // treat object literal types as any
      case _              => ty
    }
    f
  }

}

case object EmptyNodesToPredict extends Exception

case class ProjectStats(
    onlyPredictLibType: Boolean,
    predictAny: Boolean,
    projectNodes: Set[ProjNode],
    projectTypes: Set[PType],
    libraryNodes: Set[LibNode],
    predictionSpace: PredictionSpace,
    labelUsages: LabelUsages,
)

object ProjectStats {
  def computeProjectStats(
      graph: PredicateGraph,
      libTypesToPredict: Set[LibTypeNode],
      libDefs: LibDefs,
      onlyPredictLibType: Boolean,
      predictAny: Boolean,
  ): ProjectStats = {
    val projectNodes: Set[ProjNode] =
      graph.nodes.filter(_.fromProject).map(ProjNode)
    val projectObjectTypes: Set[PType] =
      if (onlyPredictLibType) Set()
      else
        // only collect named types to avoid intermediate type nodes generated by resolveType
        graph.predicates.collect {
          case DefineRel(c, _: PObject) if c.isType && c.nameOpt.nonEmpty =>
            PTyVar(c)
        }
    val projectTypes: Set[PType] = {
      graph.nodes
        .filter(n => n.fromProject && n.isType && n.nameOpt.nonEmpty)
        .map(PTyVar)
    }
    val libraryNodes: Set[LibNode] =
      graph.nodes.filter(_.fromLib).map(LibNode) ++ unknownNodes
    val predictionSpace = PredictionSpace(
      libTypesToPredict
        .map(_.n.n.pipe(PTyVar))
        ++ projectObjectTypes
        -- Set(NameDef.unknownType, NameDef.anyType)
        ++ (if (predictAny) Set(PAny) else Set()),
    )

    val labelUsages: LabelUsages = computeLabelUsages(libDefs, graph)

    ProjectStats(
      onlyPredictLibType,
      predictAny,
      projectNodes,
      projectTypes,
      libraryNodes,
      predictionSpace,
      labelUsages
    )
  }
}

case class ProjectLabelStats(nodesToPredict: Map[ProjNode, PType]) {
  lazy val libAnnots: Int = nodesToPredict.count(_._2.madeFromLibTypes)
  lazy val projAnnots: Int = nodesToPredict.count(!_._2.madeFromLibTypes)
  lazy val libLabelRatio: Double =
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
    s"{ annotations: ${nodesToPredict.size}(L:$libAnnots/P:$projAnnots, ratio: %.4f) }"
      .format(libLabelRatio)
}

/**
  * @param visibleAnnotations These are type annotations that can be used as the inputs
  *                           to the GNN. On the test set, these are all the annotations
  *                           that are inferred by the compiler.
  */
case class ProcessedProject(
    projectName: ProjectPath,
    nodesToPredict: Map[ProjNode, PType],
    visibleAnnotations: Map[ProjNode, PType],
    qModules: Vector[QModule],
    graph: PredicateGraph,
    stats: ProjectStats,
    libDefs: LibDefs,
) {
  require(visibleAnnotations.keySet.intersect(nodesToPredict.keySet).isEmpty)
  if (nodesToPredict.isEmpty) {
    throw EmptyNodesToPredict
  }

  def predictionSpace: PredictionSpace = stats.predictionSpace
  require(nodesToPredict.forall {
    case (_, ty) => predictionSpace.allTypes.contains(ty)
  })

  def mkPredictor(
      userAnnots: Map[ProjNode, PType],
      taskSupport: Option[ForkJoinTaskSupport],
      checkGraph: Boolean = true,
  ): Predictor = {
    if (checkGraph)
      require(graph.userAnnotations.isEmpty, "User annotations already exist in the graph.")
    Predictor(
      graph = graph.addUserAnnotations(userAnnots),
      stats,
      libDefs,
      taskSupport,
    )
  }

  def showInline: String = {
    val stats = ProjectLabelStats(nodesToPredict).showInline
    s"{name: $projectName, " +
      s"graph: ${graph.showSizes}, " +
      s"stats: $stats, " +
      s"predictionSpace: ${predictionSpace.size}}"
  }

  override def toString: String = showInline

  def showDetail: String = showInline

  val fseAcc: FseAccuracy = FseAccuracy(qModules, predictionSpace)

}
