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

case class DataSet(
    nodeForAny: LibTypeNode,
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

      // don't predict unknown and any
      val typesNotToPredict = Set(NameDef.unknownDef.ty.get, libDefs.nodeForAny)
      val libTypesToPredict: Set[LibTypeNode] =
        selectLibTypes(
          libDefs,
          repos.trainSet.map { _.nonInferredUserAnnots },
          coverageGoal = 0.98
        ).filterNot(n => typesNotToPredict.contains(n.n.n))

      val nonGenerifyIt = nonGenerify(libDefs)

      val data: Vector[Datum] = {
        ((trainSet ++ devSet).zip(Stream.continually(true)) ++
          testSet.zip(Stream.continually(false))).toVector.par.flatMap {
          case (p@ParsedProject(path, qModules, irModules, g), useInferred) =>
            val predictor =
              Predictor(
                path,
                g,
                libTypesToPredict,
                libDefs,
                taskSupport
              )

            val seqPredictor = SeqPredictor(
              irModules,
              libDefs,
              predictor.predictionSpace,
              taskSupport
            )

            val annots =
              if (useInferred) p.allUserAnnots else p.nonInferredUserAnnots
            val annots1 = annots.mapValuesNow(nonGenerifyIt)
            try {
              Datum(path, annots1, qModules.map { m =>
                m.copy(mapping = m.mapping.mapValuesNow(_.map(nonGenerifyIt)))
              }, predictor, seqPredictor)
                .tap(printResult)
                .pipe(Vector(_))
            } catch {
              case EmptyNodesToPredict => Vector()
            }
        }.seq
      }

      (data.map { d =>
        d.annotations.size * d.inPSpaceRatio
      }.sum / data.map(_.annotations.size.toDouble).sum)
        .tap { r =>
          printResult(s"overall InSpaceRatio = $r")
        }
      //fixme: figure out why some projects have very low inSpaceRatio

      val libAnnots = data.map(_.libAnnots).sum
      val projAnnots = data.map(_.projAnnots).sum
      printResult(
        s"Train set size: ${trainSet.size}, Dev set size: ${devSet.size}"
      )
      printResult(s"$libAnnots library targets, $projAnnots project targets.")

      val (n1, n2) = (trainSet.length, devSet.length)
      DataSet(
        LibTypeNode(LibNode(libDefs.nodeForAny)),
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
    annotations: Map[ProjNode, PType],
    qModules: Vector[QModule],
    predictor: Predictor,
    seqPredictor: SeqPredictor
) {
  val inPSpaceRatio: Double =
    annotations
      .count(
        _._2.pipe(predictor.predictionSpace.allTypes.contains)
      )
      .toDouble / annotations.size

  val distanceToConsts: PNode => Int = {
    Analysis.analyzeGraph(predictor.graph).distanceToConstNode
  }

  def libAnnots: Int = annotations.count(_._2.madeFromLibTypes)
  def projAnnots: Int = annotations.count(!_._2.madeFromLibTypes)

  def showInline: String = {
    val info = s"{name: $projectName, " +
      s"annotations: ${annotations.size}(L:$libAnnots/P:$projAnnots), " +
      s"predicates: ${predictor.graph.predicates.size}, " +
      s"predictionSpace: ${predictor.predictionSpace.size}, " +
      s"inPSpaceRatio: $inPSpaceRatio"
    val outOfSpaceTypes =
      annotations.values.toSet.diff(predictor.predictionSpace.allTypes)
    val extra = if (inPSpaceRatio < 0.5) {
      s", outOfSpace: $outOfSpaceTypes }"
    } else "}"
    info + extra
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

  /** Only predict nodes whose type is within the prediction space */
  val nodesToPredict: Vector[ProjNode] = {
    val annotsToUse = annotations.filter {
      case (_, t) => predictor.predictionSpace.allTypes.contains(t)
    }.toVector

    annotsToUse.map(_._1).tap { ns =>
      if(ns.isEmpty){
        throw EmptyNodesToPredict
      }
    }
  }
}
