package lambdanet

import ammonite.ops._
import funcdiff.SimpleMath
import lambdanet.Surface.GModule
import lambdanet.translation.IR.IRModule
import lambdanet.translation.ImportsResolution.{ErrorHandler, ModuleExports, NameDef}
import lambdanet.translation.PredicateGraph.{DefineRel, LibNode, PNode, PNodeAllocator, PObject, PType, ProjNode, TyPredicate}
import lambdanet.translation.QLang.QModule
import lambdanet.translation._
import lambdanet.utils.ProgramParsing.GProject
import lambdanet.utils.{DownloadRepos, ProgramParsing}

import java.util.concurrent.ForkJoinPool
import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.Random

@SerialVersionUID(2)
case class LibDefs(
    baseCtx: ModuleExports,
    nodeMapping: Map[PNode, PAnnot],
    libExports: Map[ProjectPath, ModuleExports],
    classes: Set[QLang.ClassDef]
) {

  /**
    * Returns the known type for each library node
    */
  def libNodeType(n: LibNode): PType =
    nodeMapping(n.n).typeOpt
      .getOrElse(PredictionSpace.unknownType)

  lazy val libClassDefs: Set[DefineRel] = classes.map {
    case QLang.ClassDef(classNode, _, vars, funcDefs) =>
      val fields = vars ++ funcDefs.mapValuesNow(_.funcNode)
      DefineRel(classNode, PObject(fields))
  }
}

object PrepareRepos {
  val reposDir: Path = pwd / up / "lambda-repos"
  val libDefsFile: Path = pwd / "models" / "libDefs.serialized"
  def parsedReposDir(predictAny: Boolean): Path =
    if (predictAny) pwd / 'data / "parsedRepos-with_any"
    else pwd / 'data / "parsedRepos-no_any"

  val allReposDir: Path = reposDir / "allRepos"

  /** use this function to download, parse, filter, divide, and serialize the
  / training, dev, and test sets. */
  def main(args: Array[String]): Unit = {
    // step1: download all repos under `reposDir/allRepos`
//    DownloadRepos.downloadAllRepos(PrepareRepos.reposDir)

    // step2: create libDefs
//    val defs = parseLibDefs()
//    SimpleMath.saveObjectToFile(libDefsFile.toIO)(defs)
//    println(s"library definitions saved to $libDefsFile")

    // step3: parse then randomly divide date set into train/dev/test
    // might need to exclude the repo named "OmniSharp_omnisharp-atom" since it somehow
    // causes out-of-memory issue
    val predictAny = false
    parseAndFilterDataSet(predictAny, loadLibDefs = true)
    divideDataSet()
    //    remixDividedDataSet()

    // step4: parse filtered repos again and serialize them into `parsedReposDir` for fast future loading.
    parseAndSerializeDataSet(predictAny, loadLibDefs = true)
  }

  private def remixDividedDataSet(): Unit = {
    val base = reposDir / "divided"
    for {
      dataSet <- Seq("trainSet", "testSet", "devSet")
      p <- ls(base / dataSet) if p.isDir && p.last != "toy"
    } {
      mv(p, allReposDir / p.last)
    }
  }

  sealed trait RepoResult {
    def toVector: Vector[ParsedProject] = this match {
      case Successful(p) => Vector(p)
      case _             => Vector()
    }
  }
  case object TooBigOrSmall extends RepoResult
  case class HasError(ex: Throwable) extends RepoResult
  case class Successful(result: ParsedProject) extends RepoResult

  def parseRepos(
      dataSetDirs: Seq[Path],
      predictAny: Boolean,
      loadLibDefs: Boolean = true,
      numThreads: Int = 8,
      maxLinesOfCode: Int = Int.MaxValue,
      parsedCallback: (Path, RepoResult) => Unit = (_, _) => ()
  ): (LibDefs, Seq[List[ParsedProject]]) = {
    lambdanet.shouldWarn = false

    val libDefs = if (loadLibDefs) {
      announced(s"loading library definitions from $libDefsFile...") {
        SimpleMath.readObjectFromFile[LibDefs](libDefsFile.toIO)
      }
    } else {
      val defs = parseLibDefs()
      SimpleMath.saveObjectToFile(libDefsFile.toIO)(defs)
      println(s"library definitions saved to $libDefsFile")
      defs
    }

    def fromDir(dir: Path) =
      (ls ! dir)
        .filter(f => f.isDir)
        .pipe(
          x =>
            if (numThreads == 1) x
            else
              x.par.tap {
                _.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(numThreads))
              }
        )
        .flatMap { f =>
          val r = if (countTsCode(f, dir) < maxLinesOfCode) {
            try {
              val p0 = parseProject(
                libDefs,
                dir,
                f,
                shouldPruneGraph = false,
                errorHandler = ErrorHandler.alwaysStoreError,
                warnOnErrors = true,
                predictAny = predictAny,
              ).mergeEqualities

              Successful(p0).tap { _ =>
                val diff =
                  (p0.pGraph.nodes ++ p0.pGraph.predicates.flatMap(_.allNodes))
                    .diff(libDefs.nodeMapping.keySet)
                val libNode = diff.find(_.fromLib)
                assert(
                  libNode.isEmpty,
                  s"lib node not in libDefs: ${libNode.get}"
                )
              }
            } catch {
              case ex: Exception => HasError(ex)
              case err: Error    => HasError(err)
            }
          } else TooBigOrSmall
          parsedCallback(f, r)
          r.toVector
        }
        .toList

    libDefs -> dataSetDirs.map { fromDir }
  }

  def countTsCode(dir: Path, workingDir: Path): Int = {
    val map = %%(
      'cloc,
      "--csv",
      dir
    )(workingDir).out.lines.collect {
      case l if l.headOption.forall(_.isDigit) && l.split(",").length == 5 =>
        val cols = l.split(",")
        cols(1) -> cols.last.toInt
    }.toMap
    map.getOrElse("TypeScript", 0)
  }

  def parseAndFilterDataSet(predictAny: Boolean, loadLibDefs: Boolean): Unit = {
    def projectDestination(p: ParsedProject): String = {
      val nodes = p.pGraph.nodes.size
      if (nodes < 500 || nodes > 20000)
        return "TooBigOrSmall"
      val annots = p.allUserAnnots
      val annotsRatio = annots.size.toDouble / p.pGraph.predicates.size
      printResult(s"annots ratio: $annotsRatio")
      if (annotsRatio < 1.0 / 10) {
        return "TooFewLabels"
      }

      val libCount = annots.count(_._2.madeFromLibTypes)
      val projCount = annots.size - libCount
      if (projCount * 10 < libCount)
        return "TooFewProjectLabels"

      "filteredRepos"
    }

    var progress = 0
    announced("parsePredGraphs")(
      parseRepos(
        Seq(allReposDir),
        loadLibDefs = loadLibDefs,
        numThreads = 10,
        predictAny = predictAny,
        maxLinesOfCode = 30000,
        parsedCallback = (file, pOpt) => {
          val dest = pOpt match {
            case Successful(p) =>
              projectDestination(p)
            case TooBigOrSmall =>
              "TooBigOrSmall"
            case HasError(ex) =>
              printWarning(
                s"$file contains error: ${ex.getMessage}",
                mustWarn = true
              )
              "HasError"
          }
          val dir = allReposDir / up / dest
          if (!exists(dir))
            mkdir(dir)
          mv(file, dir / file.last)
          this synchronized {
            progress += 1
            printResult(s"progress: $progress")
          }
        }
      )
    )
  }

  def divideDataSet(): Unit = {
    val random = new Random(1)
    val base = reposDir
    val allProjects = ls(base / "filteredRepos")
      .filter(f => f.isDir && f.last != "toy")
      .pipe(random.shuffle(_))

    def tryMove(from: Path, to: Path): Unit = {
      if (!exists(to / up)) mkdir(to / up)
      if (to != from) mv(from, to)
    }

    val targetDir = base / "divided"

    allProjects.take(60).foreach { f =>
      tryMove(f, targetDir / "testSet" / f.last)
    }
    allProjects.slice(60, 100).foreach { f =>
      tryMove(f, targetDir / "devSet" / f.last)
    }
    allProjects.drop(100).foreach { f =>
      tryMove(f, targetDir / "trainSet" / f.last)
    }
  }

  private def testNewSerialization(predictAny: Boolean): Unit = {
    val pd = parsedReposDir(predictAny)
    val repos = announced("read1") {
      SM.readObjectFromFile[ParsedRepos](
        (pd / up / "parsedDataSet.serialized").toIO
      )
    }

    announced("write") {
      repos.serializeIntoDir(pd)
    }

    announced("read2") {
      ParsedRepos.readFromDir(pd)
    }

  }

  def parseAndSerializeDataSet(
      predictAny: Boolean,
      loadLibDefs: Boolean
  ): Unit = {
    val basePath = reposDir / "divided"
    val trainSetDir: Path = basePath / "trainSet"
    val devSetDir: Path = basePath / "devSet"
    val testSetDir: Path = basePath / "testSet"
    val pd = parsedReposDir(predictAny)

    val (libDefs, Seq(trainSet, devSet, testSet)) =
      announced("parsePredGraphs") {
        var progress = 0
        parseRepos(
          Seq(trainSetDir, devSetDir, testSetDir),
          loadLibDefs = loadLibDefs,
          numThreads = 10,
          parsedCallback = (_, _) =>
            synchronized {
              progress += 1
              printResult(s"Progress: $progress")
            },
          predictAny = predictAny,
        )
      }
    for ((setName, dataset) <- Seq("train" -> trainSet, "dev" -> devSet, "test" -> testSet)) {
      val stats = repoStatistics(dataset)
      val avgStats = stats.headers
        .zip(stats.average)
        .map {
          case (h, n) => "%s: %.1f".format(h, n)
        }
        .mkString(", ")
      printResult(avgStats)
      write.over(pd / up / s"stats-$setName.txt", avgStats)
    }

    announced(s"save data set to dir: $pd") {
      ParsedRepos(libDefs, trainSet, devSet, testSet)
        .serializeIntoDir(pd)
    }
  }

  /**
    * @param predictAny when set to true, will include any into the prediction
    *                   space. However, even if it's set to true, only user-annotated
    *                   `any`s will be used as training signal since TS Compiler
    *                   can infer a lot of spurious `any`s.
    * @param srcTexts maps source file path to the corresponding code text. Combined with
    *                 the `srcSpan` field in `PNode`, this can be used to map the predictions
    *                 back into source code positions.
    */
  @SerialVersionUID(2)
  case class ParsedProject(
      path: ProjectPath,
      srcTexts: Map[ProjectPath, String],
      qModules: Vector[QModule],
      irModules: Vector[IRModule],
      pGraph: PredicateGraph,
      predictAny: Boolean,
  ) {
    import NameDef.{isAny, unknownType}

    def allAnnots: Map[PNode, PAnnot] = irModules.flatMap(_.mapping).toMap

    private def shouldExclude(a: Annot.User[PType]): Boolean = {
      a.ty == unknownType || (isAny(a.ty) && (!predictAny || a.inferred))
    }

    @transient
    lazy val rawAllUserAnnots: Map[PNode, PAnnot] =
      allAnnots.collect {
        case (n, ant @ Annot.User(_, _)) if !shouldExclude(ant) =>
          n -> ant
      }

    @transient
    lazy val allUserAnnots: Map[ProjNode, PType] = {
      allAnnots.collect {
        case (n, ant @ Annot.User(t, _)) if !shouldExclude(ant) =>
          ProjNode(n) -> t
      }
    }

    @transient
    lazy val nonInferredUserAnnots: Map[ProjNode, PType] = {
      allAnnots.collect {
        case (n, ant @ Annot.User(t, false)) if !shouldExclude(ant) =>
          ProjNode(n) -> t
      }
    }

    def mergeEqualities: ParsedProject = {
      val (graph1, merger) = pGraph.mergeEqualities
      val qModules1 = qModules.map { _.mapNodes(merger) }
      val irModules1 = irModules.map { _.mapNodes(merger) }
      copy(qModules = qModules1, irModules = irModules1, pGraph = graph1)
    }
  }

  import concurrent.ExecutionContext.Implicits.global
  import concurrent.duration._
  import concurrent.{Await, Future}

  @SerialVersionUID(2)
  case class ParsedRepos(
      libDefs: LibDefs,
      trainSet: List[ParsedProject],
      devSet: List[ParsedProject],
      testSet: List[ParsedProject]
  ) {
    def setPredictAny(predictAny: Boolean): ParsedRepos =
      copy(
        trainSet = trainSet.map(_.copy(predictAny = predictAny)),
        devSet = devSet.map(_.copy(predictAny = predictAny)),
        testSet = testSet.map(_.copy(predictAny = predictAny)),
      )

    import ParsedRepos._

    def meta(chunkNum: Int) =
      Meta(trainSet.length, devSet.length, testSet.length, chunkNum)

    def serializeIntoDir(
        dir: Path,
        timeoutSeconds: Int = 200,
        chunkSize: Int = 50
    ): Unit = {
      import cats.implicits._

      if (exists(dir)) {
        printWarning(s"Overriding results in $dir", mustWarn = true)
        rm(dir)
      }
      mkdir(dir)

      def withName(
          data: List[List[Serializable]],
          dataSetName: String
      ): List[(Serializable, String)] = {
        data.zipWithIndex.map { case (d, i) => d -> (dataSetName + i) }
      }

      val libDefsF = Future {
        SM.saveObjectToFile((dir / "libDefs").toIO)(libDefs)
      }

      val chunkSize = 30
      val chunks = withName(
        (trainSet ++ devSet ++ testSet).grouped(chunkSize).toList,
        "chunk"
      )
      val toSave = (meta(chunks.length) -> "meta") +: chunks

      val allF = (libDefsF +: toSave.toVector.map {
        case (d, name) =>
          Future { SM.saveObjectToFile((dir / name).toIO)(d) }
      }).sequence_

      Await.result(allF, timeoutSeconds.second)
    }
  }

  object ParsedRepos {

    @SerialVersionUID(1)
    case class Meta(
        trainSetSize: Int,
        devSetSize: Int,
        testSetSize: Int,
        chunkNum: Int
    ) {
      def totoalProjectNum = trainSetSize + devSetSize + testSetSize
    }

    def readFromDir(dir: Path, timeoutSeconds: Int = 400): ParsedRepos = {
      import cats.implicits._

      val meta = SM.readObjectFromFile[Meta]((dir / "meta").toIO)
      import meta._
      val libDefsF = Future {
        announced("Read libDefs") {
          SM.readObjectFromFile[LibDefs]((dir / "libDefs").toIO)
        }
      }
      Thread.sleep(100)
      val chunksF = (0 until chunkNum).toVector.map { i =>
        Future {
          announced(s"Read repo chunk $i")(
            SM.readObjectFromFile[List[ParsedProject]]((dir / s"chunk$i").toIO)
          )
        }
      }.sequence
      val resultF = for {
        libDefs <- libDefsF
        projectChunks <- chunksF.map(_.toList)
        projects = projectChunks.flatten
      } yield {
        ParsedRepos(
          libDefs,
          projects.take(trainSetSize),
          projects.slice(trainSetSize, trainSetSize + devSetSize),
          projects.drop(trainSetSize + devSetSize)
        )
      }

      Await.result(resultF, timeoutSeconds.second)
    }
  }

  case class RepoStats(
      average: Vector[Double],
      data: Map[ProjectPath, Vector[Int]]
  ) {
    val libNodes = 1
    val projNodes = 2
    val annotations = 3
    val predicates = 4

    val headers: Vector[String] =
      Vector("libNodes", "projNodes", "libAnnots", "projAnnots", "anyAnnots", "predicates")
  }

  def repoStatistics(results: Seq[ParsedProject]): RepoStats = {
    require(results.nonEmpty)

    val rows = results
      .map { p =>
        import p.{path, pGraph => graph}
        val nLib = graph.nodes.count(_.fromLib)
        val nProj = graph.nodes.count(!_.fromLib)
        val nPred = graph.predicates.size
        val libAnnots = p.allUserAnnots.count(_._2.madeFromLibTypes)
        val projAnnots = p.allUserAnnots.size - libAnnots
        val anyAnnots = p.allUserAnnots.count(x => NameDef.isAny(x._2))
        path -> Vector(nLib, nProj, libAnnots, projAnnots, anyAnnots, nPred)
      }
      .sortBy(_._2.last)

    val (paths, vec) = rows.unzip
    val average = vec
      .reduce(_.zip(_).map { case (x, y) => x + y })
      .map(_.toDouble / paths.length)

    val data = rows.toMap
    RepoStats(average, data)
  }

  def parseLibDefs(): LibDefs = {
    import cats.implicits._
    val declarationsDir = reposDir / "declarations"

    println("parsing default module...")
    val (baseCtx, libAllocator, defaultMapping, defaultModule) =
      QLangTranslation.parseDefaultModule()
    println("default module parsed")

    println("parsing library modules...")
    val GProject(_, _, modules, mapping, subProjects, devDependencies) =
      ProgramParsing
        .parseGProjectFromRoot(
          declarationsDir,
          declarationFileMode = true
        )

    println("parsing PModules...")
    val pModules =
      modules.map(m => PLangTranslation.fromGModule(m, libAllocator))

    println("imports resolution...")
    val handler =
      ErrorHandler(ErrorHandler.StoreError, ErrorHandler.StoreError)

    val resolved1 = baseCtx.publicNamespaces.map {
      case (k, m) => (k: RelPath) -> m
    }
    val exports = ImportsResolution.resolveExports(
      ImportsResolution.ProjectInfo(
        pModules,
        baseCtx,
        resolved1,
        mapping,
        defaultPublicMode = true,
        devDependencies
      ),
      errorHandler = handler,
      nameOpt => {
        libAllocator.newUnknownDef(nameOpt).tap { d =>
          printWarning(s"new unknown def: $d", mustWarn = true)
        }
      },
      maxIterations = 5
    )

    val baseCtx1 = baseCtx
    // todo: check if need to handle public modules (also the missing lodash)
//    val publicNamespaces = exports.values.flatMap {
//      _.publicNamespaces.map {
//        case (k, m) => k -> NameDef.namespaceDef(m)
//      }
//    }.toMap
//    val baseCtx1 = baseCtx |+| ModuleExports(publicSymbols = publicNamespaces)

    val namedExports = subProjects.map {
      case (name, path) =>
        name -> exports.getOrElse(
          path,
          exports.getOrElse(
            path / "index", {
              Console.err.println(
                s"Couldn't find Exports located at $path for $name, ignore this named project."
              )
              ModuleExports.empty
            }
          )
        )
    }
    handler.warnErrors()
    val libExports = exports ++ namedExports

    val qModules =
      pModules.map { m =>
        QLangTranslation.fromPModule(m, baseCtx1 |+| exports(m.path))
      }

    val anyNode = libAllocator.anyNode

    val nodeMapping = defaultMapping ++
      qModules.flatMap(_.mapping) ++
      libAllocator.namedUnknownDefs.values.map { d =>
        d.term.get -> Annot.Missing
      } +
      (anyNode -> Annot.Missing)

    import translation.QLang._
    val classes = (qModules :+ defaultModule)
      .flatMap(_.stmts)
      .collect {
        case c: ClassDef => c
      }
      .toSet

    //todo (jiayi): also collect all functions
    println("Declaration files parsed.")
    LibDefs(baseCtx1, nodeMapping, libExports, classes)
  }

  def pruneGraph(
      graph: PredicateGraph,
      needed: Set[ProjNode]
  ): PredicateGraph = {
    val predicates: Map[PNode, Set[TyPredicate]] = {
      graph.predicates
        .flatMap(p => p.allNodes.map(n => n -> p))
        .groupBy(_._1)
        .map { case (k, v) => k -> v.map(_._2) }
    }

    def neighbours(n: PNode): (Set[ProjNode], Set[TyPredicate]) = {
      val ps = predicates.getOrElse(n, Set())
      (ps.flatMap(_.allNodes) - n).filterNot(_.fromLib).map(ProjNode) -> ps
    }

    val toVisit = mutable.Queue(needed.toSeq: _*)
    var activeNodes = Set[ProjNode]()
    var newPredicates = Set[TyPredicate]()
    while (toVisit.nonEmpty) {
      val n = toVisit.dequeue()
      activeNodes += n
      val (ns, ps) = neighbours(n.n)
      newPredicates ++= ps
      toVisit.enqueue(ns.diff(activeNodes).toSeq: _*)
    }

    val newNodes = activeNodes.map(_.n) ++ graph.nodes.filter(_.fromLib)
    val newAnnots = graph.userAnnotations.filter { case (n, _) => newNodes.contains(n.n) }
    PredicateGraph(newNodes, newPredicates, newAnnots, graph.typeMap).tap { g =>
      printResult(
        s"Before pruning: ${graph.nodes.size}, after: ${g.nodes.size}"
      )
    }
  }

  def parseProject(
      libDefs: LibDefs,
      projectsBase: Path,
      projectRoot: Path,
      predictAny: Boolean,
      gModules: Vector[GModule] = null,
      skipSet: Set[String] = Set("dist", "__tests__", "test", "tests"),
      shouldPruneGraph: Boolean = true,
      shouldPrintProject: Boolean = false,
      warnOnErrors: Boolean = true,
      errorHandler: ErrorHandler = ErrorHandler.alwaysThrowError,
  ): ParsedProject =
    SimpleMath.withErrorMessage(s"In project: $projectRoot") {
      import libDefs._

      var p: GProject = null
      if(gModules != null) {
        p = ProgramParsing.parseGProjectFromRoot(
          projectRoot,
          gModules,
          filter = (path: Path) => {
            path.segments.forall(!skipSet.contains(_))
          }
        )
      } else {
        p = ProgramParsing.parseGProjectFromRoot(
          projectRoot,
          filter = (path: Path) => {
            path.segments.forall(!skipSet.contains(_))
          }
        )
      }

      if (shouldPrintProject) println { p.prettyPrint }

      val allocator = new PNodeAllocator(forLib = false)
      val irTranslator = new IRTranslation(allocator)

      val projectName = projectRoot.relativeTo(projectsBase)
      val qModules = QLangTranslation.fromProject(
        projectName,
        p.modules,
        baseCtx,
        libExports,
        allocator,
        p.pathMapping,
        p.devDependencies,
        errorHandler
      )
      val irModules = qModules.map(irTranslator.fromQModule)
      val allAnnots = irModules.flatMap(_.mapping).toMap
      val fixedAnnots = allAnnots.collect { case (n, Annot.Fixed(t)) => n -> t }

      val graph0 =
        PredicateGraphTranslation.fromIRModules(
          fixedAnnots,
          allocator,
          irModules
        )
      val userTypes =
        graph0.nodes.filter(n => !n.fromLib && n.isType).map(ProjNode)

      val userAnnots = allAnnots.collect {
        case (n, Annot.User(t, _)) => ProjNode(n) -> t
      }
      val graph =
        if (shouldPruneGraph)
          pruneGraph(graph0, userAnnots.keySet ++ userTypes)
        else graph0

      if (warnOnErrors)
        errorHandler.warnErrors()
      printResult(s"Project parsed: '$projectRoot'")
      println("number of nodes: " + graph.nodes.size)

      ParsedProject(projectName, p.srcTexts, qModules, irModules, graph, predictAny)
    }
}
