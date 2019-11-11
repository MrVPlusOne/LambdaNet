package lambdanet

import ammonite.ops._
import funcdiff.SimpleMath
import lambdanet.translation.IR.IRModule
import lambdanet.translation.ImportsResolution.{
  ErrorHandler,
  ModuleExports,
  NameDef
}
import lambdanet.translation._
import lambdanet.translation.PredicateGraph.{
  DefineRel,
  LibNode,
  PNode,
  PNodeAllocator,
  PObject,
  PTyVar,
  PType,
  ProjNode,
  TyPredicate
}
import lambdanet.translation.QLang.QModule
import lambdanet.utils.ProgramParsing
import lambdanet.utils.ProgramParsing.GProject

import scala.collection.{immutable, mutable}
import scala.util.Random

@SerialVersionUID(2)
case class LibDefs(
    baseCtx: ModuleExports,
    nodeMapping: Map[PNode, PAnnot],
    libExports: Map[ProjectPath, ModuleExports],
    classes: Set[QLang.ClassDef]
) {
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
  val libDefsFile: Path = pwd / up / "lambda-repos" / "libDefs.serialized"
//  val parsedRepoPath: Path = pwd / "data" / "parsedDataSet.serialized"
  val parsedReposDir: Path = pwd / 'data / "parsedRepos"

  val allReposDir: Path = pwd / up / "lambda-repos" / "allRepos"

  def main(args: Array[String]): Unit = {
//    val defs = parseLibDefs()
//    SimpleMath.saveObjectToFile(libDefsFile.toIO)(defs)
//    println(s"library definitions saved to $libDefsFile")

//    remixDividedDataSet()
//    parseAndFilterDataSet()
    divideDataSet()
//    parseAndSerializeDataSet()
  }

  private def remixDividedDataSet(): Unit = {
    val base = pwd / up / "lambda-repos" / "divided"
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
      loadFromFile: Boolean = true,
      inParallel: Boolean = true,
      maxLinesOfCode: Int = Int.MaxValue,
      parsedCallback: (Path, RepoResult) => Unit = (_, _) => ()
  ): (LibDefs, Seq[List[ParsedProject]]) = {
    lambdanet.shouldWarn = false

    val libDefs = if (loadFromFile) {
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
        .pipe(x => if (inParallel) x.par else x)
        .flatMap { f =>
          val r = if (countTsCode(f, dir) < maxLinesOfCode) {
            try {
              val p0 = prepareProject(
                libDefs,
                dir,
                f,
                shouldPruneGraph = false,
                errorHandler =
                  ErrorHandler(ErrorHandler.StoreError, ErrorHandler.StoreError)
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

  def parseAndFilterDataSet(): Unit = {
    def projectDestination(p: ParsedProject): String = {
      val nodes = p.pGraph.nodes.size
      if (nodes < 500 || nodes > 10000)
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
        loadFromFile = false,
        inParallel = true,
        maxLinesOfCode = 20000,
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
    val base = pwd / up / "lambda-repos"
    val allProjects = ls(base / "filteredRepos")
      .filter(f => f.isDir && f.last != "toy")
      .pipe(random.shuffle(_))

    def tryMove(from: Path, to: Path): Unit = {
      if(!exists(to/up)) mkdir(to/up)
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

  private def testNewSerialization(): Unit = {
    val repos = announced("read1") {
      SM.readObjectFromFile[ParsedRepos](
        (parsedReposDir / up / "parsedDataSet.serialized").toIO
      )
    }

    announced("write") {
      repos.serializeIntoDir(parsedReposDir)
    }

    announced("read2") {
      ParsedRepos.readFromDir(parsedReposDir)
    }

  }

  def parseAndSerializeDataSet(): Unit = {
    val basePath = pwd / up / "lambda-repos" / "divided"
    val trainSetDir: Path = basePath / "trainSet"
    val devSetDir: Path = basePath / "devSet"
    val testSetDir: Path = basePath / "testSet"
    val (libDefs, Seq(trainSet, devSet, testSet)) =
      announced("parsePredGraphs") {
        var progress = 0
        parseRepos(
          Seq(trainSetDir, devSetDir, testSetDir),
          loadFromFile = false,
          parsedCallback = (_, _) =>
            synchronized {
              progress += 1
              printResult(s"Progress: $progress")
            }
        )
      }
    val stats = repoStatistics(trainSet ++ devSet ++ testSet)
    val avgStats = stats.headers
      .zip(stats.average)
      .map {
        case (h, n) => "%s: %.1f".format(h, n)
      }
      .mkString(", ")
    printResult(avgStats)
    write.over(parsedReposDir / up / "stats.txt", avgStats)

    announced(s"save data set to dir: $parsedReposDir") {
      ParsedRepos(libDefs, trainSet, devSet, testSet)
        .serializeIntoDir(parsedReposDir)
    }
  }

  // don't predict unknown and any
  val typesNotToPredict: Set[PType] =
    Set(NameDef.unknownType, NameDef.anyType)

  @SerialVersionUID(2)
  case class ParsedProject(
      path: ProjectPath,
      qModules: Vector[QModule],
      irModules: Vector[IRModule],
      pGraph: PredicateGraph
  ) {
    @transient
    lazy val allUserAnnots: Map[ProjNode, PType] = {
      val allAnnots = irModules.flatMap(_.mapping).toMap
      allAnnots.collect {
        case (n, Annot.User(t, _)) if !typesNotToPredict.contains(t) =>
          ProjNode(n) -> t
      }
    }

    @transient
    lazy val nonInferredUserAnnots: Map[ProjNode, PType] = {
      val allAnnots = irModules.flatMap(_.mapping).toMap
      allAnnots.collect {
        case (n, Annot.User(t, false)) if !typesNotToPredict.contains(t) =>
          ProjNode(n) -> t
      }
    }

    def mergeEqualities: ParsedProject = {
      val (graph1, merger) = pGraph.mergeEqualities
      val qModules1 = qModules.map { _.mapNodes(merger) }
      val irModules1 = irModules.map { _.mapNodes(merger) }
      ParsedProject(path, qModules1, irModules1, graph1)
    }
  }

  import concurrent.{Future, Await}
  import concurrent.ExecutionContext.Implicits.global
  import concurrent.duration._

  @SerialVersionUID(2)
  case class ParsedRepos(
      libDefs: LibDefs,
      trainSet: List[ParsedProject],
      devSet: List[ParsedProject],
      testSet: List[ParsedProject]
  ) {
    import ParsedRepos._

    def meta(chunkNum: Int) =
      Meta(trainSet.length, devSet.length, testSet.length, chunkNum)

    def serializeIntoDir(
        dir: Path,
        timeoutSeconds: Int = 200,
        chunkSize: Int = 50
    ): Unit = {
      import cats.implicits._

      if (exists(dir)) rm(dir)
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

    def readFromDir(dir: Path, timeoutSeconds: Int = 200): ParsedRepos = {
      import cats.implicits._

      val meta = SM.readObjectFromFile[Meta]((dir / "meta").toIO)
      import meta._
      val libDefsF = Future {
        announced("read libDefs") {
          SM.readObjectFromFile[LibDefs]((dir / "libDefs").toIO)
        }
      }
      Thread.sleep(100)
      val chunksF = (0 until chunkNum).toVector.map { i =>
        Future {
          SM.readObjectFromFile[List[ParsedProject]]((dir / s"chunk$i").toIO)
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
      Vector("libNodes", "projNodes", "libAnnots", "projAnnots", "predicates")
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
        path -> Vector(nLib, nProj, libAnnots, projAnnots, nPred)
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
    val declarationsDir = pwd / up / "lambda-repos" / "declarations"

    println("parsing default module...")
    val (baseCtx, libAllocator, defaultMapping, defaultModule) =
      QLangTranslation.parseDefaultModule()
    println("default module parsed")

    println("parsing library modules...")
    val GProject(_, modules, mapping, subProjects, devDependencies) =
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
    PredicateGraph(newNodes, newPredicates).tap { g =>
      printResult(
        s"Before pruning: ${graph.nodes.size}, after: ${g.nodes.size}"
      )
    }
  }

  def prepareProject(
      libDefs: LibDefs,
      projectsBase: Path,
      projectRoot: Path,
      skipSet: Set[String] = Set("dist", "__tests__", "test", "tests"),
      shouldPruneGraph: Boolean = true,
      shouldPrintProject: Boolean = false,
      errorHandler: ErrorHandler =
        ErrorHandler(ErrorHandler.ThrowError, ErrorHandler.ThrowError)
  ): ParsedProject =
    SimpleMath.withErrorMessage(s"In project: $projectRoot") {
      import libDefs._

      val p = ProgramParsing.parseGProjectFromRoot(
        projectRoot,
        filter = (path: Path) => {
          path.segments.forall(!skipSet.contains(_))
        },
      )

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

      errorHandler.warnErrors()
      printResult(s"Project parsed: '$projectRoot'")
      println("number of nodes: " + graph.nodes.size)

      ParsedProject(projectName, qModules, irModules, graph)
    }

}
