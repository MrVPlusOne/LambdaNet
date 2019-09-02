package lambdanet

import ammonite.ops._
import funcdiff.SimpleMath
import lambdanet.translation.IR.IRModule
import lambdanet.translation.ImportsResolution.{ErrorHandler, ModuleExports}
import lambdanet.translation._
import lambdanet.translation.PredicateGraph.{
  DefineRel,
  LibNode,
  PNode,
  PNodeAllocator,
  PObject,
  PType,
  ProjNode,
  TyPredicate
}
import lambdanet.translation.QLang.QModule
import lambdanet.utils.ProgramParsing
import lambdanet.utils.ProgramParsing.GProject

import scala.collection.mutable
import scala.util.Random

@SerialVersionUID(2)
case class LibDefs(
    nodeForAny: PNode,
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
  val parsedRepoPath: Path = pwd / "data" / "parsedDataSet.serialized"

  def main(args: Array[String]): Unit = {
//    val defs = parseLibDefs()
//    SimpleMath.saveObjectToFile(libDefsFile.toIO)(defs)
//    println(s"library definitions saved to $libDefsFile")

//    parseAndFilterDataSet()
//    mixTestDevSet()
    parseAndSerializeDataSet()
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
              val diff =
                (p0.pGraph.nodes ++ p0.pGraph.predicates.flatMap(_.allNodes))
                  .diff(libDefs.nodeMapping.keySet)
              val libNode = diff.find(_.fromLib)
              assert(
                libNode.isEmpty,
                s"lib node not in libDefs: ${libNode.get}"
              )
              Successful(p0)
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
    val trainSetDir: Path = pwd / up / "lambda-repos" / "allRepos"
    var progress = 0
    announced("parsePredGraphs")(
      parseRepos(
        Seq(trainSetDir),
        inParallel = true,
        maxLinesOfCode = 20000,
        parsedCallback = (file, pOpt) => {
          val dest = pOpt match {
            case Successful(p) =>
              val nodes = p.pGraph.nodes.size
              if (nodes > 500 && nodes < 10000)
                "filteredRepos"
              else "TooBigOrSmall"
            case TooBigOrSmall =>
              "TooBigOrSmall"
            case HasError(ex) =>
              printWarning(
                s"$file contains error: ${ex.getMessage}",
                mustWarn = true
              )
              "HasError"
          }
          val dir = trainSetDir / up / dest
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

  def mixTestDevSet(): Unit = {
    val random = new Random(1)
    val base = pwd / up / "lambda-repos"
    val allProjects = ls(base / "filteredRepos")
      .filter(f => f.isDir && f.last != "toy")
      .pipe(random.shuffle(_))

    def tryMove(from: Path, to: Path): Unit = {
      if (to != from) mv(from, to)
    }

    val num = allProjects.length
    allProjects.take(40).foreach { f =>
      tryMove(f, base / "bigger" / "testSet" / f.last)
    }
    allProjects.slice(40, 80).foreach { f =>
      tryMove(f, base / "bigger" / "devSet" / f.last)
    }
    allProjects.drop(80).foreach { f =>
      tryMove(f, base / "bigger" / "trainSet" / f.last)
    }
  }

  def parseAndSerializeDataSet(): Unit = {
    val basePath = pwd / up / "lambda-repos" / "bigger"
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
    write.over(parsedRepoPath / up / "stats.txt", avgStats)

    announced(s"save data set to file: $parsedRepoPath") {
      SM.saveObjectToFile(parsedRepoPath.toIO)(
        ParsedRepos(libDefs, trainSet, devSet, testSet)
      )
    }
  }

  @SerialVersionUID(2)
  case class ParsedProject(
      path: ProjectPath,
      gProject: GProject,
      qModules: Vector[QModule],
      irModules: Vector[IRModule],
      pGraph: PredicateGraph
  ){
    @transient
    lazy val allUserAnnots: Map[ProjNode, PType] = {
      val allAnnots = irModules.flatMap(_.mapping).toMap
      allAnnots.collect {
        case (n, Annot.User(t, _)) => ProjNode(n) -> t
      }
    }

    @transient
    lazy val nonInferredUserAnnots: Map[ProjNode, PType] = {
      val allAnnots = irModules.flatMap(_.mapping).toMap
      allAnnots.collect {
        case (n, Annot.User(t, false)) => ProjNode(n) -> t
      }
    }

    def mergeEqualities: ParsedProject = {
      val (graph1, merger) = pGraph.mergeEqualities
      val qModules1 = qModules.map{ _.mapNodes(merger)}
      val irModules1 = irModules.map{ _.mapNodes(merger)}
      ParsedProject(path, gProject, qModules1, irModules1, graph1)
    }


  }

  @SerialVersionUID(2)
  case class ParsedRepos(
      libDefs: LibDefs,
      trainSet: List[ParsedProject],
      devSet: List[ParsedProject],
      testSet: List[ParsedProject]
  )

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
      .map {
        case p@ParsedProject(path, _, _, _, graph) =>
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
          declarationFileMod = true
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

    val anyNode = libAllocator.newNode(None, isType = true)

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
    LibDefs(anyNode, baseCtx1, nodeMapping, libExports, classes)
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
      errorHandler: ErrorHandler =
        ErrorHandler(ErrorHandler.ThrowError, ErrorHandler.ThrowError)
  ): ParsedProject =
    SimpleMath.withErrorMessage(s"In project: $projectRoot") {
      import libDefs._

      val p = ProgramParsing.parseGProjectFromRoot(
        projectRoot,
        filter = (path: Path) => {
          path.segments.forall(!skipSet.contains(_))
        }
      )

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
          nodeForAny,
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

      ParsedProject(
        projectName,
        p,
        qModules,
        irModules,
        graph
      )
    }

}
