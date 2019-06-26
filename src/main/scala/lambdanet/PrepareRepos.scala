package lambdanet

import ammonite.ops._
import funcdiff.SimpleMath
import lambdanet.translation.ImportsResolution.{ErrorHandler, ModuleExports}
import lambdanet.translation.{IRTranslation, ImportsResolution, PAnnot, PLangTranslation, PredicateGraph, PredicateGraphTranslation, QLangTranslation}
import lambdanet.translation.PredicateGraph.{PNode, PNodeAllocator, PType, ProjNode}
import lambdanet.utils.{ProgramParsing, Serialization}
import lambdanet.utils.ProgramParsing.GProject
@SerialVersionUID(2)
case class LibDefs(
    baseCtx: ModuleExports,
    nodeMapping: Map[PNode, PAnnot],
    libAllocator: PNodeAllocator,
    libExports: Map[ProjectPath, ModuleExports],
)

object PrepareRepos {

  val dataSetPath: Path = pwd / "results" / "predicateGraphs.serialized"

  def main(args: Array[String]): Unit = {
    val parsed = announced("parsePredGraphs")(parseRepos())
    val stats = repoStatistics(parsed.graphs)
    println(resultStr(stats.headers.zip(stats.average).toString()))

    //todo: implement serialization
//    import boopickle.Default._
//
//    implicit val pNodePickler = transformPickler(PNode.fromTuple)(PNode.toTuple)
//    implicit val symbolPickler = transformPickler(Symbol.apply)(_.name)
//
//    announced(s"save data set to file: $dataSetPath") {
//      val bytes = Pickle.intoBytes(parsed)
//      Serialization.writeByteBuffer(dataSetPath.toIO)(bytes)
//    }
//
//    announced("read"){
//      val newBytes = Serialization.readByteBuffer(dataSetPath.toIO)
//      val got = Unpickle.apply[ParsedRepos].fromBytes(newBytes)
//
//      println(got)
//    }
  }

  @SerialVersionUID(0)
  case class ParsedRepos(
      libDefs: LibDefs,
      graphs: Vector[(ProjectPath, PredicateGraph, List[(ProjNode, PType)])],
  )

  def parseRepos(): ParsedRepos = {

    /** Only projects for which this predicate returns true will be parsed */
    def filter(path: Path): Boolean = true

    /** set to true to load declarations from the serialization file */
    val loadFromFile = true

    val libDefsFile = pwd / up / "lambda-repos" / "libDefs.serialized"

    val libDefs = if (loadFromFile) {
      println(s"loading library definitions from $libDefsFile...")
      val read = SimpleMath.readObjectFromFile[LibDefs](libDefsFile.toIO)
      println(s"library definitions loaded.")
      read
    } else {
      val defs = parseLibDefs()
      SimpleMath.saveObjectToFile(libDefsFile.toIO)(defs)
      println(s"library definitions saved to $libDefsFile")
      defs
    }

    val projectsDir = pwd / up / "lambda-repos" / "projects"

    lambdanet.shouldWarn = false

    val graphs = (ls ! projectsDir).par
      .collect {
        case f if f.isDir && filter(f) =>
          val p = prepareProject(libDefs, f).copy()
          p.copy(_1 = p._1.relativeTo(projectsDir))
      }
      .seq
      .toVector
    ParsedRepos(libDefs, graphs)
  }

  case class RepoStats(
      average: Vector[Double],
      data: Map[ProjectPath, Vector[Int]],
  ) {
    val libNodes = 1
    val projNodes = 2
    val annotations = 3
    val predicates = 4

    val headers: Vector[String] =
      Vector("libNodes", "projNodes", "annotations", "predicates")
  }

  def repoStatistics(
      results: Seq[(ProjectPath, PredicateGraph, List[(ProjNode, PType)])],
  ): RepoStats = {
    val rows = results
      .map {
        case (path, graph, annots) =>
          val nLib = graph.nodes.count(_.fromLib)
          val nProj = graph.nodes.count(!_.fromLib)
          val nPred = graph.predicates.size
          val annotations = annots.length
          path -> Vector(nLib, nProj, annotations, nPred)
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
    val (baseCtx, libAllocator, defaultMapping) =
      QLangTranslation.parseDefaultModule()
    println("default module parsed")

    println("parsing library modules...")
    val GProject(_, modules, mapping, subProjects, devDependencies) =
      ProgramParsing
        .parseGProjectFromRoot(declarationsDir, declarationFileMod = true)

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
      pModules,
      baseCtx,
      resolved1,
      mapping,
      defaultPublicMode = true,
      errorHandler = handler,
      devDependencies,
      maxIterations = 5,
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
                s"Couldn't find Exports located at $path for $name, ignore this named project.",
              )
              ModuleExports.empty
            },
          ),
        )
    }
    handler.warnErrors()
    val libExports = exports ++ namedExports

    val qModules =
      pModules.map { m =>
        QLangTranslation.fromPModule(m, baseCtx1 |+| exports(m.path))
      }

    val nodeMapping = defaultMapping ++ qModules.flatMap(_.mapping)

    println("Declaration files parsed.")
    LibDefs(baseCtx1, nodeMapping, libAllocator, libExports)
  }

  private def prepareProject(libDefs: LibDefs, root: Path) =
    SimpleMath.withErrorMessage(s"In project: $root") {
      import libDefs._

      val skipSet = Set("dist", "__tests__", "test", "tests") //todo: also parsing the tests
      def filterTests(path: Path): Boolean = {
        path.segments.forall(!skipSet.contains(_))
      }

      val p = ProgramParsing.parseGProjectFromRoot(root, filter = filterTests)
      val allocator = new PNodeAllocator(forLib = false)
      val irTranslator = new IRTranslation(allocator)

      val errorHandler =
        ErrorHandler(ErrorHandler.ThrowError, ErrorHandler.StoreError)

//    println(s"LibExports key set: ${libExports.keySet}")
      val irModules = QLangTranslation
        .fromProject(
          p.modules,
          baseCtx,
          libExports,
          allocator,
          p.pathMapping,
          p.devDependencies,
          errorHandler,
        )
        .map(irTranslator.fromQModule)
      val allAnnots = irModules.flatMap(_.mapping).toMap
      val fixedAnnots = allAnnots.collect { case (n, Annot.Fixed(t)) => n -> t }
      val userAnnots = allAnnots.collect {
        case (n, Annot.User(t)) => ProjNode(n) -> t
      }

      val graph =
        PredicateGraphTranslation.fromIRModules(fixedAnnots, irModules)

      errorHandler.warnErrors()
      println(s"Project parsed: '$root'")

      (root, graph, userAnnots.toList)
    }

}
