package lambdanet

import ammonite.ops._
import funcdiff.{Optimizer, ParamCollection, SimpleMath}
import lambdanet.train.{DataSet, Datum, Timeouts, TrainingLoop, TrainingState}
import lambdanet.translation.ImportsResolution.{ErrorHandler, NameDef}
import lambdanet.translation.PredicateGraph.{PAny, PNode}
import lambdanet.utils.QLangDisplay

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.Random

object RunTrainedModel {

  def runTrainedModel(
      paramPath: Path,
      sourcePath: Path,
      outputPath: Path,
      numOfThreads: Int = 8
  ) = {
    import PrepareRepos._

    val repos = DataSet.loadRepos(toyMode = false)
    val libDefs = repos.libDefs
    val handler = ErrorHandler(ErrorHandler.StoreError, ErrorHandler.StoreError)
    val testProject =
      prepareProject(
        libDefs,
        sourcePath / up,
        sourcePath,
        skipSet = Set("node_modules", "__tests__", "test", "tests"),
        shouldPruneGraph = false,
        errorHandler = handler
      )
    val repos1 = repos.copy(devSet = List(), testSet = List(testProject))
    val dataSet = DataSet.makeDataSet(
      repos1,
      Some(new ForkJoinTaskSupport(new ForkJoinPool(numOfThreads))),
      useSeqModel = false,
      toyMode = false,
      testSetUseInferred = false,
      onlyPredictLibType = true,
    )

    val pc = ParamCollection.fromFile(paramPath)
    val model = announced("Loading model...") {
      TrainingLoop
        .config(numOfThreads = 8, pwd / "test-trained", None)
        .makeModel(pc, dataSet)
    }

    val datum = dataSet.testSet.head
    val (_, fwd, pred) = model
      .forward(
        datum,
        shouldDownsample = false,
        shouldDropout = false,
        maxBatchSize = None
      )
      .get
    QLangDisplay.renderProjectToDirectory(
      datum.projectName.toString,
      datum.qModules,
      pred,
      datum.predictionSpace.allTypes
    )(outputPath / "predictions")

    import train.toAccuracy
    println("libAccuracy: " + toAccuracy(fwd.libCorrect))
    println("projectAccuracy: " + toAccuracy(fwd.projCorrect))
  }

  def renameToTs() = {
    val r = pwd / RelPath("../lambda-repos/javascript-algorithms-ts")
    for { f <- ls.rec(r) if f.ext == "js" } {
      def changeExtension(name: String, old: String, newExt: String): String = {
        name.dropRight(old.length) + newExt
      }
      mv(
        f,
        r / RelPath(changeExtension(f.relativeTo(r).toString(), "js", "ts"))
      )
    }
  }

  def searchForFunctions() = {
    import translation.QLang._
    val unknownType = NameDef.unknownType

    def isLibType(module: QModule)(n: PNode): Boolean = {
      module.mapping.get(n).exists {
        case Annot.User(ty, false) =>
          ty.madeFromLibTypes && ty != unknownType && ty != PAny
        case _ => false
      }
    }

    def isValidName(name: String): Boolean = {
      ! name.contains("$Lambda") && ! name.contains("CONSTRUCTOR")
    }

    val random = new Random(1)

    val repos = DataSet.loadRepos(toyMode = false)
    val selected = for {
      p <- repos.devSet.toVector
      m <- p.qModules
      f @ FuncDef(_, args, returnType, _) <- m.stmts.filter(
        _.isInstanceOf[FuncDef]
      )
      if args.length > 1 && args.length < 6 && args.forall(isLibType(m)) &&
        f.funcNode.nameOpt.exists(s => isValidName(s.name))
    } yield {
      val annots = (args :+ returnType).map(m.mapping)
      (p.path / m.path) -> (f, annots)
    }

    printResult(s"Selected from ${selected.length} candidate functions.")
    random.shuffle(selected).take(50).foreach {
      case (path, (f, annots)) =>
        printResult(s"$path")
        println(s"annots: $annots")
        println(f)
    }
  }

  def main(args: Array[String]): Unit = {
//    searchForFunctions()

    val paramPath = pwd / RelPath(
//      "../lambda-repos/newParsing-GAT1-fc2-newSim-decay-6/params.serialized"
      "../lambda-repos/newParsing-lib-GAT1-fc2-newSim-decay-6/params.serialized"
    )
    val sourcePath = pwd / RelPath("data/comparison")
//    val sourcePath = pwd / RelPath("../lambda-repos/playground")

    val outputPath = sourcePath

    import scala.concurrent.duration._
    Timeouts.forwardTimeout = 1000.seconds
    Timeouts.optimizationTimeout = 1000.seconds
    runTrainedModel(paramPath, sourcePath, outputPath)
  }
}
