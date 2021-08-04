package lambdanet

import ammonite.ops._
import funcdiff.ParamCollection
import lambdanet.train.Training.SystemConfig
import lambdanet.train.{DataSet, Timeouts, Training}
import lambdanet.translation.ImportsResolution.{ErrorHandler, NameDef}
import lambdanet.translation.PredicateGraph.{PAny, PNode}
import lambdanet.utils.QLangDisplay

import scala.util.Random

object RunTrainedModel {

  val modelDir: Path = pwd / "models"

  @deprecated(
    "Instead of loading a ParamCollection, load a saved Model instead. The `Model` " +
      "class contains the all the hyper-parameters needed to reconstruct the original model."
  )
  def runTrainedModel(
      paramPath: Path,
      sourcePath: Path,
      outputPath: Path,
      numOfThreads: Int = 8
  ): Unit = {
    import PrepareRepos._

    val predictAny = false
    val repos = ParsedRepos.readFromDir(parsedReposDir(predictAny))
    val libDefs = repos.libDefs
    val handler = ErrorHandler(ErrorHandler.StoreError, ErrorHandler.StoreError)
    val testProject =
      parseProject(
        libDefs,
        sourcePath / up,
        sourcePath,
        skipSet = Set("node_modules", "__tests__", "test", "tests"),
        shouldPruneGraph = false,
        errorHandler = handler,
        predictAny = predictAny,
      )
    val repos1 = repos.copy(devSet = List(), testSet = List(testProject))
    val dataSet = DataSet.makeDataSet(
      repos1,
      testSetUseInferred = false,
      //todo: change this if you want to predict user defined types
      onlyPredictLibType = false,
      predictAny = false,
    )

    val model = announced("Loading model...") {
      val pc = ParamCollection.fromFile(paramPath)

      Training
        .Trainer(
          Training.ModelConfig(),
          SystemConfig(numOfThreads = numOfThreads, pwd / "test-trained")
        )
        .makeModel(pc, dataSet)
    }

    val datum = dataSet.testSet.head
    // runs the model on the test set
    val (_, fwd, pred) = model
      .forward(
        datum,
        shouldDownsample = false,
        shouldDropout = false,
        maxBatchSize = None
      )(funcdiff.ModeEval)
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

  /** Renames js files as ts files (needed to run our tool on js files)  */
  def renameToTs(): Unit = {
    val jsFiles = PrepareRepos.reposDir / RelPath("javascript-algorithms-ts")
    for { f <- ls.rec(jsFiles) if f.ext == "js" } {
      def changeExtension(name: String, old: String, newExt: String): String = {
        name.dropRight(old.length) + newExt
      }
      mv(
        f,
        jsFiles / RelPath(
          changeExtension(f.relativeTo(jsFiles).toString(), "js", "ts")
        )
      )
    }
  }

  /** Checks predictions on a class of simple functions */
  def searchForFunctions(): Unit = {
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
      !name.contains("$Lambda") && !name.contains("CONSTRUCTOR")
    }

    val random = new Random(1)

    val repos = DataSet.loadRepos(toyMode = false, predictAny = false)
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
    // todo: choose the trained weights depending on if to predict user defined types
    val paramPath = modelDir / RelPath(
      "newParsing-GAT1-fc2-newSim-decay-6/params.serialized"
//      "newParsing-lib-GAT1-fc2-newSim-decay-6/params.serialized"
    )
    // todo: change this to the root directory of the target TS project to predict
    val sourcePath = pwd / RelPath("data/train/mojiito-master")

    val outputPath = sourcePath

    import scala.concurrent.duration._
    Timeouts.forwardTimeout = 1000.seconds
    Timeouts.optimizationTimeout = 1000.seconds
    runTrainedModel(paramPath, sourcePath, outputPath)
  }
}
