package lambdanet.test

import funcdiff.{GraphMode, ModeEval, SimpleMath}
import lambdanet.PrepareRepos.parseGProject
import lambdanet.{ChainingSyntax, Configs}
import lambdanet.test.TestUtils.{Gaussian, loadModelData}
import lambdanet.train.{Counted, toAccuracy, toAccuracyD}
import lambdanet.train.Training.AnnotsSampling
import lambdanet.translation.ImportsResolution.ErrorHandler
import lambdanet.utils.ProgramParsing.GProject
import ammonite.{ops => amm}
import lambdanet.translation.PredicateGraph.ProjNode
import me.tongfei.progressbar.ProgressBar

import scala.util.Random

object EvalFileLevelPrediction {
  def main(args: Array[String]): Unit = {
    val modelDir = Configs().modelDir()
    val (model, repos, _) = loadModelData(modelDir)
    import repos.{libDefs, testSet}
    val annotsRatio = 0.5
    val repeats = 10
    val sampler = AnnotsSampling(minKeepProb = annotsRatio, maxKeepProb = annotsRatio)
    val rand = new Random(123)

    implicit val m: GraphMode = ModeEval
    val service = model.PredictionService(Configs().numOfThreads(), predictTopK = 1)

    import cats.implicits._
    val projects = List.fill(repeats)(testSet).flatten
    val nModules = projects.map(_.gProject.modules.size).sum
    val prog = new ProgressBar("File-level evaluation", nModules)
    val (avgAcc, parseTimes, predTimes) = projects.foldMap { p =>
      val proj = p.gProject
      proj.modules.foldMap { mod =>
        val miniProj = GProject.fromSingleModule(mod, proj.path, proj.srcTexts)
        val (parsed, parseTime) = SimpleMath.measureTimeAsMillis {
          parseGProject(
            miniProj,
            libDefs,
            predictAny = model.config.predictAny,
            warnOnErrors = false,
            errorHandler = ErrorHandler.alwaysStoreError,
          )
        }
        val (kept, drop) = sampler.randomSplit(parsed.allUserAnnots, rand)
        val toPredict = drop.collect {
          case (n, ty) if !parsed.rawAllUserAnnots(n.n).inferred =>
            n -> service.nonGenerifyIt(ty)
        }
        if (toPredict.isEmpty) {
          // No prediction needed; skip this file
          prog.step()
          (Map[Boolean, Counted[Int]](), Vector(), Vector())
        } else {
          val (preds, predTime) = SimpleMath.measureTimeAsMillis {
            service.predictOnParsedProject(
              parsed,
              toPredict.keySet,
              excludeAnnots = drop.keySet,
            )
          }
          val acc = preds.toList.foldMap {
            case (n, tDist) =>
              val label = toPredict(ProjNode(n))
              val isLib = label.madeFromLibTypes
              Map(isLib -> Counted.fromBool(label == tDist.topValue))
          }
          prog.step()
          (acc, Vector(parseTime), Vector(predTime))
        }
      }
    }
    prog.close()

    val totalAcc = toAccuracy(avgAcc(true) |+| avgAcc(false))
    val libAcc = toAccuracy(avgAcc(true))
    val projAcc = toAccuracy(avgAcc(false))
    val summary =
      s"""total accuracy: ${totalAcc*100}%
         |library type accuracy: ${libAcc*100}%
         |project type accuracy: ${projAcc*100}%
         |parsing time: ${Gaussian(parseTimes)}ms
         |inference time: ${Gaussian(predTimes)}ms""".stripMargin
    println("----- Results -----")
    println(summary)
    amm.write.over(modelDir / "EvalFileLevel_summary.txt", summary)

    val line1 = ("parsing time (ms)" +: parseTimes.map(_.toString)).mkString(",")
    val line2 = ("inference time (ms)" +: predTimes.map(_.toString)).mkString(",")
    val table = List(line1, line2).mkString("\n")
    amm.write.over(modelDir / "EvalFileLevel_times.csv", table)
  }
}
