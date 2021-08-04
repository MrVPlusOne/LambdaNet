package lambdanet.test

import ammonite.ops.Path
import funcdiff.SimpleMath
import funcdiff.SimpleMath.{readObjectFromFile, stdDev}
import lambdanet.PrepareRepos.{ParsedRepos, parsedReposDir}
import lambdanet.train.DataSet
import lambdanet.{Model, announced}

object TestUtils {
  def loadModelData(modelDir: Path) = {
    val modelPath = modelDir / "model.serialized"
    val model = announced("Loading model") {
      readObjectFromFile[Model](modelPath)
    }
    import model.config.{predictAny, onlyPredictLibType}
    val repos = ParsedRepos.readFromDir(parsedReposDir(predictAny))
    val dataSet = DataSet.makeDataSet(repos, onlyPredictLibType, predictAny)
    (model, repos, dataSet)
  }

  case class Gaussian(mean: Double, std: Double) {
    override def toString: String = {
      val precision = if (std < 1.0) math.ceil(-math.log10(std)).toInt else 1
      s"%.${precision}f±%.${precision+1}f".format(mean, std)
    }
  }

  object Gaussian {
    def apply(xs: Seq[Double]): Gaussian =
      Gaussian(SimpleMath.mean(xs), stdDev(xs))
  }
}
