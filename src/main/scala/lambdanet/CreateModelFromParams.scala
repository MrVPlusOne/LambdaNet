package lambdanet

import ammonite.ops.{Path, pwd}
import lambdanet.TypeInferenceService.{ModelConfig, loadModel}

import scala.io.StdIn

object CreateModelFromParams{
  def main(args: Array[String]): Unit = {
    println("Note: it's better to run this task at the machine that created " +
      "the specified params.serialized file to ensure that the created model" +
      "uses the correct libDefs and parsedRepos, etc.")
    val paramPath = Path(StdIn.readLine("Enter param path (source):"), pwd)
    val modelCachePath = Path(StdIn.readLine("Enter model cache path (destination):"), pwd)
    val model = loadModel(paramPath, modelCachePath, ModelConfig(), numOfThreads = 8)
    model.libTypesToPredict.foreach(println)
    printResult("Model created.")
  }
}
