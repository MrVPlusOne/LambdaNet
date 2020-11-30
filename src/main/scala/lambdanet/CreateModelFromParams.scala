package lambdanet

import ammonite.ops.{Path, pwd}
import lambdanet.TypeInferenceService.{ModelConfig, loadModel}

import scala.io.StdIn

object CreateModelFromParams{
  def main(args: Array[String]): Unit = {
    val paramPath = Path(StdIn.readLine("Enter param path (source):"), pwd)
    val modelCachePath = Path(StdIn.readLine("Enter model cache path (destination):"), pwd)
    loadModel(paramPath, modelCachePath, ModelConfig(), numOfThreads = 8)
    printResult("Model created.")
  }
}
