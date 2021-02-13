package lambdanet

import ammonite.ops.Path
import ammonite.{ops => amm}
import funcdiff.SimpleMath.{readObjectFromFile}
import lambdanet.train.{LossAggMode, TopNDistribution}
import lambdanet.translation.PredicateGraph


object TypeInferenceService {

  @SerialVersionUID(3L)
  case class ModelConfig(
      gnnIterations: Int = 8,
      dimMessage: Int = 32,
      gatHeads: Int = 1,
      lossAggMode: LossAggMode.Value = LossAggMode.Product,
      seed: Long = 1,
  )

  case class PredictionResults(
      map: Map[PredicateGraph.PNode, TopNDistribution[PredicateGraph.PType]]
  ) {
    def prettyPrint(): Unit = {
      val byFile = map.keys.groupBy(_.srcSpan.get.srcFile).toSeq.sortBy(_._1)
      byFile.foreach {
        case (file, nodes) =>
          println(s"=== File: $file ===")
          nodes.toSeq.sortBy(_.srcSpan.get.start).foreach { n =>
            val span = n.srcSpan.get.showShort()
            val rankedList = map(n).distr.zipWithIndex
              .map {
                case ((p, ty), i) => {
                  val acc = "%.2f".format(p * 100)
                  s"[${i + 1}]($acc%) ${ty.showSimple}"
                }
              }
              .mkString(", ")
            println(s"$span: $rankedList")
          }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    NeuralInference.checkOMP()

    val modelDir = LoadModel.newestModelDir
    val modelPath = modelDir / "model.serialized"

    val model = announced("Loading model") {
      readObjectFromFile[Model](modelPath)
    }

    val service = model.PredictionService(numOfThreads = 8, predictTopK = 5)
    printResult("Type Inference Service successfully started.")
    printResult(s"Current working directory: ${amm.pwd}")
    while (true) {
      print("Enter project path: ")
      System.out.flush()
      try {
        val line = scala.io.StdIn.readLine()
        require(line.strip().nonEmpty, "Specified path should not be empty.")
        val sourcePath = Path(line, amm.pwd)
        val results = service.predictOnProject(sourcePath, warnOnErrors = false)
        PredictionResults(results).prettyPrint()
      } catch {
        case e: Throwable =>
          println(s"Got exception: ${e.getMessage}")
          e.printStackTrace(System.out)
      }
    }
  }
}
