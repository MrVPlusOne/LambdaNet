package lambdanet

import ammonite.ops.Path
import ammonite.{ops => amm}
import amm.RelPath
import amm.pwd
import funcdiff.SimpleMath.readObjectFromFile
import lambdanet.train.{LossAggMode, TopNDistribution}
import lambdanet.translation.PredicateGraph
import lambdanet.utils.ModelFormatConversion

object TypeInferenceService {

  case class PredictionResults(
      map: Map[PredicateGraph.PNode, TopNDistribution[PredicateGraph.PType]],
      srcLines: Map[ProjectPath, Array[String]],
  ) {
    def sourceText(span: SrcSpan): String = {
      val SrcSpan((r1, c1), (r2, c2), file) = span
      if (r1 == r2){
        srcLines(file)(r1).substring(c1, c2)
      } else {
        srcLines(file)(r1).substring(c1) + " ..."
      }
    }

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
            println(s"$span '${sourceText(n.srcSpan.get)}': $rankedList")
          }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val modelDir = pwd / RelPath(
      "running-results/NewData-GAT1-fc2AnnotsSampling(0.0,0.81)--decay-lossAgg_sum-encodeSignature-6/saved/epoch40"
    )

    val modelPath = modelDir / "model.serialized"

    val model = announced("Loading model") {
      readObjectFromFile[Model](modelPath)
    }

    val service = model.PredictionService(numOfThreads = 8, predictTopK = 5)
    printResult("Type Inference Service successfully started.")
    printResult(s"Current working directory: ${amm.pwd}")
    while (true) {
      println("Enter project path: ")
      System.out.flush()
      try {
        val line = scala.io.StdIn.readLine()
        require(line.strip().nonEmpty, "Specified path should not be empty.")
        val sourcePath = Path(line, amm.pwd)
        val results = service.predictOnProject(
          sourcePath,
          predictAny = false,
          onlyPredictLibType = false,
          warnOnErrors = false
        ).prettyPrint()
      } catch {
        case e: Throwable =>
          println(s"Got exception: ${e.getMessage}")
          e.printStackTrace(System.out)
      }
    }
  }
}
