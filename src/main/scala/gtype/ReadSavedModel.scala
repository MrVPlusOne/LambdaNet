package gtype

import ammonite.ops._
import botkop.numsca
import botkop.numsca.Tensor
import funcdiff.API.{:>, crossEntropyOnSoftmax, mean}
import funcdiff.{API, ParamCollection}
import gtype.TrainingTypeGeneration.augmentWithRandomTypes
import gtype.TypeAliasGraph.{
  TypeAliasing,
  typeAliasingsToGroundTruths,
  typeContextToAliasings
}

import scala.util.Random

object ReadSavedModel {

  def main(args: Array[String]): Unit = {
    val modelDir = pwd / 'results / "i10-d80"
    val modelName = "model700"
    val encoder = TypeEncoder.readEncoderFromFiles(modelDir, modelName)

    implicit val random = new Random()

    def generateTestData(
        baseContext: TypeContext,
        sampleNum: Int
    ): (Map[Symbol, TypeAliasing], List[(Symbol, Symbol)], List[(Symbol, Symbol)]) = {
      val context = augmentWithRandomTypes(baseContext, sampleNum)
      val symbolDefMap = typeContextToAliasings(context)
      val (reflexivity, posRelations, negRelations) = typeAliasingsToGroundTruths(
        symbolDefMap
      )

      val posData = random.shuffle(posRelations).take(250)
      val reflData = random.shuffle(reflexivity).take(50)
      val negData = random.shuffle(negRelations).take(300)
      (symbolDefMap, posData ++ reflData, negData)
    }

    val posVec = Tensor(1, 0).reshape(1, -1)
    val negVec = Tensor(0, 1).reshape(1, -1)
    def forwardPredict(
        symbolDefMap: Map[Symbol, TypeAliasing],
        posExamples: Seq[(Symbol, Symbol)],
        negExamples: Seq[(Symbol, Symbol)],
        encodingBatch: Int,
        encodingIterations: Int
    ) = {
      val target = numsca.concatenate(
        Vector.fill(posExamples.length * encodingBatch)(posVec)
          ++ Vector.fill(posExamples.length * encodingBatch)(negVec),
        axis = 0
      )

      val typeEncoding = encoder.encode(symbolDefMap, encodingIterations, encodingBatch)
      val examples = posExamples ++ negExamples
      val predictions = encoder.subtypePredict(typeEncoding, examples)
      (target, predictions)
    }

    def evaluate(
        iterations: Int,
        dataSet: (
            Map[Symbol, TypeAliasing],
            List[(Symbol, Symbol)],
            List[(Symbol, Symbol)]
        )
    ): Double = {
      val (symbolDefMap, posData, negData) = dataSet
      val (target, predictions) =
        forwardPredict(
          symbolDefMap,
          posData,
          negData,
          encodingBatch = 1,
          encodingIterations = iterations
        )

      val (accuracy, (correct, wrong)) = {
        API.accuracy(predictions.value, target(:>, 1).data.map(_.toInt), 1)
      }
      accuracy
    }

    for (testSize <- 100 to 800 by 100) {
      val testSets = Vector.fill(6) {
        generateTestData(JSExamples.realWorldExamples, testSize)
      }

      val acc = Statistics.mean(testSets.map { data =>
        evaluate(10, data)
      }: _*)
      println(s"{$testSize, $acc},")
    }
  }
}
