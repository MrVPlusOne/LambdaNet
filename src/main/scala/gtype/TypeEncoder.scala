package gtype

import funcdiff.API._
import funcdiff._
import botkop.numsca
import botkop.numsca.Tensor
import funcdiff.Optimizers.Adam
import gtype.TypeAliasGraph._

import scala.util.Random

case class TypeEncoding(labelMap: Map[Symbol, CompNode], typeMap: Map[Symbol, CompNode])

//noinspection TypeAnnotation
object FieldCombineMethod extends Enumeration {

  /** Sum all field encodings together */
  val Sum = Value

  /** Weighted average field encodings using an attention mechanism */
  val Attention = Value
}

//noinspection TypeAnnotation
object ArgsEncodingMethod extends Enumeration {

  /** Encode arg types and return type separately and combine them using a linear layer */
  val Separate = Value

  /** Use the return type as the initial state of an RNN, then feed all arg types to it */
  val Unified = Value
}

@SerialVersionUID(0)
case class EncoderParams(
    labelDim: Int,
    typeDim: Int,
    fieldDim: Int,
    fieldCombineMethod: FieldCombineMethod.Value = FieldCombineMethod.Attention,
    argsEncodingMethod: ArgsEncodingMethod.Value = ArgsEncodingMethod.Separate,
    updateWithRNN: Boolean = false,
    activationName: Symbol
)

object EncoderParams {

  def getActivation(activation: Symbol): CompNode => CompNode = {
    activation match {
      case 'leakyRelu =>
        x =>
          funcdiff.API.leakyRelu(x)
      case 'relu => funcdiff.API.relu
    }
  }

  def ofDimension(dim: Int) = {
    EncoderParams(
      labelDim = dim,
      typeDim = dim,
      fieldDim = dim,
      activationName = 'leakyRelu,
    )
  }

  val tiny = EncoderParams(
    labelDim = 20,
    typeDim = 20,
    fieldDim = 20,
    activationName = 'leakyRelu,
  )

  val small = EncoderParams(
    labelDim = 40,
    typeDim = 40,
    fieldDim = 40,
    activationName = 'leakyRelu,
  )

  val medium = EncoderParams(
    labelDim = 60,
    typeDim = 60,
    fieldDim = 60,
    activationName = 'leakyRelu,
  )

  val large = EncoderParams(
    labelDim = 80,
    typeDim = 80,
    fieldDim = 80,
    activationName = 'leakyRelu,
  )
}

class TypeEncoder(val encoderParams: EncoderParams,
                  val pc: ParamCollection = ParamCollection()) {

  import encoderParams._

  val typePath: SymbolPath = SymbolPath.empty / 'type
  val subtypePath: SymbolPath = SymbolPath.empty / 'subtype
  val activation: CompNode => CompNode = EncoderParams.getActivation(activationName)

  /**
    * Encodes a type aliasing graph into type vectors
    * @param symbolDefMap the type aliasing graph
    * @param iterations how many iterations of information propagation should be used
    * @param batchSize how many batches should be used.
    *                  In each batch, the field vectors are sampled with uniformly random directions.
    *                  But we want these vectors to change frequently enough during training to
    *                  prevent the encoding network from
    *                  overfitting to any particular set of field vectors.
    */
  def encode(symbolDefMap: Map[Symbol, TypeAliasing],
             iterations: Int,
             batchSize: Int): TypeEncoding = {
    import collection.mutable
    val labelMap = mutable.HashMap[Symbol, CompNode]()

    def getFieldLabel(label: Symbol): CompNode = {
      labelMap.getOrElseUpdate(
        label,
        const {
          numsca.concatenate(Vector.fill(batchSize) { TensorExtension.randomUnitVec(labelDim) },
                             axis = 0)
        }
      )
    }

    val anyInit =
      pc.getVar(typePath / 'anyInit)(numsca.randn(1, typeDim) * 0.01).repeat(batchSize, axis = 0)
    val fieldZeroInit = const(numsca.zeros(batchSize, fieldDim))
    val typeInit =
      pc.getVar(typePath / 'typeInit)(numsca.randn(1, typeDim) * 0.01).repeat(batchSize, axis = 0)
    val funcArgInit: CompNode =
      pc.getVar(typePath / 'funcArgInit)(numsca.rand(1, typeDim) * 0.01).repeat(batchSize, axis = 0)

    val attentionTypeKernel = pc.getVar(typePath / 'attentionTypeKernel)(
      numsca.rand(typeDim, typeDim) * 0.001
    )
    val attentionFieldKernel = pc.getVar(typePath / 'attentionFieldKernel)(
      numsca.rand(fieldDim, typeDim) * 0.001
    )
    val attentionVec = pc
      .getVar(typePath / 'attentionVec)(numsca.rand(1, 2 * fieldDim) * 0.01)
      .t

    var typeMap: Map[Symbol, CompNode] = symbolDefMap.keys
      .map { tyName =>
        tyName -> typeInit
      }
      .toMap
      .updated(AnyType.id, anyInit)

    for (_ <- 0 until iterations) {
      val newTypeMap = symbolDefMap.map {
        case (tyName, typeDef) =>
          val typeFactory = LayerFactory(typePath, pc)
          import typeFactory._

          val aggregated = typeDef match {
            case FuncAliasing(argTypes, returnType) =>
              argsEncodingMethod match {
                case ArgsEncodingMethod.Separate =>
                  val argsEncoding = argTypes
                    .map(typeMap)
                    .foldLeft(funcArgInit)(gru('FuncArgsGru))
                  val x = argsEncoding.concat(typeMap(returnType), axis = 1)
                  activation(linear('FuncDefLinear, nOut = fieldDim)(x))
                case ArgsEncodingMethod.Unified =>
                  val xs = argTypes.map(typeMap) :+ typeMap(returnType)
                  xs.foldRight(funcArgInit)(gru('FuncArgsGru))
              }

            case ObjectAliasing(fields) =>
              val fieldEncodings = fields.map {
                case (fieldName, fieldType) =>
                  val x = getFieldLabel(fieldName)
                    .concat(typeMap(fieldType), axis = 1)
                  activation(linear('ContainsFieldLinear, nOut = fieldDim)(x))
              }.toVector
              fieldCombineMethod match {
                case FieldCombineMethod.Sum =>
                  if (fieldEncodings.nonEmpty) total(fieldEncodings)
                  else fieldZeroInit
                case FieldCombineMethod.Attention =>
                  // see the paper 'Graph Attention Networks'
                  val transformedThis = typeMap(tyName) dot attentionTypeKernel
                  assert(transformedThis.shape(1) == typeDim)
                  val transformedFields = fieldEncodings.map { _ dot attentionFieldKernel }
                  val allTransformed = transformedFields :+ transformedThis
                  val attentionLogits = allTransformed.map { ft =>
                    val w = transformedThis
                      .concat(ft, axis = 1)
                      .dot(attentionVec)
                    assert(w.shape.product == batchSize)
                    leakyRelu(w, 0.2)
                  }
                  val aWeights = softmax(concatN(attentionLogits, axis = 1))
                  total(allTransformed.indices.map { i =>
                    aWeights.slice(:>, i :> (i + 1)) * allTransformed(i)
                  })
              }
          }

          val newTypeEnc = if (updateWithRNN) {
            gru('UpdateGRU)(
              state = typeMap(tyName),
              input = aggregated
            )
          } else aggregated
          tyName -> newTypeEnc
      }

      typeMap = newTypeMap.updated(AnyType.id, anyInit)
    }
    TypeEncoding(labelMap.toMap, typeMap)
  }

  def subtypePredict(encoding: TypeEncoding, typePairs: Seq[(Symbol, Symbol)]): CompNode = {
    val (t1s, t2s) = typePairs.unzip
    val t1 = concatN(t1s.map(encoding.typeMap).toVector, axis = 0)
    val t2 = concatN(t2s.map(encoding.typeMap).toVector, axis = 0)

    val modelFactory = LayerFactory(subtypePath, pc)
    import modelFactory._

    val classifierNeurons = 40 // lets fix this to 40

    val layer1 = activation(
      linear('linear1, nOut = classifierNeurons)(t1.concat(t2, axis = 1).concat(t1 * t2, axis = 1))
    )
    activation(linear('linear2, nOut = 2)(layer1))

//    linear('simpleLinear, nOut = 2)((t1 * t2).concat(t1 + t2, axis = 1).concat(t2 - t1, axis = 1))
  }

  def saveToPath(dir: ammonite.ops.Path, name: String): Unit ={
    pc.saveToFile((dir / s"$name.pc").toIO)
    ParamCollection.saveObjectToFile((dir / s"$name.params").toIO)(encoderParams)
  }

}

object TypeEncoder {

  def readEncoderFromFiles(dir: ammonite.ops.Path, name: String): TypeEncoder ={
    val pc = ParamCollection.fromFile((dir / s"$name.pc").toIO)
    val params = ParamCollection.readObjectFromFile[EncoderParams]((dir / s"$name.params").toIO)
    new TypeEncoder(params, pc)
  }

  def main(args: Array[String]): Unit = {
    val experimentName = "quickTest"
    println(s"Experiment Name: $experimentName")

    import ammonite.ops._
    val loggerDir = pwd / 'results / experimentName
    val fileLogger = new FileLogger(loggerDir / "log.txt", printToConsole = true)
    require(!exists(loggerDir), s"The directory $loggerDir already exists!")
    mkdir(loggerDir)
    import fileLogger.{println, print}

    def describe[T](name: String)(x: T): T = {
      println(s"$name: $x")
      x
    }

    implicit val random: Random = new Random()

    val encParams = describe("Encoder params")(EncoderParams.ofDimension(10))
    val encoder = new TypeEncoder(encParams)

    val encodingIterations = describe("encodingIterations")(10)
    val trainingEncodingBatch = describe("trainingEncodingBatch")(1)

    val optimizer = describe("optimizer")(Adam(learningRate = 0.002))

    val posVec = Tensor(1, 0).reshape(1, -1)
    val negVec = Tensor(0, 1).reshape(1, -1)
    def forwardPredict(symbolDefMap: Map[Symbol, TypeAliasing],
                       posExamples: Seq[(Symbol, Symbol)],
                       negExamples: Seq[(Symbol, Symbol)],
                       encodingBatch: Int) = {
      val target = numsca.concatenate(Vector.fill(posExamples.length * encodingBatch)(posVec)
                                        ++ Vector.fill(posExamples.length * encodingBatch)(negVec),
                                      axis = 0)

      val typeEncoding = encoder.encode(symbolDefMap, encodingIterations, encodingBatch)
      val examples = posExamples ++ negExamples
      val predictions = encoder.subtypePredict(typeEncoding, examples)
      (target, predictions)
    }

    import TrainingTypeGeneration.augmentWithRandomTypes

    def generateTestData(baseContext: TypeContext, sampleNum: Int)
      : (Map[Symbol, TypeAliasing], List[(Symbol, Symbol)], List[(Symbol, Symbol)]) = {
      val context = augmentWithRandomTypes(baseContext, sampleNum)
      val symbolDefMap = typeContextToAliasings(context)
      println(s"data set graph size: " + symbolDefMap.size)
      val (reflexivity, posRelations, negRelations) = typeAliasingsToGroundTruths(symbolDefMap)
      println(
        s"pos relations: ${posRelations.length}, neg relations: ${negRelations.length}, " +
          s"reflexivity: ${reflexivity.length}"
      )

      val posData = random.shuffle(posRelations).take(250)
      val reflData = random.shuffle(reflexivity).take(50)
      val negData = random.shuffle(negRelations).take(300)
      (symbolDefMap, posData ++ reflData, negData)
    }

    describe("dev set name")("JSCore200")
    val devData = generateTestData(JSExamples.typeContext, 200)

    describe("test set name")("JSReal200")
    val testData = generateTestData(JSExamples.realWorldExamples, 200)

    // === training loop ===
    for (epoch <- 0 until 1000) {
      val trainingSet = {
        val (name, context) = SimpleMath.randomSelect(
          Vector("Train" -> TrainingTypeGeneration.trainingContext,
                 "JSCore" -> JSExamples.typeContext))
        val newTypeNum = 100 + random.nextInt(101)
        s"$name$newTypeNum" -> augmentWithRandomTypes(context, newTypeNum)
      }

      val (dataSetName, context) = trainingSet
      val symbolDefMap = typeContextToAliasings(context)

      val (reflexivity, posRelations, negRelations) = typeAliasingsToGroundTruths(symbolDefMap)

      val posData = random.shuffle(posRelations)
      val reflData = random.shuffle(reflexivity).take(posData.length / 4)
      val negData = random.shuffle(negRelations).take(posData.length + reflData.length)

      val (target, predictions) =
        forwardPredict(symbolDefMap, posData ++ reflData, negData, trainingEncodingBatch)

      val loss = mean(crossEntropyOnSoftmax(predictions, target))

      val accuracy =
        API.accuracy(predictions.value, target(:>, 1).data.map(_.toInt), trainingEncodingBatch)._1
      println(
        s"[$epoch] $dataSetName \t loss: ${loss.value},\t accuracy = %.4f".format(accuracy)
      )

      optimizer.minimize(loss, encoder.pc.allParams, weightDecay = Some(1e-4))

      if ((epoch != 0) && (epoch % 25 == 0)) {
        val testEncodingBatch = 5

        def evaluate(dataName: String,
                     dataSet: (Map[Symbol, TypeAliasing],
                               List[(Symbol, Symbol)],
                               List[(Symbol, Symbol)])): Unit = {
          val (symbolDefMap, posData, negData) = dataSet
          val (target, predictions) =
            forwardPredict(symbolDefMap, posData, negData, testEncodingBatch)
          val loss = mean(crossEntropyOnSoftmax(predictions, target))

          val (accuracy, (correct, wrong)) = {
            API.accuracy(predictions.value, target(:>, 1).data.map(_.toInt), testEncodingBatch)
          }
          println(s"[$epoch] $dataName \t loss: ${loss.value},\t accuracy = %.4f".format(accuracy))

        }

        encoder.saveToPath(loggerDir, s"model$epoch")
        evaluate("dev set ", devData)
        evaluate("test set", testData)
      }
    }
  }
}
