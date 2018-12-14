package gtype

import ammonite.ops._
import funcdiff.ParamCollection

object ReadSavedModel {
  def main(args: Array[String]): Unit = {
    val modelDir = pwd / 'results / 'test
    val modelName = "model0"
    val encoder = TypeEncoder.readEncoderFromFiles(modelDir, modelName)
    println{
      encoder.encoderParams
    }
  }
}
