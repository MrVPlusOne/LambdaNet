package lambdanet

import ammonite.ops.{Path, pwd}

object LoadModel {
  val newestModelDir: Path = pwd / "models" / "NewModelFormat-epoch24"

  def main(args: Array[String]): Unit = {
    val modelDir = newestModelDir
    val model = announced("load model..."){
      SM.readObjectFromFile[Model](modelDir / "model.serialized")
    }

    announced("save model as the new format"){
      SM.saveObjectToFile(modelDir / "model-copy.serialized")(model)
    }

//    val paramPath = modelDir / "params.serialized"
//    val modelCachePath = modelDir / "model.serialized"
//
//    val sfPath = (modelDir / "params-sf.serialized").toIO
//    val pc = announced("read pc special format"){
//      ParamCollection.fromFile(sfPath)
//    }
//
//    val normalPath = (modelDir / "params.serialized").toIO
//    announced("Save pc as normal format") {
//      saveObjectToFile(normalPath)(pc)
//    }
//
//    announced("read pc normal format"){
//      readObjectFromFile[ParamCollection](normalPath)
//    }

//    announced("save pc as pc_format"){
//      pc.saveToFile(pwd/'data/"pc.pcformat")
//    }

//    announced("load pc from pc_format"){
//      ParamCollection.fromFile(pwd/'data/"pc.pcformat")
//    }

  }
}
