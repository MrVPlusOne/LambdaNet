package lambdanet

import lambdanet.TypeInferenceService.{ModelConfig, loadModel, newestModelDir}

object LoadModel {
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
//    val modelConfig = ModelConfig()
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


//    val pc = PrecomputeResults.model.architecture.layerFactory.paramCollection
//
//    announced("save pc as pc_format"){
//      pc.saveToFile(pwd/'data/"pc.pcformat")
//    }

//    announced("load pc from pc_format"){
//      ParamCollection.fromFile(pwd/'data/"pc.pcformat")
//    }

//    //    val allKeys = pc.paramMap.keys.toVector
//    val keyPath = (pwd/'data/"allKeys.serializable").toIO
//    val stringKeyPath = (pwd/'data/"allStringKeys.serializable").toIO
//
//    //    announced("save keys"){SM.saveObjectToFile(keyPath)(allKeys)}
//    val allKeys = announced("load keys"){SM.loadObjectFromFile[Vector[SymbolPath]](keyPath)}
//
//    val stringKeys = allKeys.map(_.toString)
//    announced("save string keys"){SM.saveObjectToFile(stringKeyPath)(stringKeys)}
//    val loaded = announced("load string keys"){SM.loadObjectFromFile[Vector[String]](stringKeyPath)}
//    announced("rebuild symbol path"){ loaded.map(SymbolPath.parse)}

  }
}
