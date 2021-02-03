package lambdanet

import ammonite.ops.{RelPath, pwd}
import funcdiff.{ParamCollection, SymbolPath}
import lambdanet.utils.PrecomputeResults

object LoadModel {
  def main(args: Array[String]): Unit = {

    announced("load model"){
      SM.readObjectFromFile[Model]("running-results/LambdaNet-GAT1-fc2-noSig-decay-with_any-lossAgg_sum-8/saved/initial/model.serialized")
    }

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
