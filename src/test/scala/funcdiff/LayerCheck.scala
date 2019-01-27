package funcdiff

import botkop.numsca
import API._


class LayerCheck extends TestUtils {

  "Linear layer" should "have the right shape" in {
    val nInput = 5
    val input = const(numsca.randn(nInput,3))

    implicit val collection = new ParamCollection()
    val factory = LayerFactory(SymbolPath.empty, collection)
    import factory._

    val nOut = 10
    val y = linear('linear1, nOut)(input)
    y.shape shouldBe List(nInput, nOut)

    println(collection.paramMap)
  }

  "A param collection" should "be saved and restored" in {
    import java.io.File

    val nInput = 5
    val input = const(numsca.randn(nInput,3))

    val pc = new ParamCollection()
    val f1 = LayerFactory(SymbolPath.empty, pc)

    println("create y")
    val y = f1.linear('linear1, 4)(input) ~> f1.linear('linear2,6)

    val file = new File("testParamCollection.serialized")
    try {
      pc.saveToFile(file)

      val pc1 = ParamCollection.fromFile(file)
      val f2 = LayerFactory(SymbolPath.empty, pc1)
      println("Access")
      println{
        pc1.paramMap
      }
      println("End of access")
      val newParams = pc1.paramMap
      pc1.paramMap.foreach { case (pPath, param) =>
        assert {
          relError(newParams(pPath).node.value, param.node.value) < 1e-5
        }
        newParams(pPath).attributes shouldBe param.attributes
        newParams(pPath).path shouldBe param.path
      }
      println("create y2")
      val y2 = f2.linear('linear1, 4)(input) ~> f2.linear('linear2,6)

      val gradients = y2.backpropForParams(None)



      pc1.paramMap.foreach{ case (path, param) =>
        if(!gradients.contains(param.path))
          println(s"$param has no gradient")
      }

    }finally {
      file.delete()
    }
  }
}
