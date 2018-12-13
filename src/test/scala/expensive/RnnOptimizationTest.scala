package expensive

import funcdiff._
import botkop.numsca
import botkop.numsca.Tensor
import botkop.{numsca => ns}
import funcdiff.Optimizers.{Adam, SGD}
import API._


class RnnOptimizationTest extends TestUtils {
  ns.rand.setSeed(11)

  {
    val nState = 5
    val inputs = (0 until nState).map { i => TensorExtension.oneHot(Seq(i), nState) }
    val targets = inputs.scanLeft(ns.zeros(1, nState))(_ + _).tail

    "LSTM" should "be able to sum a bit vector" in {

      val collection = new ParamCollection()
      val factory = LayerFactory(SymbolPath.empty, collection)

      val optimizer = Adam(learningRate = 0.1)

      val errors = for (i <- 0 until 200) yield {
        val initH: CompNode = collection.getVar(SymbolPath.empty / 'initH) {
          Tensor(Array.fill(nState)(0.0)).reshape(1, nState)
        }

        val initC: CompNode = collection.getVar(SymbolPath.empty / 'initC) {
          Tensor(Array.fill(nState)(0.0)).reshape(1, nState)
        }

        val states = inputs.scanLeft(initH -> initC) { case ((h, c), input) =>
          factory.lstm('LSTM)((h, c), input)
        }
        val error = sum(total(states.zip(targets).map { case ((h, _), t) => square(h - t): CompNode }))
        //      val outputs = states.map{x => x ~> factory.linear('GruOutput, 1) }

        optimizer.minimize(error, collection.allParams, weightDecay = Some(1e-4))
        val e = error.value.squeeze()
        println(s"error[$i]: $e")
        e
      }

      assert(errors.last < 0.01)
    }


    "GRU" should "be able to sum a bit vector" in {

      val collection = new ParamCollection()
      val factory = LayerFactory(SymbolPath.empty, collection)

      val optimizer = Adam(learningRate = 0.1)

      val errors = for (i <- 0 until 200) yield {
        val initState = collection.getVar(SymbolPath.empty / 'initState) {
          Tensor(Array.fill(nState)(0.0)).reshape(1, nState)
        }

        val states = inputs.zip(targets).scanLeft(initState: CompNode) { case (s, (input, t)) =>
          factory.gru('TestGRU)(s, input)
        }
        val error = sum(total(states.zip(targets).map { case (s, t) => square(s - t): CompNode }))
        //      val outputs = states.map{x => x ~> factory.linear('GruOutput, 1) }

        optimizer.minimize(error, collection.allParams, weightDecay = Some(1e-4))
        val e = error.value.squeeze()
        println(s"error[$i]: $e")
        e
      }

      assert(errors.last < 0.01)
    }

  }


  "All RNN" should "be able to fit a sine wave" in {
    numsca.rand.setSeed(1)

    val nState = 4
    val constInput = Tensor(0).reshape(1,1)
    val timeSteps = 40

    val inputs = IS.fill(timeSteps)(constInput: CompNode)
    val collection = new ParamCollection()

    val targets = (0 until timeSteps).map{ i => Tensor(math.sin(i * 6 * math.Pi / timeSteps)): CompNode }

    val optimizer = Adam(learningRate = 0.01)
//    optimizer.printUpdates = true

    val factory = LayerFactory(SymbolPath.empty, collection)

    val errors = for(i <- 0 until 100) yield {
      val initState = collection.getVar(SymbolPath.empty / 'initState){ Tensor(Array.fill(nState)(0.0)).reshape(1, nState) }

      val states = inputs.scanLeft(initState: CompNode) {
        factory.gru('TestGRU)
      }
      val outputs = states.map{x => x ~> factory.linear('GruOutput, 1) }
      val error = sum(total(outputs.zip(targets).map { case (s, t) =>
        square(s - t): CompNode
      })) / timeSteps

      optimizer.minimize(error, collection.allParams, weightDecay = Some(1e-4))

      val e = error.value.squeeze()
      println(s"error[$i]: $e")
      e
    }

    assert(errors.last < 0.01)
  }

}