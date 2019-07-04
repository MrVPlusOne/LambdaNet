package lambdanet.train

import ammonite.ops.Path
import funcdiff.{Optimizer, ParamCollection}
import lambdanet._

object TrainingState {
  def fromFile(file: Path): TrainingState = {
    val map = SM
      .readObjectFromFile[List[(String, Any)]](file.toIO)
      .toMap
    val step = map("epoch").asInstanceOf[Int]
    val dimMessage = map("dimMessage").asInstanceOf[Int]
    val optimizer = map("optimizer").asInstanceOf[Optimizer]
    val iterationNum = map.getOrElse("iterationNum", 10).asInstanceOf[Int]
    val pcData = map("pcData")
      .asInstanceOf[ParamCollection.SerializableFormat]
    val pc = ParamCollection.fromSerializable(pcData)
    TrainingState(step, dimMessage, iterationNum, optimizer, pc)
  }

  def loadTrainingState(): TrainingState =
    announced("loadTrainingState") {
      val loadFromFile: Option[Path] =
        TrainingControl.restoreFromFile(consumeFile = true)

      loadFromFile
        .map { p =>
          announced("Loading training from file: " + p) {
            TrainingState.fromFile(p)
          }
        }
        .getOrElse(
          TrainingState(
            epoch0 = 0,
            dimMessage = 32,
            optimizer = Optimizer.Adam(learningRate = 1e-3),
            iterationNum = 4,
            pc = ParamCollection(),
          ),
        )
        .tap(println)
    }

}

case class TrainingState(
    epoch0: Int,
    dimMessage: Int,
    iterationNum: Int,
    optimizer: Optimizer,
    pc: ParamCollection,
) {
  def saveToFile(file: Path): Unit = {
    val toSave =
      List[(String, Any)](
        "epoch" -> epoch0,
        "dimMessage" -> dimMessage,
        "iterationNum" -> iterationNum,
        "optimizer" -> optimizer,
        "pcData" -> pc.toSerializable,
      )
    SM.saveObjectToFile(file.toIO)(toSave)
  }

  override def toString: String = {
    s"""TrainingState:
       |  epoch: $epoch0
       |  dimMessage: $dimMessage
       |  iterationNum: $iterationNum
       |  optimizer: $optimizer
       """.stripMargin
  }
}
