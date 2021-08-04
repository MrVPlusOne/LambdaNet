package lambdanet

object ServerUtils {

  val serverPath = "utopia1:/mnt/nas/jiayi/withAny"
  def downloadPredictions(task: String, epoch: Int): String = {
    val remoteDir = s"$serverPath/$task/saved/epoch$epoch"
    val localDir = s"~/Downloads/LambdaNetDownloads/$task-predictions-epoch$epoch"

    s"""
       |mkdir $localDir;
       |scp -r $remoteDir/predictions $localDir/predictions;
       |scp $remoteDir/correct.html $localDir/correct.html;
       |scp $remoteDir/incorrect.html $localDir/incorrect.html;
       |scp -r $remoteDir/bootstrap $localDir/bootstrap;
       |""".stripMargin
  }

  def main(args: Array[String]): Unit = {
    println(downloadPredictions("LambdaNet-GAT1-fc2-decay-with_any-lossAgg_sum-8", 60))
  }
}
