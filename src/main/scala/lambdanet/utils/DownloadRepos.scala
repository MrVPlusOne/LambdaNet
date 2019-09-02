package lambdanet.utils

import ammonite.ops._
import lambdanet._

import scala.util.Random

object DownloadRepos {

  def main(args: Array[String]): Unit = {
    downloadAllRepos()
  }

  def downloadAllRepos() = {
    implicit val workingDir: Path = pwd / up / "lambda-repos"
    val random = new Random(1023)
    val repoList =
      read(workingDir / "repo-SHAs.txt").split("\\n").toVector
    val testSet = repoList
      .pipe(random.shuffle(_))
      .drop(500)

    val totalSize = testSet.size
    var progress = 0
    testSet.foreach { line =>
      val Array(name, sha) = line.split("\\s")
      val newName = name.replace("/", "_")
      if (!exists(workingDir / "allRepos" / newName))
        try {
          %(
            'git,
            "clone",
            "-q",
            s"https://github.com/$name",
            s"allRepos/$newName"
          )
          %(
            'git,
            "-C",
            s"allRepos/$newName",
            "reset",
            "--hard",
            sha
          )
        } catch {
          case ex: InteractiveShelloutException =>
            printWarning(ex.getMessage)
        }

      synchronized { progress += 1 }
      printResult(s"progress: $progress / $totalSize")
    }
  }
}
