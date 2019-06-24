package lambdanet.utils

import ammonite.ops._
object ProcessAngular {
  def process(rootDir: Path): Unit = {
    for {
      dir <- ls(rootDir) if dir.isDir
      name = dir.last
    } {
      val file = dir / s"$name.d.ts"
      if (exists(file)) {
        val target = dir / "index.d.ts"
        assert(!exists(target))
        mv(file, target)
        val data =
          s"""
             |{
             |  "name": "@angular/$name"
             |}
          """.stripMargin
        write.over(dir / "package.json", data)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    process(
      Path("/Users/weijiayi/Programming/lambda-repos/declarations/angular"),
    )
  }

}
