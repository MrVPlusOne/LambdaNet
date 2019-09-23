name := "LambdaNet"

version := "0.3"

organization in ThisBuild := "mrvplusone.github.io"
scalaVersion in ThisBuild := "2.12.9"

scalacOptions ++= Seq(
  "-feature",
  "-Ypartial-unification", // for using cats
  "-language:higherKinds"
//  "-deprecation"
)

// to make the classpath right
fork in run := true

val runOnMac = System.getProperty("os.name") == "Mac OS X"
val memories = {
  val s = scala.io.Source.fromFile("configs/memory.txt")
  val r = s.getLines().toList.map(_.trim.toInt)
  s.close()
  r
}
val heapLimit = memories.head
val offHeapLimit = memories(1)
//val heapLimit = if(runOnMac) 8 else 16
//val offHeapLimit = if(runOnMac) 20 else 30

val nd4jBinary = {
  val useCuda = new File("configs/useCuda.txt").exists()
  if (useCuda) "nd4j-cuda-10.0-platform" else "nd4j-native-platform"
}

javaOptions ++= Seq(
  s"-Xms2G",
  s"-Xmx${heapLimit}G",
  s"-Dorg.bytedeco.javacpp.maxbytes=${offHeapLimit}G",
  s"-Dorg.bytedeco.javacpp.maxphysicalbytes=${offHeapLimit + heapLimit + 1}G"
)

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "2.0.4",
  "org.scalacheck" %% "scalacheck" % "1.14.0",
  "org.scalatest" %% "scalatest" % "3.0.3" % Test,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.lihaoyi" %% "ammonite-ops" % "1.0.3",
  "org.json4s" %% "json4s-native" % "3.6.3",
  "com.github.daddykotex" %% "courier" % "1.0.0", // for email notifications
  //  "be.botkop" %% "numsca" % "0.1.4",
  // for building numsca
  "org.nd4j" % nd4jBinary % "1.0.0-beta4",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.typelevel" %% "cats-core" % "2.0.0-M3",
  "org.typelevel" %% "cats-effect" % "2.0.0-M3",
  "com.github.nscala-time" %% "nscala-time" % "2.22.0",
  "com.lihaoyi" %% "upickle" % "0.7.5",
  "com.lihaoyi" %% "scalatags" % "0.7.0"
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")

val circeVersion = "0.10.0"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

// My tasks

val train = taskKey[Unit]("start training")
train :=
  (runMain in Compile).toTask(" lambdanet.train.TrainingLoop").value

val prepareRepos =
  taskKey[Unit]("parse and prepare data for training and evaluation")
prepareRepos :=
  (runMain in Compile).toTask(" lambdanet.PrepareRepos").value

TaskKey[Unit]("prepareAndTrain") := Def.sequential(prepareRepos, train).value
