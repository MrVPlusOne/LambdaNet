name := "LambdaNet"

version := "0.2"

organization in ThisBuild := "mrvplusone.github.io"
scalaVersion in ThisBuild := "2.12.7"

scalacOptions ++= Seq(
  "-feature",
  "-Ypartial-unification", // for using cats
  "-language:higherKinds",
//  "-deprecation"
)

// to make the classpath right
fork in run := true

val memoryLimit = {
  // for some reason, the memory usage keeps going up on the server
  if(System.getProperty("os.name") == "Mac OS X") 8 else 32
}

val nd4jBinary = {
  val useCuda = new File("configs/useCuda.txt").exists()
  if(useCuda) "nd4j-cuda-10.0" else "nd4j-native-platform"
}

javaOptions ++= Seq(s"-Xms2G", "-Xmx6G", s"-Dorg.bytedeco.javacpp.maxbytes=${memoryLimit}G",
  s"-Dorg.bytedeco.javacpp.maxphysicalbytes=${memoryLimit+8}G")

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
  "org.nd4j" % nd4jBinary % "1.0.0-beta3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.typelevel" %% "cats-core" % "2.0.0-M3",
  "org.typelevel" %% "cats-effect" % "2.0.0-M3",
  "com.github.nscala-time" %% "nscala-time" % "2.22.0",
  "com.lihaoyi" %% "upickle" % "0.7.5",
  "com.lihaoyi" %% "scalatags" % "0.7.0",
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
