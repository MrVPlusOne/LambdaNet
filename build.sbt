name := "GradualTypingWithRL"

version := "0.1"


organization in ThisBuild := "mrvplusone.github.io"
scalaVersion in ThisBuild := "2.12.7"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "2.0.4",
  "org.scalacheck" %% "scalacheck" % "1.14.0",
  "org.scalatest" %% "scalatest" % "3.0.3" % Test,

  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.akka" %% "akka-actor" % "2.5.12",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.12" % Test,
  "com.lihaoyi" %% "ammonite-ops" % "1.0.3",
  "org.json4s" %% "json4s-native" % "3.6.3",

  //  "be.botkop" %% "numsca" % "0.1.4",
  // for building numsca
  "org.nd4j" % "nd4j-native-platform" % "1.0.0-beta3",
//  "org.nd4j" % "nd4j-cuda-10.0" % "1.0.0-beta3",
  
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
)