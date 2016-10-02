name := "mcmcScala"

version := "v0.2"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.3", // not added by default > 2.11
  "org.scalanlp" %% "breeze" % "latest.integration",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)
