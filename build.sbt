name := "mcmcScala"

version := "v0.2"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.0",
  "org.scalanlp" %% "breeze" % "latest.integration",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)
