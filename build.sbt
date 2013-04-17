name := "Loprog"

version := "0.1-SNAPSHOT"

mainClass in (Compile, run) := Some("ru.org.codingteam.loprog.Main")

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)

fork in run := true

connectInput in run := true

outputStrategy in run := Some(StdoutOutput)
