name := "Euler"

version := "0.1.0-SNAPSHOT"

organization := "org.smop"

scalaVersion := "2.9.0-1"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.5" % "test"
)

initialCommands in console := "import Euler._"
