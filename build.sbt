libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.2.30",
  "org.rogach" %% "scallop" % "3.4.0",
  "org.scalactic" %% "scalactic" % "3.1.1",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)

libraryDependencies ++= List(
  "klib" %% "klib" % "0.1.0" from "file:///home/kalin/dev/projects/current/KLib/target/scala-2.13/klib_2.13-0.1.jar",
  "jline" %% "jline" % "2.10.1" from "file:///home/kalin/dev/projects/current/Slyce/dependencies/jline2-2.10.1.jar"
)

name := "Slyce"
version := "0.1"
scalaVersion := "2.13.2"
