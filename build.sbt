import sbt.Keys.version

lazy val root = project
  .in(file("."))
  .settings(
    name := "Slyce",
    version := "0.1",
    scalaVersion := "0.25.0-RC2",
    libraryDependencies ++=
      List(
        "org.scalaz" %% "scalaz-core" % "7.2.30",
        "org.scalactic" %% "scalactic" % "3.1.1",
        "org.scalatest" %% "scalatest" % "3.1.1" % "test",
        // "org.scalameta" %% "scalameta" % "4.3.10"
      ).map(_.withDottyCompat(scalaVersion.value)),
    libraryDependencies ++= List(
      "klib" %% "klib" % "0.1.0" from "file:///home/kalin/dev/projects/current/KLib/target/scala-0.24/klib_0.24-0.1.jar"
    )
  )
