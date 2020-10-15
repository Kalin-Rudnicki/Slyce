import sbt.Keys.version

lazy val root = project
  .in(file("."))
  .settings(
    name := "Slyce",
    version := "0.1",
    scalaVersion := "2.13.2",
    libraryDependencies ++=
      List(
        "org.scalaz" %% "scalaz-core" % "7.2.30",
        "org.scalactic" %% "scalactic" % "3.1.1",
        "org.scalatest" %% "scalatest" % "3.1.1" % "test",
        "org.scalameta" %% "scalameta" % "4.3.10",
        "com.chuusai" %% "shapeless" % "2.3.3"
      )
  )
