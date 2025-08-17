val scala3Version = "3.7.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "orka",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions += "-Xprint:typer",
    logLevel := Level.Debug,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
