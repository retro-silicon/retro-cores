ThisBuild / organization := "retro"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.12"

val spinalVersion = "1.15.0"

lazy val root = (project in file("."))
  .settings(
    name := "retro-cores",
    libraryDependencies ++= Seq(
      "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion,
      "com.github.spinalhdl" %% "spinalhdl-lib"  % spinalVersion,
      compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion),
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "com.lihaoyi" %% "ujson" % "4.0.2" % Test
    ),
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:reflectiveCalls"),
    Compile / run / fork := true,
    // Sims write to ./simWorkspace; run them one at a time to keep Verilator builds predictable.
    Test / fork := true,
    Test / parallelExecution := false
  )
