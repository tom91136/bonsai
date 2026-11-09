ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

lazy val core = project
lazy val root = project.in(file(".")).aggregate(core)
