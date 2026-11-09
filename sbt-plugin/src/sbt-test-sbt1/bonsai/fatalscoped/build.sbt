ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

lazy val root = project.in(file(".")).settings(
  Compile / scalacOptions += "-Werror",
  Test / scalacOptions += "-Werror"
)
