ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode   := BonsaiMode.Dev

lazy val root = project.in(file(".")).settings(
  Compile / scalacOptions += "-Werror",
  Test / scalacOptions += "-Werror"
)
