ThisBuild / scalaVersion := "3.3.9"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

lazy val root = project.in(file(".")).settings(
  Compile / scalacOptions += "-Werror"
)
