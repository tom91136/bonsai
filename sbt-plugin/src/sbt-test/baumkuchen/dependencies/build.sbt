ThisBuild / scalaVersion := "3.9.0"

lazy val core = project
lazy val app  = project.dependsOn(core)
lazy val root = project.in(file(".")).aggregate(core, app)
