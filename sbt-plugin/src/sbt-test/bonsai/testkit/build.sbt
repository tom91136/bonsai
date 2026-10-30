ThisBuild / scalaVersion   := "3.9.0"
ThisBuild / bonsaiMode := BonsaiMode.CI
ThisBuild / bonsaiErrors ++= Set("test-only", "test-only-member")

// A project whose main code only other projects' tests use, as test fixtures are.
lazy val kit  = project.settings(publish / skip := true)
lazy val app  = project.dependsOn(kit % "test->compile")
lazy val root = project.in(file(".")).aggregate(kit, app)
