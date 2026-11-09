ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

def declared(project: Project) = project.settings(
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))
)

lazy val published = declared(project)
lazy val plain     = project
lazy val program   = project
lazy val homepaged = project.settings(homepage := Some(url("https://example.org/demo")))
lazy val developed = project.settings(
  developers := List(Developer("demo", "Demo", "demo@example.org", url("https://example.org")))
)
lazy val skipped = declared(project).settings(publish / skip := true)
lazy val optout  = declared(project)
lazy val optin   = project

lazy val check = inputKey[Unit]("Check that bonsaiLibrary holds the expected value in each project")

lazy val root = project.in(file(".")).settings(
  publish / skip := true,
  check          := {
    val libraries = Map(
      "published" -> (published / bonsaiLibrary).value,
      "plain"     -> (plain / bonsaiLibrary).value,
      "program"   -> (program / bonsaiLibrary).value,
      "homepaged" -> (homepaged / bonsaiLibrary).value,
      "developed" -> (developed / bonsaiLibrary).value,
      "skipped"   -> (skipped / bonsaiLibrary).value,
      "optout"    -> (optout / bonsaiLibrary).value,
      "optin"     -> (optin / bonsaiLibrary).value
    )
    val expected = Def.spaceDelimited().parsed.map(_.split("=")).map(pair => pair(0) -> pair(1).toBoolean).toMap
    assert(libraries == expected, libraries.toString)
  }
)
