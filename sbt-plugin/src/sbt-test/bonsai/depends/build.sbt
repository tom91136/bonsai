ThisBuild / scalaVersion := "3.9.0"

lazy val checkDepends = taskKey[Unit]("Check the manifest names the projects each project depends on")

lazy val core   = project.settings(exportJars := true)
lazy val app    = project.dependsOn(core).settings(exportJars := true)
lazy val checks = project.dependsOn(core % "test->compile")
lazy val tool   = project

lazy val root = project.in(file(".")).aggregate(core, app, checks, tool).settings(
  checkDepends := Def.uncached {
    val actual = IO.readLines(target.value / "bonsai" / "manifest.tsv").filter(_.startsWith("depends\t"))
    val expected = Seq("depends\tapp\tcore", "depends\tchecks\tcore")
    assert(actual == expected, actual.mkString("\n"))
  }
)
