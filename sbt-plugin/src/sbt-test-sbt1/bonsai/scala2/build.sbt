ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

lazy val analysed = inputKey[Unit]("Check the manifest holds the output of each project")
lazy val skipped  = inputKey[Unit]("Check the manifest holds the output of none of the projects")
lazy val checkOptions = taskKey[Unit]("Only the projects the compiler plugin runs in take its options")

def projects(manifest: File): Set[String] =
  IO.readLines(manifest).map(_.split("\t")).collect { case Array("output", project, _, _) => project }.toSet

lazy val modern = project
lazy val legacy = project.settings(scalaVersion := "2.13.16")
lazy val both   = project.settings(crossScalaVersions := Seq("3.10.0", "2.13.16"))

lazy val root = project.in(file(".")).aggregate(modern, legacy, both).settings(
  checkOptions := {
    assert((modern / Compile / scalacOptions).value.exists(_.startsWith("-P:bonsai:mode=")))
    assert(!(legacy / Compile / scalacOptions).value.exists(_.startsWith("-P:bonsai:")))
  },
  analysed := {
    val found = projects(target.value / "bonsai" / "manifest.tsv")
    val wanted = Def.spaceDelimited().parsed
    assert(wanted.forall(found), found.mkString(", "))
  },
  skipped := {
    val found = projects(target.value / "bonsai" / "manifest.tsv")
    val wanted = Def.spaceDelimited().parsed
    assert(!wanted.exists(found), found.mkString(", "))
  }
)
