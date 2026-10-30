ThisBuild / scalaVersion := "3.9.0"

lazy val gitInit = taskKey[Unit]("Make the build a git repository, since the text rules read the files git sees")

lazy val app = project.in(file(".")).settings(
  gitInit := Def.uncached {
    val _ = scala.sys.process.Process(Seq("git", "init", "-q"), baseDirectory.value).!
  }
)
