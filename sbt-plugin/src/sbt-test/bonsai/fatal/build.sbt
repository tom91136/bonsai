ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

lazy val flagged   = inputKey[Unit]("Check the compiler options hold each argument")
lazy val testing   = inputKey[Unit]("Check the compiler options of Test hold each argument")
lazy val undemoted = taskKey[Unit]("Check the compiler options demote none of bonsai's warnings")

lazy val root = project.in(file(".")).settings(
  scalacOptions := Seq("-feature", "-Werror"),
  flagged := {
    val options = (Compile / scalacOptions).value
    val wanted  = Def.spaceDelimited().parsed
    assert(wanted.forall(options.contains), options.mkString(" "))
  },
  testing := {
    val options = (Test / scalacOptions).value
    val wanted  = Def.spaceDelimited().parsed
    assert(wanted.forall(options.contains), options.mkString(" "))
  },
  undemoted := {
    val options = (Compile / compile / scalacOptions).value
    assert(!options.exists(_.startsWith("-Wconf:msg=\\[bonsai")), options.mkString(" "))
  }
)
