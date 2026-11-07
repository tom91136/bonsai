ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode   := BonsaiMode.CI

lazy val flagged   = inputKey[Unit]("Check the compiler options hold each argument")
lazy val unflagged = inputKey[Unit]("Check the compiler options hold none of the arguments")
lazy val listed    = inputKey[Unit]("Check the scalafix configuration runs each rule")
lazy val unlisted  = inputKey[Unit]("Check the scalafix configuration runs none of the rules")

def configuration(base: File): String = IO.read(base / "bonsai" / "scalafix.conf")

lazy val app = project.in(file(".")).settings(
  flagged := {
    val options = scalacOptions.value
    val wanted  = Def.spaceDelimited().parsed
    assert(wanted.forall(options.contains), options.mkString(" "))
  },
  unflagged := {
    val options = scalacOptions.value
    val wanted  = Def.spaceDelimited().parsed
    assert(!wanted.exists(options.contains), options.mkString(" "))
  },
  listed := {
    val text   = configuration(target.value)
    val wanted = Def.spaceDelimited().parsed
    assert(wanted.forall(rule => text.contains(s"\"$rule\"")), text)
  },
  unlisted := {
    val text   = configuration(target.value)
    val wanted = Def.spaceDelimited().parsed
    assert(!wanted.exists(rule => text.contains(s"\"$rule\"")), text)
  }
)
