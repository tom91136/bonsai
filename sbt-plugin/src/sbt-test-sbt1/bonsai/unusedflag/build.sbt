ThisBuild / scalaVersion := "3.3.5"
ThisBuild / bonsaiMode   := BonsaiMode.Dev

lazy val once = inputKey[Unit]("Check the compiler options hold each argument once")

lazy val root = project.in(file(".")).settings(
  scalacOptions += "-Wunused:all",
  once := {
    val options = (Compile / scalacOptions).value
    val wanted  = Def.spaceDelimited().parsed
    assert(wanted.forall(argument => options.count(_ == argument) == 1), options.mkString(" "))
  }
)
