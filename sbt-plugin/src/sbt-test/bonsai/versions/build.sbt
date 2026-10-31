ThisBuild / scalaVersion := "3.7.9"

lazy val checkLine = taskKey[Unit]("The compiler plugin is the one of the minor line.")
lazy val checkNone = taskKey[Unit]("There is no compiler plugin.")

lazy val root = project.in(file("."))
  .settings(
    checkLine := {
      val plugin = libraryDependencies.value.filter(_.name == "bonsai-plugin")
      assert(plugin.map(_.crossVersion) == Seq(CrossVersion.constant("3.7")), plugin.toString)
      assert(scalacOptions.value.exists(_.startsWith("-P:bonsai:mode=")))
    },
    checkNone := {
      assert(!libraryDependencies.value.exists(_.name == "bonsai-plugin"))
      assert(!scalacOptions.value.exists(_.startsWith("-P:bonsai:")))
    }
  )
