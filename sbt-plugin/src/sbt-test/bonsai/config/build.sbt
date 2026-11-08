ThisBuild / scalaVersion := "3.9.0"

lazy val checkChecked = taskKey[Unit]("Check the refinement types the configuration names reach the compiler plugin")

lazy val checkNesting = taskKey[Unit]("Check the limits of the nesting rule the configuration names")

lazy val checkGeneratedDisabled = taskKey[Unit]("Check the rules the configuration disables over generated code")

lazy val app = project.in(file(".")).settings(
  checkNesting := Def.uncached {
    val limits = bonsaiNesting.value
    assert(limits == Map("depth" -> 3, "size" -> 120), limits.toString)
  },
  checkGeneratedDisabled := Def.uncached {
    val disabled = bonsaiGeneratedDisabled.value
    assert(disabled == Set("duplicate-code", "*"), disabled.toString)
  },
  checkChecked := Def.uncached {
    val options = (Compile / scalacOptions).value
    assert(options.contains("-P:bonsai:checked=demo.where"), options.mkString(" "))
  }
)
