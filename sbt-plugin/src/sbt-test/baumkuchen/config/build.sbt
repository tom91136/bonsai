ThisBuild / scalaVersion := "3.9.0"

lazy val checkChecked = taskKey[Unit]("Check the refinement types the configuration names reach the compiler plugin")

lazy val app = project.in(file(".")).settings(
  checkChecked := Def.uncached {
    val options = scalacOptions.value
    assert(options.contains("-P:baumkuchen:checked=demo.where"), options.mkString(" "))
  }
)
