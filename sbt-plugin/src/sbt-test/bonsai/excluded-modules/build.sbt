ThisBuild / scalaVersion := "3.9.0"

lazy val checkExcluded = taskKey[Unit]("Check the listed modules get no compiler plugin and the others do")

lazy val broken = project
lazy val lib    = project
lazy val fine   = project.dependsOn(lib)

def injected(options: Seq[String]) = options.exists(_.contains("bonsai"))

lazy val app = project.in(file(".")).aggregate(broken, lib, fine).settings(
  checkExcluded := Def.uncached {
    assert(!injected((broken / Compile / scalacOptions).value), "broken")
    assert(!injected((lib / Compile / scalacOptions).value), "lib")
    assert(injected((fine / Compile / scalacOptions).value), "fine")
  }
)
