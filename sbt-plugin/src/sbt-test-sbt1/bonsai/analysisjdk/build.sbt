ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

def jdk(version: String): File = {
  val home = IO.createTemporaryDirectory
  val java = home / "bin" / "java"
  IO.write(
    java,
    s"""#!/bin/sh
       |case "$$1" in -version) echo 'openjdk version "$version"' >&2; exit 0 ;; esac
       |echo "$$@" >> "$$(dirname "$$0")/../invocations"
       |exec "${sys.props("java.home")}/bin/java" "$$@"
       |""".stripMargin
  )
  java.setExecutable(true)
  home
}

ThisBuild / bonsaiAnalysisJavaHome := jdk("21.0.1")

lazy val checkInvocation = taskKey[Unit]("Check the analysis ran on the JDK that was named")
lazy val fresh           = taskKey[Unit]("Change the sources, so the analysis is not served from the cache")

lazy val root = project.in(file(".")).settings(
  fresh := {
    val source = s"package demo\nobject Fresh { val id = ${System.nanoTime}L }\n"
    IO.write(baseDirectory.value / "accepted/src/main/scala/demo/Fresh.scala", source)
  },
  checkInvocation := {
    val invocations = (accepted / bonsaiAnalysisJavaHome).value / "invocations"
    assert(invocations.exists && IO.read(invocations).contains("net.kurobako.bonsai.analysis.CLI"), "the analysis did not run")
  }
)

lazy val refused  = project.in(file("refused")).settings(bonsaiAnalysisJavaHome := jdk("1.8.0_402"))
lazy val accepted = project.in(file("accepted"))
