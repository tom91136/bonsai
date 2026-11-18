ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

val jdk: File = {
  val home = IO.createTemporaryDirectory
  val java = home / "bin" / "java"
  IO.write(
    java,
    s"""#!/bin/sh
       |case "$$1" in -version) echo 'openjdk version "21.0.1"' >&2; exit 0 ;; esac
       |echo "$$@" >> "$$(dirname "$$0")/../invocations"
       |exec "${sys.props("java.home")}/bin/java" "$$@"
       |""".stripMargin
  )
  java.setExecutable(true)
  home
}

ThisBuild / bonsaiAnalysisJavaHome := jdk

lazy val checkManifest    = taskKey[Unit]("Check the manifest task returns the manifest it wrote")
lazy val checkNotAnalysed = taskKey[Unit]("Check no analysis has run")
lazy val checkAnalysed    = taskKey[Unit]("Check the analysis has run")

lazy val root = project.in(file(".")).settings(
  checkManifest := {
    val manifest = bonsaiManifest.value
    assert(manifest == target.value / "bonsai" / "manifest.tsv", manifest.toString)
    assert(manifest.exists && IO.read(manifest).contains("demo"), "the manifest was not written")
  },
  checkNotAnalysed := {
    assert(!(bonsaiAnalysisJavaHome.value / "invocations").exists, "the analysis ran")
  },
  checkAnalysed := {
    val invocations = bonsaiAnalysisJavaHome.value / "invocations"
    assert(invocations.exists && IO.read(invocations).contains("net.kurobako.bonsai.analysis.CLI"), "the analysis did not run")
  }
)
