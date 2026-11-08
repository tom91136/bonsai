ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Dev

lazy val checkManifest = taskKey[Unit]("Check the manifest has the shape every sbt line writes")

lazy val root = project.in(file(".")).settings(
  checkManifest := {
    val actual   = ManifestShape.of(IO.readLines(target.value / "bonsai" / "manifest.tsv"), baseDirectory.value)
    val expected = IO.readLines(file(sys.props("bonsai.shared")) / "manifest.shape")
    assert(actual == expected, actual.mkString("\n"))
  }
)
