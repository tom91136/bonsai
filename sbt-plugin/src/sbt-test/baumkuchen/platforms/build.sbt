ThisBuild / scalaVersion   := "3.9.0"
ThisBuild / baumkuchenMode := BaumkuchenMode.CI

// A JVM project and its Native twin compile the same sources, and each a platform directory of its own.
lazy val app = project.settings(Compile / unmanagedSourceDirectories ++= Seq(file("shared"), file("jvm")))
lazy val appNative =
  project.enablePlugins(ScalaNativePlugin).settings(Compile / unmanagedSourceDirectories ++= Seq(file("shared"), file("native")))
lazy val root      = project.in(file(".")).aggregate(app, appNative)

lazy val checkManifest = taskKey[Unit]("Fail unless the analysis manifest marks the Native twin's output as a platform.")
checkManifest := {
  val manifest = IO.read((root / target).value / "baumkuchen" / "manifest.tsv").linesIterator.toList
  if (!manifest.exists(line => line.startsWith("output") && line.contains("appNative")) || !manifest.contains("platform\tappNative"))
    sys.error(s"the Native twin is not a platform of the analysis:\n${manifest.mkString("\n")}")
}
