ThisBuild / scalaVersion   := "3.9.0"
ThisBuild / baumkuchenMode := BaumkuchenMode.CI

// A JVM project and its Native twin compile the same sources, and each a platform directory of its own.
lazy val app = project.settings(Compile / unmanagedSourceDirectories ++= Seq(file("shared"), file("jvm")))
lazy val appNative =
  project.enablePlugins(ScalaNativePlugin).settings(Compile / unmanagedSourceDirectories ++= Seq(file("shared"), file("native")))
lazy val root      = project.in(file(".")).aggregate(app, appNative)

lazy val checkManifest = taskKey[Unit]("Fail when the analysis manifest lists the Native twin's output.")
checkManifest := {
  val manifest = IO.read((root / target).value / "baumkuchen" / "manifest.tsv")
  if (manifest.linesIterator.exists(line => line.startsWith("output") && line.contains("appNative")))
    sys.error(s"the Native twin was analysed:\n$manifest")
}
