ThisBuild / scalaVersion   := "3.9.0"
ThisBuild / bonsaiMode := BonsaiMode.CI

// A JVM project and its Native twin compile the same sources, and each a platform directory of its own.
lazy val app = project.settings(
  Compile / unmanagedSourceDirectories ++= Seq(file("shared"), file("jvm")),
  unmanagedBase := platformLibrary("jvm").getParentFile
)
lazy val appNative =
  project.enablePlugins(ScalaNativePlugin).settings(
    Compile / unmanagedSourceDirectories ++= Seq(file("shared"), file("native")),
    unmanagedBase := platformLibrary("native").getParentFile
  )
lazy val root      = project.in(file(".")).aggregate(app, appNative)

// Each platform has its own jar of one artifact.
def platformLibrary(platform: String): File = {
  val jar = file("target") / "libraries" / platform / "library.jar"
  IO.jar(Seq(), jar, new java.util.jar.Manifest, None)
  jar
}

lazy val checkManifest = taskKey[Unit]("Fail unless the analysis manifest marks the Native twin's output as a platform.")
checkManifest := {
  val manifest = IO.read((root / target).value / "bonsai" / "manifest.tsv").linesIterator.toList
  if (!manifest.exists(line => line.startsWith("output") && line.contains("appNative")) || !manifest.contains("platform\tappNative"))
    sys.error(s"the Native twin is not a platform of the analysis:\n${manifest.mkString("\n")}")
  def holds(project: String, platform: String) =
    manifest.exists(line => line.startsWith(s"classpath\t$project\t") && line.contains(s"/libraries/$platform/"))
  if (!holds("app", "jvm") || holds("app", "native") || !holds("appNative", "native") || holds("appNative", "jvm"))
    sys.error(s"a module's classpath is not its own:\n${manifest.mkString("\n")}")
}
