addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"  % Versions.scalafix)
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"  % "2.6.2")
addSbtPlugin("io.stryker-mutator" % "sbt-stryker4s" % "1.1.1")

// The build reads .bonsai.conf, writes its manifest and keeps cached semanticdb writable through the sources of the
// sbt plugin and of the reader it shares.
Compile / unmanagedSources ++= {
  val root      = baseDirectory.value.getParentFile
  val sbtPlugin = root / "sbt-plugin/src/main/scala/net/kurobako/bonsai/sbt"
  val sbt2      = root / "sbt-plugin/src/main/scala-3/net/kurobako/bonsai/sbt"
  Seq(
    root / "configuration/src/main/scala/net/kurobako/bonsai/configuration/BonsaiConfiguration.scala",
    sbtPlugin / "BonsaiConfiguration.scala",
    sbtPlugin / "Fixtures.scala",
    sbtPlugin / "ManifestLines.scala",
    sbt2 / "Compat.scala",
    sbt2 / "WritableSemanticdb.scala"
  )
}
