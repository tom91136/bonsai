package net.kurobako.bonsai.sbt

class ScalaVersionsSuite extends munit.FunSuite {

  test("a final release takes the compiler plugin of its minor line, whatever its patch") {
    assertEquals(ScalaVersions.plugin("3.3.9"), Some("3.3"))
    assertEquals(ScalaVersions.plugin("3.6.0"), Some("3.6"))
    assertEquals(
      ScalaVersions.plugin(BonsaiArtifacts.analysisScala),
      Some(BonsaiArtifacts.analysisScala.split('.').take(2).mkString("."))
    )
  }

  test("a patch older than the oldest the plugin of its line runs in takes none") {
    assertEquals(ScalaVersions.plugin("3.7.3"), None)
    assertEquals(ScalaVersions.plugin("3.7.4"), Some("3.7"))
    assertEquals(ScalaVersions.plugin("3.7.5"), Some("3.7"))
    assertEquals(ScalaVersions.plugin("3.3.3"), None)
    assertEquals(ScalaVersions.plugin("3.3.4"), Some("3.3"))
  }

  test("a line the plugin is not built for, or a release without final TASTy, takes none") {
    assertEquals(ScalaVersions.plugin("3.2.2"), None)
    assertEquals(ScalaVersions.plugin("3.99.0"), None)
    assertEquals(ScalaVersions.plugin("3.7.0-RC1"), None)
    assertEquals(ScalaVersions.plugin("3.8.0-RC1-bin-20250101-abc-NIGHTLY"), None)
    assertEquals(ScalaVersions.plugin("2.13.16"), None)
  }

  test("the analysis reads the TASTy of its own minor line and the older ones") {
    val newest = BonsaiArtifacts.analysisScala.split('.')(1).toInt
    assert(ScalaVersions.readable(BonsaiArtifacts.analysisScala))
    assert(ScalaVersions.readable("3.3.5"))
    assert(ScalaVersions.readable(s"3.$newest.99"))
    assert(!ScalaVersions.readable(s"3.${newest + 1}.0"))
    assert(!ScalaVersions.readable(s"3.$newest.0-RC1"))
    assert(!ScalaVersions.readable("2.13.16"))
  }

  test("RemoveUnused runs from the oldest release whose compiler takes -Wunused:all") {
    assert(!ScalaVersions.removeUnused("3.3.3"))
    assert(ScalaVersions.removeUnused("3.3.4"))
    assert(ScalaVersions.removeUnused("3.4.0"))
    assert(ScalaVersions.removeUnused(BonsaiArtifacts.analysisScala))
    assert(ScalaVersions.removeUnused("3.8.0-RC1"))
    assert(!ScalaVersions.removeUnused("2.13.16"))
  }

  test("ExplicitResultTypes is left out on the lines whose presentation compiler scalafix cannot load") {
    assert(!ScalaVersions.explicitResultTypes("3.4.0"))
    assert(!ScalaVersions.explicitResultTypes("3.4.3"))
    assert(ScalaVersions.explicitResultTypes("3.3.9"))
    assert(ScalaVersions.explicitResultTypes("3.5.2"))
    assert(ScalaVersions.explicitResultTypes(BonsaiArtifacts.analysisScala))
    assert(!ScalaVersions.explicitResultTypes("2.13.16"))
  }

  test("every line the plugin is built for is one the analysis reads") {
    assert(BonsaiArtifacts.lines.forall(line => ScalaVersions.readable(s"$line.0")), BonsaiArtifacts.lines.toString)
  }
}
