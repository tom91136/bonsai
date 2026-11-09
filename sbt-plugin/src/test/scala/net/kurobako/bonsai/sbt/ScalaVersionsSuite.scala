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

  test("a Scala 2 release is told from a Scala 3 one") {
    assert(ScalaVersions.scala2("2.13.16"))
    assert(ScalaVersions.scala2("2.12.21"))
    assert(!ScalaVersions.scala2("3.3.5"))
    assert(!ScalaVersions.scala2(BonsaiArtifacts.analysisScala))
    assert(!ScalaVersions.scala2("3.99.0"))
  }

  test("the projects left out are named with their versions, in order, or not at all") {
    assertEquals(ScalaVersions.skipped(Nil), None)
    val note = ScalaVersions.skipped(Seq("legacy" -> "2.13.16", "both" -> "2.12.21")).get
    assert(note.startsWith("note: "), note)
    assert(note.endsWith("both (Scala 2.12.21), legacy (Scala 2.13.16)"), note)
  }

  test("RemoveUnused of a Scalafix that takes 3.10 for 3.1 is left out from that Scala on") {
    assert(!ScalaVersions.removeUnusedRule("3.10.0", "0.14.3"))
    assert(!ScalaVersions.removeUnusedRule("3.10.0", "0.14.7"))
    assert(ScalaVersions.removeUnusedRule("3.10.0", "0.14.8"))
    assert(ScalaVersions.removeUnusedRule("3.9.0", "0.14.3"))
    assert(!ScalaVersions.removeUnusedRule("3.3.3", "0.14.9"))
  }
}
