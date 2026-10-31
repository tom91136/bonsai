package net.kurobako.bonsai.sbt

import sbt.*

class ManifestLinesSuite extends munit.FunSuite {

  test("a library output adds its library line after the output line") {
    assertEquals(
      ManifestLines.outputs("api", "main", "/api/classes", library = true),
      Seq("output\tapi\tmain\t/api/classes", "library\tapi")
    )
  }

  test("a test output has no library line") {
    assertEquals(
      ManifestLines.outputs("api", "test", "/api/test-classes", library = false),
      Seq("output\tapi\ttest\t/api/test-classes")
    )
  }

  test("a fixture or test code is test scope") {
    assertEquals(ManifestLines.scope(Set("fixture"), "api", test = false), "main")
    assertEquals(ManifestLines.scope(Set("fixture"), "fixture", test = false), "test")
    assertEquals(ManifestLines.scope(Set.empty, "api", test = true), "test")
  }

  test("a test scope output has no library line even for a library project") {
    assertEquals(
      ManifestLines.outputs("fixture", "test", "/fixture/test-classes", library = true),
      Seq("output\tfixture\ttest\t/fixture/test-classes")
    )
  }

  test("sources, classpath and options name their project or entry") {
    assertEquals(ManifestLines.sources("api", "main", Seq("/api/A.scala")), Seq("source\tapi\tmain\t/api/A.scala"))
    assertEquals(ManifestLines.classpath(Seq("/lib/cats.jar")), Seq("classpath\t/lib/cats.jar"))
    assertEquals(ManifestLines.options("api", Seq("-deprecation")), Seq("option\tapi\t-deprecation"))
  }

  test("the rendered manifest ends with the root line and a newline") {
    assertEquals(
      ManifestLines.render(Seq("output\tapi\tmain\t/api/classes", "classpath\t/lib/cats.jar"), file("/build")),
      "output\tapi\tmain\t/api/classes\nclasspath\t/lib/cats.jar\nroot\t/build\n"
    )
  }
}
