package net.kurobako.bonsai.sbt

import sbt.*

class ManifestLinesSuite extends munit.FunSuite {

  test("a library output adds its library line after the output line") {
    assertEquals(
      ManifestLines.outputs("api", "main", library = true, classes = "/api/classes"),
      Seq("output\tapi\tmain\t/api/classes", "library\tapi")
    )
  }

  test("a test output has no library line") {
    assertEquals(
      ManifestLines.outputs("api", "test", library = false, classes = "/api/test-classes"),
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
      ManifestLines.outputs("fixture", "test", library = true, classes = "/fixture/test-classes"),
      Seq("output\tfixture\ttest\t/fixture/test-classes")
    )
  }

  test("sources, classpath and options name their project or entry") {
    assertEquals(ManifestLines.sources("api", "main", Seq("/api/A.scala")), Seq("source\tapi\tmain\t/api/A.scala"))
    assertEquals(ManifestLines.classpath("api", Seq("/lib/cats.jar")), Seq("classpath\tapi\t/lib/cats.jar"))
    assertEquals(ManifestLines.options("api", Seq("-deprecation")), Seq("option\tapi\t-deprecation"))
  }

  test("resource lines name the resource directories that exist, once each") {
    IO.withTemporaryDirectory { directory =>
      val present = directory / "resources"
      IO.createDirectory(present)
      assertEquals(
        ManifestLines.resources(
          "api",
          "main",
          Seq(present.toString, (directory / "managed").toString, present.toString)
        ),
        Seq(s"resource\tapi\tmain\t$present")
      )
    }
  }

  test("the jdk line names the target home") {
    assertEquals(ManifestLines.jdk(file("/opt/jdk-25")), Seq("jdk\t/opt/jdk-25"))
  }

  test("the rendered manifest ends with the root line and a newline") {
    assertEquals(
      ManifestLines.render(Seq("output\tapi\tmain\t/api/classes", "classpath\tapi\t/lib/cats.jar"), file("/build")),
      "output\tapi\tmain\t/api/classes\nclasspath\tapi\t/lib/cats.jar\nroot\t/build\n"
    )
  }

  test("generated sources are named by the managed directory that holds them, or by their own file") {
    assertEquals(
      ManifestLines.generated(
        Seq("/b/target/src_managed/main", "/b/target/src_managed/test"),
        Seq(
          "/b/target/src_managed/main/A.scala",
          "/b/target/src_managed/main/demo/B.scala",
          "/b/target/emitted/C.scala",
          "/b/target/src_managed/main2/D.scala"
        )
      ),
      Seq(
        "generated\t/b/target/emitted/C.scala",
        "generated\t/b/target/src_managed/main",
        "generated\t/b/target/src_managed/main2/D.scala"
      )
    )
  }

  test("a build that generates nothing has no generated lines") {
    assertEquals(ManifestLines.generated(Seq("/b/target/src_managed/main"), Nil), Nil)
  }
}
