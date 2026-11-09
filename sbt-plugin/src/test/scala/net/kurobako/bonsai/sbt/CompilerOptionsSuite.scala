package net.kurobako.bonsai.sbt

class CompilerOptionsSuite extends munit.FunSuite {

  private val newest  = BonsaiArtifacts.analysisScala
  private val failing = "-Wconf:msg=\\[bonsai.[a-z0-9-]+\\]:e"

  test("the compiler fails on warnings with either of its two options") {
    assert(CompilerOptions.fatal(Seq("-feature", "-Werror")))
    assert(CompilerOptions.fatal(Seq("-Xfatal-warnings")))
    assert(!CompilerOptions.fatal(Seq("-feature", "-Wconf:any:e")))
  }

  test("a build with no unused option gets the options RemoveUnused reads") {
    assertEquals(CompilerOptions.unused(Seq("-feature"), Set.empty, newest), CompilerOptions.UnusedOptions)
    assert(CompilerOptions.UnusedOptions.contains("-Wunused:all"))
  }

  test("a build that asks for unused warnings itself gets none, whichever flag it uses") {
    assertEquals(CompilerOptions.unused(Seq("-Wunused:all"), Set.empty, newest), Nil)
    assertEquals(CompilerOptions.unused(Seq("-Wunused:imports"), Set.empty, newest), Nil)
    assertEquals(CompilerOptions.unused(Seq("-Wall"), Set.empty, newest), Nil)
  }

  test("an option the build already has is not added again") {
    val conf = CompilerOptions.UnusedOptions.filter(_.startsWith("-Wconf"))
    assert(conf.nonEmpty)
    assertEquals(CompilerOptions.unused(conf, Set.empty, newest), CompilerOptions.UnusedOptions.diff(conf))
  }

  test("the options are left out when the rule is disabled or the compiler does not take them") {
    assertEquals(CompilerOptions.unused(Nil, Set("RemoveUnused"), newest), Nil)
    assertEquals(CompilerOptions.unused(Nil, Set.empty, "3.3.3"), Nil)
    assertEquals(CompilerOptions.unused(Nil, Set.empty, "2.13.16"), Nil)
  }

  test("outside the strict mode a build that does not fail on warnings has none demoted") {
    assertEquals(CompilerOptions.routed(Seq("-feature"), strict = false, Set("acyclic"), newest), Nil)
  }

  test("outside the strict mode no finding of the compiler plugin fails a build that fails on warnings") {
    assertEquals(
      CompilerOptions.routed(Seq("-Werror"), strict = false, Set("acyclic"), newest),
      Seq("-Wconf:msg=\\[bonsai.[a-z0-9-]+\\]:i")
    )
  }

  test("a Scala release the compiler plugin cannot run in gets no routing options") {
    assertEquals(CompilerOptions.routed(Seq("-Werror"), strict = false, Set.empty, "3.0.2"), Nil)
    assertEquals(CompilerOptions.routed(Seq("-feature"), strict = true, Set("acyclic"), "3.0.2"), Nil)
  }

  test("in the strict mode every finding fails the build, whether or not it fails on warnings") {
    assertEquals(CompilerOptions.routed(Seq("-feature"), strict = true, Set.empty, newest), Seq(failing))
    assertEquals(CompilerOptions.routed(Seq("-Xfatal-warnings"), strict = true, Set.empty, newest), Seq(failing))
  }

  test("in the strict mode only the checks the build keeps as warnings are demoted from failing") {
    val expected = Seq(failing, "-Wconf:msg=\\[bonsai.acyclic\\]:i", "-Wconf:msg=\\[bonsai.same-type-arguments\\]:i")
    assertEquals(
      CompilerOptions.routed(
        Seq("-Werror"),
        strict = true,
        Set("same-type-arguments", "acyclic", "RemoveUnused"),
        newest
      ),
      expected
    )
    assertEquals(
      CompilerOptions.routed(
        Seq("-feature"),
        strict = true,
        Set("same-type-arguments", "acyclic", "RemoveUnused"),
        newest
      ),
      expected
    )
  }
}
