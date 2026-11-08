package net.kurobako.bonsai.sbt

class CompilerOptionsSuite extends munit.FunSuite {

  private val newest = BonsaiArtifacts.analysisScala

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

  test("a build that does not fail on warnings has none demoted") {
    assertEquals(CompilerOptions.demoted(Seq("-feature"), ci = false, Set("acyclic")), Nil)
    assertEquals(CompilerOptions.demoted(Seq("-feature"), ci = true, Set("acyclic")), Nil)
  }

  test("in Dev no finding of the compiler plugin fails a build that fails on warnings") {
    assertEquals(
      CompilerOptions.demoted(Seq("-Werror"), ci = false, Set("acyclic")),
      Seq("-Wconf:msg=\\[bonsai.[a-z0-9-]+\\]:i")
    )
  }

  test("in CI only the checks the build keeps as warnings are demoted") {
    assertEquals(CompilerOptions.demoted(Seq("-Xfatal-warnings"), ci = true, Set.empty), Nil)
    assertEquals(
      CompilerOptions.demoted(Seq("-Werror"), ci = true, Set("same-type-arguments", "acyclic", "RemoveUnused")),
      Seq("-Wconf:msg=\\[bonsai.acyclic\\]:i", "-Wconf:msg=\\[bonsai.same-type-arguments\\]:i")
    )
  }
}
