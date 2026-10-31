package net.kurobako.bonsai.sbt

class FixturesSuite extends munit.FunSuite {

  test("a project that every dependant uses from its test configuration is a fixture") {
    assertEquals(
      Fixtures.fixtures(Seq("testSupport" -> Some("test"), "testSupport" -> Some("test"))),
      Set("testSupport")
    )
  }

  test("a project one dependant uses from its main configuration is not a fixture") {
    assertEquals(
      Fixtures.fixtures(Seq("lifting" -> Some("test"), "lifting" -> None, "testSupport" -> Some("test"))),
      Set("testSupport")
    )
  }

  test("a project no dependant uses is not a fixture") {
    assertEquals(Fixtures.fixtures(Nil), Set.empty[String])
  }
}
