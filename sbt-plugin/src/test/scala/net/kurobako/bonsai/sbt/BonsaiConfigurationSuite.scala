package net.kurobako.bonsai.sbt

import sbt.*

class BonsaiConfigurationSuite extends munit.FunSuite {

  private def library(conf: String, id: String): Option[Boolean] =
    IO.withTemporaryDirectory { directory =>
      val file = directory / ".bonsai.conf"
      IO.write(file, conf)
      BonsaiConfiguration.read(file).library(id)
    }

  test("a project without its own flag takes the build-level library") {
    assertEquals(library("library = true\nprojects.api.wildcards = 1\n", "api"), Some(true))
  }

  test("a project flag overrides the build-level library") {
    assertEquals(library("library = true\nprojects.api.library = false\n", "api"), Some(false))
  }

  test("no library flag at either level gives no decision") {
    assertEquals(library("projects.api.wildcards = 1\n", "api"), None)
  }
}
