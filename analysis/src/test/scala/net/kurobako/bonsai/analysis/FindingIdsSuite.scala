package net.kurobako.bonsai.analysis

import munit.FunSuite
import scala.io.Source
import scala.util.Using

class FindingIdsSuite extends FunSuite {
  test("the ids the build reads from the sources are the rules of the analysis") {
    val read = Using.resource(Source.fromResource("META-INF/bonsai/findings.id"))(_.getLines().toSet)
    assertEquals(read, Finding.Rule.values.map(_.id).toSet)
  }
}
