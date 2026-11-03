package net.kurobako.bonsai.analysis

import munit.FunSuite
import net.kurobako.bonsai.java.*

/** The ids of the lints of the Java code, which the analysis reports as findings of their own. */
class LintIdsSuite extends FunSuite {

  /** The ids of the lints of the Java code, which the analysis holds a rule of every one of them. */
  private val lintIds: Set[String] = LintId.ids

  /** The ids of the rules of the analysis. */
  private val ruleIds: Vector[String] = Finding.Rule.values.map(_.id)

  test("every lint of the Java code is reported as a finding of the analysis") {
    val missing = lintIds -- ruleIds
    assert(missing.isEmpty, s"the lints ${missing.toVector.sorted.mkString(", ")} are not reported")
  }

  test("the ids of the rules are distinct, so that the id of a lint is the id of no other rule") {
    assertEquals(ruleIds.distinct.size, ruleIds.size)
  }

  test("every rule of a lint has a lint of its own") {
    assertEquals(ruleIds.count(lintIds.contains), lintIds.size)
  }
}
