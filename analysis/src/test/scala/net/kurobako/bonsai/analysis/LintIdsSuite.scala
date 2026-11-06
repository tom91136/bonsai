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

  private def suppresses(name: String, rule: String): Boolean =
    Finding.Rule.values.exists(candidate => candidate.id == rule && candidate.suppressedBy(LintId.resolve(name)))

  test("a suppression name suppresses the rule of the lint it names, however the name is written") {
    val cases = Vector(
      "bonsai:FallThrough"              -> "fallthrough-in-switch",
      "bonsai:MissingOverride"          -> "missing-override",
      "bonsai:SizeReplaceableByIsEmpty" -> "size-to-is-empty",
      "bonsai:unused"                   -> "unused-member",
      "bonsai:fallthrough"              -> "fallthrough-in-switch",
      "FallThrough"                     -> "fallthrough-in-switch",
      "fallthrough"                     -> "fallthrough-in-switch"
    )
    assertEquals(cases.filterNot((name, rule) => suppresses(name, rule)), Vector.empty[(String, String)])
  }

  test("a suppression name does not suppress a rule its lint does not name") {
    assert(!suppresses("bonsai:size-to-is-empty", "size-to-non-empty"))
  }
}
