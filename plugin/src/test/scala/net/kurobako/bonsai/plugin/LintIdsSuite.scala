package net.kurobako.bonsai.plugin

import munit.FunSuite
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.java.*

/** The ids of the checks of the compiler plugin, which must not be the ids of the lints of the Java code unless both
  * are the laws of collection chains, which the Java code has too.
  */
class LintIdsSuite extends FunSuite {

  test("the ids of the checks are not the ids of the lints of the Java code") {
    val lintIds = LintId.ids -- Laws.ids
    val clashes = Check.all.map(_.id).filter(lintIds.contains)
    assert(clashes.isEmpty, s"the checks ${clashes.mkString(", ")} have the ids of the lints")
  }

  test("the lints of the laws of chains in the Java code are laws of the compiler plugin") {
    val missing = LintId.chains.filterNot(id => Check.all.exists(_.id == id))
    assert(missing.isEmpty, s"the laws ${missing.mkString(", ")} are no checks")
  }
}
