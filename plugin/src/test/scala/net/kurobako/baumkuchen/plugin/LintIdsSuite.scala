package net.kurobako.baumkuchen.plugin

import munit.FunSuite
import net.kurobako.baumkuchen.java.JavaLint

/** The ids of the checks of the compiler plugin, which must not be the ids of the lints of the Java code. */
class LintIdsSuite extends FunSuite {

  test("the ids of the checks are not the ids of the lints of the Java code") {
    val lintIds = JavaLint.values.map(_.id).toSet
    val clashes = Check.values.map(_.id).filter(lintIds.contains)
    assert(clashes.isEmpty, s"the checks ${clashes.mkString(", ")} have the ids of the lints")
  }
}
