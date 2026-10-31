package net.kurobako.bonsai.plugin

import munit.FunSuite

class ImpliedSuite extends FunSuite {
  private def implied(source: String): List[String] = Warnings.tagged(Warnings.of(source), "implied-condition")

  private val always = "this condition is always true here"
  private val never  = "this condition is always false here"

  private val decided = List(
    "def nested(name: String, strict: Boolean): Int = if (strict) { if (name.isEmpty) 0 else if (strict) 1 else 2 } else 3" ->
      always,
    "def chained(a: Boolean, b: Boolean): Int = if (a) 1 else if (b) 2 else if (a) 3 else 4"        -> never,
    "def conjunct(x: Int, y: Int): Int = if (x > 0 && y > 0) { if (x > 0) 1 else 2 } else 3"        -> always,
    "def negated(ready: Boolean): Int = if (!ready) 0 else if (ready) 1 else 2"                     -> always,
    "def defined(value: Option[Int]): Int = if (value.isEmpty) 0 else if (value.nonEmpty) 1 else 2" -> always,
    "def inequal(left: String, right: String): Int = if (left != right) { if (left == right) 1 else 2 } else 3" -> never
  )

  decided.foreach((definition, message) =>
    test(s"a condition an enclosing if decided: ${definition.takeWhile(_ != '(').stripPrefix("def ")}") {
      assertEquals(implied(s"object Implied {\n  $definition\n}"), List(message))
    }
  )

  test("conditions the enclosing ifs did not decide, or that could have changed") {
    val found = implied(
      """import scala.collection.mutable.ArrayBuffer
        |object Implied {
        |  def sequential(a: Boolean): Int = { val x = if (a) 1 else 2; if (a) x else 0 }
        |  def disjunct(a: Boolean, b: Boolean): Int = if (a || b) { if (a) 1 else 2 } else 3
        |  def different(a: Int): Int = if (a > 0) { if (a > 1) 1 else 2 } else 3
        |  def variable(): Int = {
        |    var open = true
        |    if (open) { open = false; if (open) 1 else 2 } else 3
        |  }
        |  def mutated(buffer: ArrayBuffer[Int]): Int = if (buffer.isEmpty) { buffer += 1; if (buffer.isEmpty) 1 else 2 } else 3
        |  def call(next: () => Boolean): Int = if (next()) { if (next()) 1 else 2 } else 3
        |  def shadowed(n: Int, xs: List[Int]): List[Int] = if (n > 0) xs.map(n => if (n > 0) n else 0) else xs
        |  def local(a: Boolean): Int = if (a) { def f(): Int = if (a) 1 else 2; f() } else 3
        |  def literal(a: Boolean): Int = if (true) { if (true) 1 else 2 } else 3
        |  inline def inlined(a: Boolean): Int = if (a) { inline if (true) 1 else 2 } else 3
        |}""".stripMargin
    )
    assertEquals(found, Nil)
  }
}
