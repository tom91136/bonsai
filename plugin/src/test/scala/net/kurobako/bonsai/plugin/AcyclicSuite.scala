package net.kurobako.bonsai.plugin

import munit.FunSuite

class AcyclicSuite extends FunSuite {
  private def run(options: List[String], sources: Seq[(String, String)]): (List[String], List[String]) =
    Compilations.run(sources, options) { (result, _) =>
      (result.allErrors.map(_.message).toList, result.allWarnings.map(_.message).toList)
    }

  private def compile(sources: (String, String)*): List[String] = run(List("error=acyclic"), sources)._1

  private def assertCycle(sources: (String, String)*): Unit = {
    val errors = compile(sources*)
    assertEquals(errors.size, 1, errors.mkString("\n"))
    assert(errors.head.startsWith("Cyclic source dependency ("), errors.head)
    sources.foreach { case (name, _) => assert(errors.head.contains(name), errors.head) }
  }

  private val reported = Seq(
    "mutual references report both files and source lines" ->
      Seq("A.scala" -> "class A(val b: B)", "B.scala" -> "class B(val a: A)"),
    "nested type arguments contribute dependencies" ->
      Seq("A.scala" -> "class A(val b: List[Option[B]])", "B.scala" -> "class B(val a: List[A])"),
    "inheritance contributes dependencies" ->
      Seq("A.scala" -> "class A extends B", "B.scala" -> "trait B { def a: A }"),
    "constructor-only parameters contribute dependencies" ->
      Seq("A.scala" -> "class A(b: B)", "B.scala" -> "class B(a: A)"),
    "class type parameter bounds contribute dependencies" ->
      Seq("A.scala" -> "class A[T <: B]", "B.scala" -> "class B(val a: A[?])"),
    "term references and inferred result types contribute dependencies" ->
      Seq("A.scala" -> "object A { def b = B.n }", "B.scala" -> "object B { val n = 1; def a = A.b }"),
    "type aliases contribute dependencies" ->
      Seq("A.scala" -> "object A { type Item = B }", "B.scala" -> "class B { def a: A.type = A }"),
    "opaque type arguments retain their defining file" ->
      Seq(
        "A.scala" -> "object A { opaque type Id = Long; def b: B = ??? }",
        "B.scala" -> "class B(val id: Option[A.Id])"
      ),
    "selected givens contribute dependencies" ->
      Seq(
        "A.scala" -> "object A { import B.given; val n = 1; def value = summon[Ordering[Int]] }",
        "B.scala" -> "object B { given Ordering[Int] = Ordering.by[Int, Int](_ + A.n)(using Ordering.Int) }"
      ),
    "annotations contribute dependencies" ->
      Seq(
        "A.scala" -> "@B class A",
        "B.scala" -> "class B extends scala.annotation.StaticAnnotation { def owner: A = ??? }"
      ),
    "a normal dependency parallel to a macro expansion is still checked" ->
      Seq(
        "A.scala" -> "object A { val n = B.n; inline def value: Int = ${ B.impl } }",
        "B.scala" -> "import scala.quoted.*; object B { val n = 1; def impl(using Quotes): Expr[Int] = '{ A.n } }"
      ),
    "macro naming does not exempt an ordinary cycle" ->
      Seq("macros.scala" -> "class Macros(val b: B)", "B.scala" -> "class B(val a: Macros)"),
    "ordinary inline methods do not exempt a cycle" ->
      Seq(
        "A.scala" -> "object A { val n = 1; inline def value: Int = B.n }",
        "B.scala" -> "object B { def n: Int = A.n }"
      ),
    "cycles spanning three files are reported" ->
      Seq("A.scala" -> "class A(val b: B)", "B.scala" -> "class B(val c: C)", "C.scala" -> "class C(val a: A)"),
    "a whole mutually dependent group is reported, including overlapping cycles" ->
      Seq(
        "A.scala" -> "class A(val b: B)",
        "B.scala" -> "class B(val a: A, val c: C)",
        "C.scala" -> "class C(val b: B)"
      )
  )

  for ((name, sources) <- reported) test(s"reports a cycle: $name")(assertCycle(sources*))

  test("a cycle is shown as its shortest cycle, then the other files of the group") {
    val errors = compile(
      "A.scala" -> "class A(val b: B)",
      "B.scala" -> "class B(val a: A, val c: C)",
      "C.scala" -> "class C(val d: D)",
      "D.scala" -> "class D(val b: B)"
    )
    assertEquals(errors.size, 1, errors.mkString("\n"))
    val lines = errors.head.linesIterator.toVector
    assert(lines.head.startsWith("Cyclic source dependency (4 files), through the shortest cycle:"), errors.head)
    assert(lines(1).contains("A.scala:1 ->") && lines(1).endsWith("B.scala"), errors.head)
    assert(lines(2).contains("B.scala:1 ->") && lines(2).endsWith("A.scala"), errors.head)
    assert(lines(3).startsWith("and 2 more files on cycles with it:"), errors.head)
    assert(lines(4).endsWith("C.scala") && lines(5).contains("D.scala"), errors.head)
  }

  test("cycles are warnings unless the check is escalated") {
    val (errors, warnings) = run(Nil, Seq("A.scala" -> "class A(val b: B)", "B.scala" -> "class B(val a: A)"))
    assertEquals(errors, Nil)
    assertEquals(warnings.size, 1)
    assert(warnings.head.startsWith("Cyclic source dependency ("), warnings.head)
    assert(warnings.head.contains("A.scala:1 ->") && warnings.head.contains("B.scala:1 ->"), warnings.head)
    assert(warnings.head.endsWith("[bonsai:acyclic]"), warnings.head)
  }

  test("unknown checks fail") {
    val (errors, _) = run(List("error=bogus", "disable=other"), Seq("A.scala" -> "class A"))
    assertEquals(errors.sorted, List("unknown bonsai check: bogus", "unknown bonsai check: other"))
  }

  test("unknown checker options still fail") {
    val (errors, _) = run(List("ignored"), Seq("A.scala" -> "class A"))
    assert(errors.exists(_.contains("unknown bonsai option")), errors.mkString("\n"))
  }

  test("cycles through actual macro expansion are exempt regardless of names") {
    assertEquals(
      compile(
        "A.scala" -> "object A { val n = 1; inline def value: Int = ${ B.impl } }",
        "B.scala" -> "import scala.quoted.*; object B { def impl(using Quotes): Expr[Int] = '{ A.n } }"
      ),
      Nil
    )
  }

  test("a longer cycle crossing a macro expansion is exempt") {
    assertEquals(
      compile(
        "A.scala" -> "object A { val n = 1; inline def value: Int = ${ B.impl } }",
        "B.scala" -> "import scala.quoted.*; object B { def impl(using Quotes): Expr[Int] = C.impl }",
        "C.scala" -> "import scala.quoted.*; object C { def impl(using Quotes): Expr[Int] = '{ A.n } }"
      ),
      Nil
    )
  }

  test("an ordinary subcycle remains checked when its group also contains a macro") {
    val errors = compile(
      "A.scala" -> "object A { val n = 1; inline def value: Int = ${ B.impl } }",
      "B.scala" ->
        "import scala.quoted.*; object B { def impl(using Quotes): Expr[Int] = '{ A.n }; def c = C.n; val n = 1 }",
      "C.scala" -> "object C { def n = B.n }"
    )
    assertEquals(errors.size, 1)
    assert(errors.head.contains("B.scala:1 ->"))
    assert(errors.head.contains("C.scala:1 ->"))
    assert(!errors.head.contains("A.scala"))
  }

  test("independent cycles are both reported") {
    val errors = compile(
      "A.scala" -> "class A(val b: B)",
      "B.scala" -> "class B(val a: A)",
      "C.scala" -> "class C(val d: D)",
      "D.scala" -> "class D(val c: C)"
    )
    assertEquals(errors.size, 2)
    assert(errors.forall(_.startsWith("Cyclic source dependency (")))
  }

  test("diamonds and self references are acyclic across files") {
    assertEquals(
      compile(
        "A.scala" -> "class A(val b: B, val c: C, val a: Option[A])",
        "B.scala" -> "class B(val d: D)",
        "C.scala" -> "class C(val d: D)",
        "D.scala" -> "class D"
      ),
      Nil
    )
  }

  test("mutual references in one file are allowed") {
    assertEquals(compile("Both.scala" -> "class A(val b: B); class B(val a: A)"), Nil)
  }

  test("unused imports do not create dependencies") {
    assertEquals(
      compile(
        "A.scala" -> "package example; import B.*; object A { val n = 1 }",
        "B.scala" -> "package example; object B { def n = A.n }"
      ),
      Nil
    )
  }

  test("normal cycles involving suspended sources are still rejected") {
    val errors = compile(
      "A.scala"              -> "class A(val b: B) { val n = API.value }",
      "B.scala"              -> "class B(val a: A)",
      "API.scala"            -> "object API { transparent inline def value: Any = ${ Implementation.value } }",
      "Implementation.scala" ->
        "import scala.quoted.*; object Implementation { def value(using Quotes): Expr[Int] = '{ 1 } }"
    )
    assertEquals(errors.size, 1, errors.mkString("\n"))
    assert(errors.head.contains("A.scala:1 ->"))
    assert(errors.head.contains("B.scala:1 ->"))
    assert(!errors.head.contains("Implementation.scala"))
  }

  test("a suppressed case class closing a cycle through its parameter type is not reported") {
    assertEquals(
      compile(
        "A.scala" -> "@SuppressWarnings(Array(\"bonsai:acyclic\")) case class A(val b: B)",
        "B.scala" -> "class B(val a: A)"
      ),
      Nil
    )
  }

  test("a suppressed trait closing a cycle through its parent is not reported") {
    assertEquals(
      compile(
        "A.scala" -> "@SuppressWarnings(Array(\"bonsai:acyclic\")) trait A extends B",
        "B.scala" -> "trait B { def a: A }"
      ),
      Nil
    )
  }

  test("unsuppressed case classes and traits closing a cycle are reported") {
    assertCycle("A.scala" -> "case class A(val b: B)", "B.scala" -> "class B(val a: A)")
    assertCycle("A.scala" -> "trait A extends B", "B.scala"      -> "trait B { def a: A }")
  }

  test("a suppression holding the reported edge applies when the report is in a later file") {
    val sources = Seq(
      "X.scala" -> "class X(val y: Y)",
      "Y.scala" -> "class Y(val z: Z)",
      "Z.scala" -> "class Z(val y: Y, val x: X)"
    )
    assertCycle(sources*)
    assertEquals(
      compile(sources.updated(1, "Y.scala" -> "@SuppressWarnings(Array(\"bonsai:acyclic\")) class Y(val z: Z)")*),
      Nil
    )
  }

  test("a suppression applies to an edge recorded before a macro suspends another file") {
    val sources = Seq(
      "A.scala"              -> "class A(val z: Z)",
      "Z.scala"              -> "class Z(val a: A) { val n = API.value }",
      "API.scala"            -> "object API { transparent inline def value: Any = ${ Implementation.value } }",
      "Implementation.scala" ->
        "import scala.quoted.*; object Implementation { def value(using Quotes): Expr[Int] = '{ 1 } }"
    )
    val errors = compile(sources*)
    assertEquals(errors.size, 1, errors.mkString("\n"))
    assert(errors.head.contains("A.scala:1 ->"), errors.head)
    assertEquals(
      compile(sources.updated(0, "A.scala" -> "@SuppressWarnings(Array(\"bonsai:acyclic\")) class A(val z: Z)")*),
      Nil
    )
  }

  test("repeated compilations keep detecting cycles") {
    for (_ <- 1 to 2) assertCycle("A.scala" -> "class A(val b: B)", "B.scala" -> "class B(val a: A)")
  }
}
