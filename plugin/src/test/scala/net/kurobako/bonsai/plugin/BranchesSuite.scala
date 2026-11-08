package net.kurobako.bonsai.plugin

import java.nio.file.Files
import munit.FunSuite

class BranchesSuite extends FunSuite {
  private def run(source: String, options: String*): (List[String], List[String], String) =
    Compilations.run(Seq("Branches.scala" -> source), options) { (result, directory) =>
      (
        result.allErrors.map(_.message).toList,
        result.allWarnings.map(_.message).toList,
        Files.readString(directory.resolve("Branches.scala"))
      )
    }

  private def found(source: String, id: String): List[String] = {
    val (errors, warnings, _) = run(source)
    assertEquals(errors, Nil)
    warnings.collect { case message if message.endsWith(s"[bonsai:$id]") => message.takeWhile(_ != ';') }
  }

  private def rewritten(source: String): String = {
    val (errors, _, content) = run(source, "-rewrite")
    assertEquals(errors, Nil)
    content
  }

  private def assertJoined(source: String, expected: String): Unit = {
    assertEquals(found(source, identical).size, 1)
    assertEquals(rewritten(source), expected)
  }

  private val identical = "identical-branches"
  private val call      = "repeated-branch-call"

  test("adjacent if branches with equal bodies join their conditions") {
    val source = """object Branches {
                   |  def role(equals: Boolean, hash: Boolean, other: Boolean, name: String): Option[String] =
                   |    if (equals) Some(name)
                   |    else if (hash) Some(name)
                   |    else if (other) Some(name.trim)
                   |    else None
                   |}""".stripMargin
    assertJoined(
      source,
      """object Branches {
        |  def role(equals: Boolean, hash: Boolean, other: Boolean, name: String): Option[String] =
        |    if (equals || hash) Some(name)
        |    else if (other) Some(name.trim)
        |    else None
        |}""".stripMargin
    )
  }

  test("a run of three equal branches joins all of its conditions, braces and all") {
    val source = """object Branches {
                   |  def note(a: Boolean, b: Boolean, c: Boolean, text: String): String =
                   |    if (a) {
                   |      text.trim + "!"
                   |    } else if (b) {
                   |      text.trim + "!"
                   |    } else if (c) {
                   |      text.trim + "!"
                   |    } else {
                   |      text
                   |    }
                   |}""".stripMargin
    assertJoined(
      source,
      """object Branches {
        |  def note(a: Boolean, b: Boolean, c: Boolean, text: String): String =
        |    if (a || b || c) {
        |      text.trim + "!"
        |    } else {
        |      text
        |    }
        |}""".stripMargin
    )
  }

  test("a condition that is not a plain call is parenthesised when joined") {
    val source = """object Branches {
                   |  def pick(a: Boolean, b: Boolean, text: String): String =
                   |    if (if (a) b else !b) text.trim else if (a && b) text.trim else text
                   |}""".stripMargin
    assertJoined(
      source,
      """object Branches {
        |  def pick(a: Boolean, b: Boolean, text: String): String =
        |    if ((if (a) b else !b) || a && b) text.trim else text
        |}""".stripMargin
    )
  }

  test("if branches that are equal by shape after renaming the values they bind are the same") {
    val source = """object Branches {
                   |  def total(a: Boolean, b: Boolean, xs: List[Int]): Int =
                   |    if (a) xs.map(x => x + 1).sum
                   |    else if (b) xs.map(y => y + 1).sum
                   |    else 0
                   |}""".stripMargin
    assertEquals(found(source, identical).size, 1)
  }

  test("if branches equal only by text are not the same when they resolve to different symbols") {
    val source = """object Branches {
                   |  object Left { def render(value: Int): String = "l" + value }
                   |  object Right { def render(value: Int): String = "r" + value }
                   |  def total(a: Boolean, b: Boolean, value: Int): String =
                   |    if (a) { import Left.*; render(value) }
                   |    else if (b) { import Right.*; render(value) }
                   |    else ""
                   |  def show(value: Int): String  = "int"
                   |  def show(value: Long): String = "long"
                   |  def overloaded(a: Boolean, b: Boolean, x: Int, y: Long): String =
                   |    if (a) show(x) else if (b) show(y) else ""
                   |  def generic(a: Boolean, b: Boolean, x: Int, y: String): List[Any] =
                   |    if (a) List(x) else if (b) List(y) else Nil
                   |}""".stripMargin
    assertEquals(found(source, identical), Nil)
  }

  test("equal constants, non-adjacent equal branches and commented branches are left alone") {
    val source = """object Branches {
                   |  def table(a: Boolean, b: Boolean, c: Boolean): Int =
                   |    if (a) 1 else if (b) 1 else if (c) 2 else 3
                   |  def apart(a: Boolean, b: Boolean, c: Boolean, text: String): String =
                   |    if (a) text.trim else if (b) text.toUpperCase else if (c) text.trim else text
                   |  def commented(a: Boolean, b: Boolean, text: String): String =
                   |    if (a) text.trim
                   |    // kept apart on purpose
                   |    else if (b) text.trim
                   |    else text
                   |  def effects(a: Boolean, b: Boolean): Unit =
                   |    if (a) println("a") else if (b) println("b") else ()
                   |}""".stripMargin
    assertEquals(found(source, identical), Nil)
  }

  test("an if whose branches are equal decides nothing, and is fixed only when the condition is pure") {
    val source = """object Branches {
                   |  def pure(a: Boolean, text: String): String = if (a) text.trim else text.trim
                   |  def check(text: String): Boolean = text.nonEmpty
                   |  def calls(text: String): String = if (check(text)) text.trim else text.trim
                   |  def last(a: Boolean, b: Boolean, text: String): String =
                   |    if (a) text.toUpperCase else if (b) text.trim else text.trim
                   |}""".stripMargin
    assertEquals(found(source, identical).size, 3)
    assertEquals(
      rewritten(source),
      """object Branches {
        |  def pure(a: Boolean, text: String): String = text.trim
        |  def check(text: String): Boolean = text.nonEmpty
        |  def calls(text: String): String = if (check(text)) text.trim else text.trim
        |  def last(a: Boolean, b: Boolean, text: String): String =
        |    if (a) text.toUpperCase else text.trim
        |}""".stripMargin
    )
  }

  private val trees = """sealed trait Tree { val name: String = "tree" }
                        |final class TermSelect extends Tree
                        |final class TypeSelect extends Tree
                        |final class Other extends Tree
                        |""".stripMargin

  test("adjacent typed cases with equal guards and bodies become one case over a union") {
    val source = trees + """object Branches {
                           |  def diagnostic(tree: Tree): String = tree.toString
                           |  def lint(tree: Tree, q: Boolean, n: Boolean, c: Boolean): Option[String] = tree match {
                           |    case reference: TermSelect if q && n && c => Some(diagnostic(reference))
                           |    case reference: TypeSelect if q && n && c => Some(diagnostic(reference))
                           |    case _                                    => None
                           |  }
                           |}""".stripMargin
    assertJoined(
      source,
      trees + """object Branches {
                |  def diagnostic(tree: Tree): String = tree.toString
                |  def lint(tree: Tree, q: Boolean, n: Boolean, c: Boolean): Option[String] = tree match {
                |    case reference: (TermSelect | TypeSelect) if q && n && c => Some(diagnostic(reference))
                |    case _                                    => None
                |  }
                |}""".stripMargin
    )
  }

  test("cases that bind different names for their values are not joined, even with one body") {
    val source = trees + """object Branches {
                           |  def name(tree: Tree): String = tree match {
                           |    case term: TermSelect => term.name.trim
                           |    case kind: TypeSelect => kind.name.trim
                           |    case other: Other     => other.name
                           |  }
                           |}""".stripMargin
    assertEquals(found(source, identical), Nil)
  }

  test("cases whose bodies call different members of their own types are not the same") {
    val source = """sealed trait Shape
                   |final case class Disc(radius: Double) extends Shape { def size: Double = radius }
                   |final case class Square(side: Double) extends Shape { def size: Double = side }
                   |object Branches {
                   |  def size(shape: Shape): Double = shape match {
                   |    case disc: Disc     => disc.size
                   |    case square: Square => square.size
                   |  }
                   |  def render(value: Any): String = value match {
                   |    case n: Int  => String.valueOf(n)
                   |    case l: Long => String.valueOf(l)
                   |    case _       => ""
                   |  }
                   |  def boxed(value: Any): List[Any] = value match {
                   |    case n: Int    => List(n)
                   |    case s: String => List(s)
                   |    case _         => Nil
                   |  }
                   |}""".stripMargin
    assertEquals(found(source, identical), Nil)
  }

  test("alternative constants and objects join when their bodies are equal") {
    val source = """enum Colour { case Red, Green, Blue }
                   |object Branches {
                   |  def code(n: Int): String = n match {
                   |    case 1 => "one".trim
                   |    case 2 => "one".trim
                   |    case 3 => "other"
                   |    case _ => ""
                   |  }
                   |  def warm(colour: Colour, text: String): String = colour match {
                   |    case Colour.Red   => text.trim
                   |    case Colour.Green => text.trim
                   |    case Colour.Blue  => text
                   |  }
                   |}""".stripMargin
    assertEquals(found(source, identical).size, 2)
    val fixed = rewritten(source)
    assert(fixed.contains("case 1 | 2 => \"one\".trim"), fixed)
    assert(fixed.contains("case Colour.Red | Colour.Green   => text.trim"), fixed)
  }

  test("typed cases without binders join as one test over a union") {
    val source = trees + """object Branches {
                           |  def kind(tree: Tree, text: String): String = tree match {
                           |    case _: TermSelect => text.trim
                           |    case _: TypeSelect => text.trim
                           |    case _             => text
                           |  }
                           |}""".stripMargin
    assertEquals(found(source, identical).size, 1)
    assert(rewritten(source).contains("case _: (TermSelect | TypeSelect) => text.trim"))
  }

  test("a case with the body of the default case is removed") {
    val source = trees + """object Branches {
                           |  def size(tree: Tree, text: String): String = tree match {
                           |    case term: TermSelect => term.name
                           |    case _: TypeSelect    => text.trim
                           |    case _: Other         => text.trim
                           |    case _                => text.trim
                           |  }
                           |}""".stripMargin
    assertJoined(
      source,
      trees + """object Branches {
                |  def size(tree: Tree, text: String): String = tree match {
                |    case term: TermSelect => term.name
                |    case _                => text.trim
                |  }
                |}""".stripMargin
    )
  }

  test("cases with impure guards, extractor patterns, non-adjacent bodies, constants or comments are left alone") {
    val source = trees + """object Branches {
                           |  def check(tree: Tree): Boolean = scala.util.Random.nextBoolean()
                           |  def guards(tree: Tree, text: String): String = tree match {
                           |    case _: TermSelect if check(tree) => text.trim
                           |    case _: TypeSelect if check(tree) => text.trim
                           |    case _                            => text
                           |  }
                           |  final case class Term(a: Int)
                           |  final case class Typ(a: Int)
                           |  def extractors(tree: Any, text: String): String = tree match {
                           |    case Term(a) => text.trim + a
                           |    case Typ(a)  => text.trim + a
                           |    case _       => text
                           |  }
                           |  def apart(tree: Tree, text: String): String = tree match {
                           |    case _: TermSelect => text.trim
                           |    case _: Other      => text.toUpperCase
                           |    case _: TypeSelect => text.trim
                           |  }
                           |  def constants(n: Int): String = n match {
                           |    case 1 => "x"
                           |    case 2 => "x"
                           |    case _ => "y"
                           |  }
                           |  def commented(tree: Tree, text: String): String = tree match {
                           |    case _: TermSelect => text.trim
                           |    // type selects are spelt out for the next reader
                           |    case _: TypeSelect => text.trim
                           |    case _             => text
                           |  }
                           |  def guarded(tree: Tree, text: String, flag: Boolean): String = tree match {
                           |    case _: TermSelect if flag  => text.trim
                           |    case _: TypeSelect if !flag => text.trim
                           |    case _                       => text
                           |  }
                           |}""".stripMargin
    assertEquals(found(source, identical), Nil)
  }

  test("catch handlers with equal bodies join") {
    val source = """object Branches {
                   |  def parse(text: String): Int =
                   |    try text.toInt
                   |    catch {
                   |      case _: NumberFormatException => text.length
                   |      case _: IllegalStateException => text.length
                   |    }
                   |}""".stripMargin
    assertEquals(found(source, identical).size, 1)
    assert(rewritten(source).contains("case _: (NumberFormatException | IllegalStateException) => text.length"))
  }

  test("branches that each call one method differing in one argument are reported") {
    val source =
      """object Branches {
                   |  def at(nodes: Map[String, Int], key: String, text: String): Option[String] =
                   |    nodes.get(key).map(_ => text)
                   |  def findings(size: Int, enumerated: Boolean, hot: Boolean, nodes: Map[String, Int], key: String): Option[String] =
                   |    if (size > 100) at(nodes, key, s"has $size bytes")
                   |    else if (enumerated) at(nodes, key, s"enum of $size bytes")
                   |    else if (hot) at(nodes, key, s"$size bytes interpreted")
                   |    else at(nodes, key, s"$size bytes")
                   |}""".stripMargin
    val reported = found(source, call)
    assertEquals(reported.size, 1)
    assert(reported.head.contains("every branch calls at, differing in text"), reported.head)
  }

  test("match cases that each call one method differing in one argument are reported") {
    val source =
      """object Branches {
                   |  def emit(sink: StringBuilder, indent: Int, name: String, value: Int): Unit = sink.append(name * indent + value): Unit
                   |  def write(kind: Int, sink: StringBuilder, indent: Int, n: Int): Unit = kind match {
                   |    case 0 => emit(sink, indent, "zero", n)
                   |    case 1 => emit(sink, indent, "zero", n + 1)
                   |    case _ => emit(sink, indent, "zero", n * 2)
                   |  }
                   |}""".stripMargin
    assertEquals(found(source, call).size, 1)
  }

  test(
    "calls differing in two arguments, building objects, or taking functions and by-name arguments are not reported"
  ) {
    val source =
      """object Branches {
                   |  final case class Pair(left: Int, right: Int, name: String)
                   |  def emit(sink: StringBuilder, indent: Int, name: String, value: Int): Unit = sink.append(name * indent + value): Unit
                   |  def lazily(sink: StringBuilder, indent: Int, name: => String): Unit = sink.append(name * indent): Unit
                   |  def apply(sink: StringBuilder, indent: Int, f: Int => Int): Unit = sink.append(f(indent)): Unit
                   |  def other(sink: StringBuilder, indent: Int, name: String, value: Int): Unit = sink.append(name): Unit
                   |  def two(kind: Int, a: Int, b: Int): Int = kind match {
                   |    case 0 => math.max(a, 1)
                   |    case 1 => math.max(a, 2)
                   |    case _ => math.max(a, 3)
                   |  }
                   |  def shared(kind: Int, sink: StringBuilder, indent: Int, n: Int): Unit = kind match {
                   |    case 0 => emit(sink, indent + 1, "zero", n)
                   |    case 1 => emit(sink, indent + 2, "one", n + 1)
                   |    case _ => emit(sink, indent + 3, "many", n + 2)
                   |  }
                   |  def constructed(kind: Int, n: Int): Pair = kind match {
                   |    case 0 => Pair(n, 1, "a")
                   |    case 1 => Pair(n, 1, "b" + n)
                   |    case _ => Pair(n, 1, "c" + n)
                   |  }
                   |  def literals(kind: Int, sink: StringBuilder, n: Int): Unit = kind match {
                   |    case 0 => emit(sink, 1, "zero", n)
                   |    case 1 => emit(sink, 1, "one", n)
                   |    case _ => emit(sink, 1, "many", n)
                   |  }
                   |  def named(kind: Int, sink: StringBuilder, n: Int): Unit = kind match {
                   |    case 0 => lazily(sink, n, "a" + n)
                   |    case 1 => lazily(sink, n, "b" + n)
                   |    case _ => lazily(sink, n, "c" + n)
                   |  }
                   |  def functions(kind: Int, sink: StringBuilder, n: Int): Unit = kind match {
                   |    case 0 => apply(sink, n, _ + 1)
                   |    case 1 => apply(sink, n, _ + 2)
                   |    case _ => apply(sink, n, _ + 3)
                   |  }
                   |  def mixed(kind: Int, sink: StringBuilder, n: Int, m: String): Unit = kind match {
                   |    case 0 => emit(sink, n, m, 1)
                   |    case 1 => other(sink, n, m, 2)
                   |    case _ => emit(sink, n, m, 3)
                   |  }
                   |  def wide(kind: Int, sink: StringBuilder, indent: Int, n: Int): Unit = kind match {
                   |    case 0 => emit(sink, indent + 1, "zero", n)
                   |    case 1 => emit(sink, indent + 2, "zero", n + 1)
                   |    case _ => emit(sink, indent + 3, "zero", n + 2)
                   |  }
                   |  def short(kind: Int, sink: StringBuilder, n: Int, m: String): Unit =
                   |    if (kind == 0) emit(sink, n, m, 1) else emit(sink, n, m, n)
                   |  def open(kind: Int, sink: StringBuilder, n: Int, m: String): Unit =
                   |    if (kind == 0) emit(sink, n, m, 1) else if (kind == 1) emit(sink, n, m, n) else if (kind == 2) emit(sink, n, m, n + 1)
                   |}""".stripMargin
    assertEquals(found(source, call), Nil)
  }

  test("a match of eight or more single-line cases is a dispatch table, not identical branches") {
    def table(arms: Int): String = {
      val cases = (0 until arms).map(code => s"    case $code => sink.append(n): Unit").mkString("\n")
      s"""object Branches {
         |  def write(code: Int, sink: StringBuilder, n: Int): Unit = code match {
         |$cases
         |  }
         |}""".stripMargin
    }
    assertEquals(found(table(8), identical), Nil)
    assertEquals(found(table(7), identical).size, 1)
  }

  private val binding = "binding-in-one-branch"

  test("a value computed for every path but used in one branch moves into it") {
    val source = """object Bindings {
                   |  def shape(set: Set[String], elements: List[String]): Int = {
                   |    val category = set.headOption.fold("")(_.trim)
                   |    if (elements.forall(_.isEmpty)) category.length else 0
                   |  }
                   |}""".stripMargin
    assertEquals(found(source, binding).size, 1)
    assertEquals(
      rewritten(source),
      """object Bindings {
        |  def shape(set: Set[String], elements: List[String]): Int = {
        |    if (elements.forall(_.isEmpty)) { val category = set.headOption.fold("")(_.trim); category.length } else 0
        |  }
        |}""".stripMargin
    )
  }

  test("a value used by one case moves into it, and into the braces of a block") {
    val source = """object Bindings {
                   |  def label(kind: Int, names: List[String]): String = {
                   |    val joined = names.mkString(",")
                   |    kind match {
                   |      case 0 => "none"
                   |      case 1 => joined
                   |      case _ => "many"
                   |    }
                   |  }
                   |  def total(flag: Boolean, xs: List[Int]): Int = {
                   |    val sum = xs.map(_ * 2).sum
                   |    println("start")
                   |    if (flag) {
                   |      sum + 1
                   |    } else {
                   |      0
                   |    }
                   |  }
                   |}""".stripMargin
    assertEquals(found(source, binding).size, 2)
    assertEquals(
      rewritten(source),
      """object Bindings {
        |  def label(kind: Int, names: List[String]): String = {
        |    kind match {
        |      case 0 => "none"
        |      case 1 => { val joined = names.mkString(","); joined }
        |      case _ => "many"
        |    }
        |  }
        |  def total(flag: Boolean, xs: List[Int]): Int = {
        |    println("start")
        |    if (flag) {
        |      val sum = xs.map(_ * 2).sum
        |      sum + 1
        |    } else {
        |      0
        |    }
        |  }
        |}""".stripMargin
    )
  }

  test("a value used by several branches, a condition, a guard or the code after the branching stays") {
    val source = """object Bindings {
                   |  def both(flag: Boolean, xs: List[Int]): Int = {
                   |    val sum = xs.sum
                   |    if (flag) sum + 1 else sum - 1
                   |  }
                   |  def condition(xs: List[Int]): Int = {
                   |    val sum = xs.sum
                   |    if (sum > 3) sum else 0
                   |  }
                   |  def guard(kind: Int, xs: List[Int]): Int = {
                   |    val sum = xs.sum
                   |    kind match {
                   |      case n if n > sum => 1
                   |      case _            => 0
                   |    }
                   |  }
                   |  def after(flag: Boolean, xs: List[Int]): Int = {
                   |    val sum = xs.sum
                   |    val chosen = if (flag) sum else 0
                   |    chosen + sum
                   |  }
                   |  def nested(flag: Boolean, xs: List[Int]): Int = {
                   |    val sum = xs.sum
                   |    Math.max(1, if (flag) sum else 0)
                   |  }
                   |  def scrutinee(xs: List[Int]): Int = {
                   |    val sum = xs.sum
                   |    sum match {
                   |      case 0 => 1
                   |      case n => n
                   |    }
                   |  }
                   |}""".stripMargin
    assertEquals(found(source, binding), Nil)
  }

  test("a value that is not pure, not computed, or not a plain value is not moved") {
    val source = """import scala.collection.mutable.ArrayBuffer
                   |object Bindings {
                   |  private var counter = 0
                   |  private def expensive(xs: List[Int]): Int = xs.size
                   |  def user(flag: Boolean, xs: List[Int]): Int = {
                   |    val size = expensive(xs)
                   |    if (flag) size else 0
                   |  }
                   |  def failing(flag: Boolean, xs: List[Int]): Int = {
                   |    val first = xs.head + 1
                   |    if (flag) first else 0
                   |  }
                   |  def variable(flag: Boolean): Int = {
                   |    val read = counter + 1
                   |    counter += 1
                   |    if (flag) read else 0
                   |  }
                   |  def mutable(flag: Boolean, buffer: ArrayBuffer[Int]): Int = {
                   |    val size = buffer.size + 1
                   |    buffer += 1
                   |    if (flag) size else 0
                   |  }
                   |  def effect(flag: Boolean): Int = {
                   |    val unit = println("x")
                   |    if (flag) unit.hashCode else 0
                   |  }
                   |  def alias(flag: Boolean, n: Int): Int = {
                   |    val same = n
                   |    if (flag) same else 0
                   |  }
                   |  def literal(flag: Boolean): Int = {
                   |    val three = 3
                   |    if (flag) three else 0
                   |  }
                   |  def lazily(flag: Boolean, xs: List[Int]): Int = {
                   |    lazy val size = xs.size + 1
                   |    if (flag) size else 0
                   |  }
                   |  def divided(flag: Boolean, a: Int, b: Int): Int = {
                   |    val ratio = a / b
                   |    if (flag) ratio else 0
                   |  }
                   |}""".stripMargin
    assertEquals(found(source, binding), Nil)
  }

  test("a value is reported but not moved into a case whose pattern binds a name its initialiser uses") {
    val source = """object Bindings {
                   |  def capture(kind: Any, items: List[Int]): Int = {
                   |    val size = items.size + 1
                   |    kind match {
                   |      case items: List[?] => size + items.size
                   |      case _              => 0
                   |    }
                   |  }
                   |}""".stripMargin
    assertEquals(found(source, binding).size, 1)
    assertEquals(rewritten(source), source)
  }

  test("a value computed by a polymorphic function value compiles and is not moved") {
    val source = """object Bindings {
                   |  def lifted(lift: [A] => List[A] => List[A], flag: Boolean, xs: List[Int]): Int = {
                   |    val doubled = lift(xs).map(_ * 2)
                   |    if (flag) doubled.size else 0
                   |  }
                   |}""".stripMargin
    assertEquals(found(source, binding), Nil)
  }

  private val lazily = "hand-written-lazy"

  test("a method filling a flag and a cache on its first call is a lazy val") {
    val source = """final class Node(key: String) {
                   |  private var element: Option[String] = None
                   |  private var known: Boolean         = false
                   |
                   |  /** The element the node names. */
                   |  def symbol: Option[String] = {
                   |    if !known then {
                   |      known = true
                   |      element = Option(key.trim)
                   |    }
                   |    element
                   |  }
                   |
                   |  def other: Int = 1
                   |}""".stripMargin
    assertEquals(found(source, lazily).size, 1)
    assertEquals(
      rewritten(source),
      """final class Node(key: String) {
        |
        |  /** The element the node names. */
        |  lazy val symbol: Option[String] = Option(key.trim)
        |
        |  def other: Int = 1
        |}""".stripMargin
    )
  }

  test("a method filling a cache that is null until its first call is a lazy val") {
    val source = """final class Holder(key: String) {
                   |  private var cached: String = null
                   |  private def first: String = {
                   |    if (cached == null) cached = key.trim
                   |    cached
                   |  }
                   |  def both: String = first + first
                   |}""".stripMargin
    assertEquals(found(source, lazily).size, 1)
    assertEquals(
      rewritten(source),
      """final class Holder(key: String) {
        |  private lazy val first: String = key.trim
        |  def both: String = first + first
        |}""".stripMargin
    )
  }

  test("a cache that something else writes or reads, or a method that can be overridden, is not a lazy val") {
    val source = """class Node(key: String) {
                   |  private var element: Option[String] = None
                   |  private var known: Boolean         = false
                   |  def reset(): Unit = known = false
                   |  final def symbol: Option[String] = {
                   |    if (!known) {
                   |      known = true
                   |      element = Option(key.trim)
                   |    }
                   |    element
                   |  }
                   |}
                   |class Open(key: String) {
                   |  private var element: Option[String] = None
                   |  private var known: Boolean         = false
                   |  def symbol: Option[String] = {
                   |    if (!known) {
                   |      known = true
                   |      element = Option(key.trim)
                   |    }
                   |    element
                   |  }
                   |}
                   |final class Peek(key: String) {
                   |  private var element: Option[String] = None
                   |  private var known: Boolean         = false
                   |  def symbol: Option[String] = {
                   |    if (!known) {
                   |      known = true
                   |      element = Option(key.trim)
                   |    }
                   |    element
                   |  }
                   |  def loaded: Boolean = known
                   |}
                   |final class Self(key: String) {
                   |  private var element: Option[String] = None
                   |  private var known: Boolean         = false
                   |  def symbol: Option[String] = {
                   |    if (!known) {
                   |      known = true
                   |      element = element.orElse(Option(key.trim))
                   |    }
                   |    element
                   |  }
                   |}
                   |final class Counter(key: String) {
                   |  private var count: Int = 0
                   |  def next(): Int = {
                   |    if (count == 0) count = key.length
                   |    count
                   |  }
                   |  private var element: String = ""
                   |  private var known: Boolean   = false
                   |  def again(): String = {
                   |    if (!known) {
                   |      known = true
                   |      element = key.trim
                   |    }
                   |    element
                   |  }
                   |}
                   |final class Repeat(key: String) {
                   |  private var element: String = null
                   |  private def twice: String = {
                   |    if (element == null) element = key.trim
                   |    element
                   |  }
                   |  def reset(): Unit = element = null
                   |}""".stripMargin
    assertEquals(found(source, lazily), Nil)
  }

  private val roundTrip = "string-round-trip"

  test("a string built by an interpolation and parsed back with the same format in one method is reported") {
    val source =
      """object Trips {
                   |  final case class Entry(project: String, scope: String, classes: String)
                   |  def analysed(entries: Vector[Entry]): Set[String] = {
                   |    val outputs = entries.map(entry => s"output\t${entry.project}\t${entry.scope}\t${entry.classes}")
                   |    outputs.collect { case s"output\t$_\t$_\t$directory" => directory }.toSet
                   |  }
                   |  def direct(a: String, b: String): Option[String] =
                   |    s"$a:$b" match { case s"$left:$_" => Some(left); case _ => None }
                   |}""".stripMargin
    assertEquals(found(source, roundTrip).size, 2)
  }

  test("a pattern over strings from elsewhere, another format, or another method is not a round trip") {
    val source =
      """object Trips {
                   |  final case class Entry(project: String, scope: String)
                   |  def parsed(lines: Vector[String], entries: Vector[Entry]): (Set[String], Vector[String]) = {
                   |    val outputs = entries.map(entry => s"output\t${entry.project}\t${entry.scope}")
                   |    (lines.collect { case s"output\t$_\t$scope" => scope }.toSet, outputs)
                   |  }
                   |  def other(entries: Vector[Entry]): Set[String] = {
                   |    val outputs = entries.map(entry => s"output\t${entry.project}\t${entry.scope}")
                   |    outputs.collect { case s"library\t$_\t$scope" => scope }.toSet
                   |  }
                   |  def render(entries: Vector[Entry]): Vector[String] =
                   |    entries.map(entry => s"output\t${entry.project}\t${entry.scope}")
                   |  def parse(lines: Vector[String]): Set[String] = lines.collect { case s"output\t$_\t$scope" => scope }.toSet
                   |  def bare(a: String, b: String): String = s"$a$b" match { case s"$x$y" => x; case _ => "" }
                   |  def single(a: String): String = s"<$a>" match { case s"<$x>" => x; case _ => "" }
                   |}""".stripMargin
    assertEquals(found(source, roundTrip), Nil)
  }

  test("conditions written with then are joined and parenthesised") {
    val source = """object Branches {
                   |  def pick(a: Boolean, b: Boolean, text: String): String =
                   |    if a then text.trim else if b then text.trim else text
                   |}""".stripMargin
    assertEquals(found(source, identical).size, 1)
    assert(rewritten(source).contains("if (a || b) then text.trim else text"), rewritten(source))
  }

  test("code the compiler generates is not reported") {
    val source =
      """enum Colour(val code: Int) { case Red extends Colour(1); case Green extends Colour(1); case Blue extends Colour(1) }
                   |final case class Point(x: Int, y: Int, z: Int) derives CanEqual
                   |sealed trait Shape
                   |final case class Disc(r: Double) extends Shape
                   |final case class Square(s: Double) extends Shape
                   |object Branches {
                   |  def pairs(xs: List[(Int, Int)]): List[Int] = for ((a, b) <- xs; if a > 0) yield a + b
                   |  def destructure(pair: (Int, Int)): Int = { val (a, b) = pair; a + b }
                   |  def tag(a: Int, b: Int): String = s"$a:$b"
                   |  def total: PartialFunction[Shape, Double] = { case Disc(r) => r; case Square(s) => s }
                   |}""".stripMargin
    val (errors, warnings, _) = run(source)
    assertEquals(errors, Nil)
    val ours = List(identical, call, binding, lazily, roundTrip).flatMap(id =>
      warnings.filter(_.endsWith(s"[bonsai:$id]"))
    )
    assertEquals(ours, Nil)
  }
}
