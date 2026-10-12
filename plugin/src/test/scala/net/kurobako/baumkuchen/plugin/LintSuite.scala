package net.kurobako.baumkuchen.plugin

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.nio.file.Files
import munit.FunSuite
import scala.jdk.CollectionConverters.*
import scala.util.Using

class LintSuite extends FunSuite {
  private def warnings(source: String, options: String*): List[String] = warningsAt("Lints.scala", source, options*)

  private def warningsAt(path: String, source: String, options: String*): List[String] = {
    val (errors, warnings) = compile(path, source, options*)
    assertEquals(errors, Nil)
    warnings
  }

  private def compile(path: String, source: String, options: String*): (List[String], List[String]) = {
    val (errors, warnings, _) = run(path, source, options*)
    (errors, warnings)
  }

  /** The source after compiling it with `-rewrite`, which applies the fixes of the warnings. */
  private def rewritten(source: String, options: String*): String = {
    val (errors, _, content) = run("Lints.scala", source, ("-rewrite" +: options)*)
    assertEquals(errors, Nil)
    content
  }

  private def run(path: String, source: String, options: String*): (List[String], List[String], String) = {
    val (errors, warnings, content) = report(path, source, options*)
    (errors, warnings.map(_._1), content)
  }

  /** Warnings with their lines and columns, since a console reporter shows only one warning per position. */
  private def positioned(source: String): List[(String, Int, Int)] = report("Lints.scala", source)._2

  private def report(path: String, source: String, options: String*)
      : (List[String], List[(String, Int, Int)], String) = {
    val directory = Files.createTempDirectory("baumkuchen-")
    try {
      val output = Files.createDirectory(directory.resolve("classes"))
      val file   = directory.resolve(path)
      Files.createDirectories(file.getParent)
      Files.writeString(file, source)
      val result = Driver().process(
        Array(
          "-classpath",
          sys.props("baumkuchen.plugin.classpath"),
          "-d",
          output.toString,
          s"-Xplugin:${sys.props("baumkuchen.plugin.jar")}",
          "-Xplugin-require:baumkuchen",
          "-color:never"
        ) ++ options.map(option => if (option.startsWith("-")) option else s"-P:baumkuchen:$option") :+ file.toString,
        StoreReporter()
      )
      (
        result.allErrors.map(_.message).toList,
        result.allWarnings.map(warning => (warning.message, warning.pos.line, warning.pos.column)).toList,
        Files.readString(file)
      )
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  private def assertWarns(source: String, fragment: String): Unit = {
    val found = warnings(source)
    assert(found.exists(_.contains(fragment)), found.mkString("\n"))
  }

  test("with -rewrite, argument names, extractors and dropped conversions are written in") {
    val source = """case class Point(x: Int, y: Int)
                   |case class Wide(a: Int, b: Int, c: Int, d: Int)
                   |object Geometry {
                   |  def rectangle(width: Int, height: Int): Int        = width * height
                   |  def label(text: String, centred: Boolean): String = text
                   |  def each(count: Int)(body: Int => Int): Int     = body(count)
                   |  def mix(red: Int, green: Int, blue: Int, alpha: Int, gamma: Int): Int = red
                   |}
                   |object Calls {
                   |  val partial: (Int, Int, Int, Int) => Int = Geometry.mix(_, _, _, _, 5)
                   |  val area  = Geometry.rectangle(3, 4)
                   |  val text  = Geometry.label("x", true)
                   |  val block = Geometry.each(2) { count => count }
                   |  def same(value: Long): Long = value.toLong
                   |  def sum(left: Long, right: Long): Long = (left + right).toLong
                   |  def kind(value: Any): Int = value match {
                   |    case _: Point         => 1
                   |    case Wide(_, _, _, d) => d
                   |    case Wide(a, _, _, _) => a
                   |    case Wide(_, b, _, _) => b
                   |    case ((_, _, _, d: Int), i: Int) => d + i
                   |    case _: (String, String) => 2
                   |    case _                => 0
                   |  }
                   |}""".stripMargin
    val expected = """case class Point(x: Int, y: Int)
                     |case class Wide(a: Int, b: Int, c: Int, d: Int)
                     |object Geometry {
                     |  def rectangle(width: Int, height: Int): Int        = width * height
                     |  def label(text: String, centred: Boolean): String = text
                     |  def each(count: Int)(body: Int => Int): Int     = body(count)
                     |  def mix(red: Int, green: Int, blue: Int, alpha: Int, gamma: Int): Int = red
                     |}
                     |object Calls {
                     |  val partial: (Int, Int, Int, Int) => Int = Geometry.mix(_, _, _, _, 5)
                     |  val area  = Geometry.rectangle(width = 3, height = 4)
                     |  val text  = Geometry.label("x", centred = true)
                     |  val block = Geometry.each(2) { count => count }
                     |  def same(value: Long): Long = value
                     |  def sum(left: Long, right: Long): Long = (left + right)
                     |  def kind(value: Any): Int = value match {
                     |    case Point(_, _)         => 1
                     |    case Wide(d = d) => d
                     |    case Wide(a = a) => a
                     |    case Wide(b = b) => b
                     |    case ((_, _, _, d: Int), i: Int) => d + i
                     |    case (_, _) => 2
                     |    case _                => 0
                     |  }
                     |}""".stripMargin
    assertEquals(rewritten(source), expected)
    assertEquals(rewritten(expected), expected)
  }

  private def lines(found: List[String], check: String): List[String] =
    found.filter(_.endsWith(s"[baumkuchen:$check]"))

  test("exceptions thrown only for a handler or a helper's caller to turn into a result are reported") {
    val found = warnings(
      """object Lints {
        |  def parse(t: String): Either[String, Int] =
        |    try { if (t.isEmpty) throw IllegalArgumentException("e"); Right(1) }
        |    catch { case e: IllegalArgumentException => Left(e.getMessage) }
        |  def other(t: String): Either[String, Int] =
        |    try { if (t.isEmpty) throw IllegalStateException("e"); Right(1) }
        |    catch { case _: IllegalArgumentException => Left(t) }
        |  def later(t: String): Either[String, () => Int] =
        |    try Right(() => throw IllegalArgumentException(t)) catch { case _: IllegalArgumentException => Left(t) }
        |  def rethrown(t: String): Either[String, Int] =
        |    try { if (t.isEmpty) throw IllegalArgumentException("e"); Right(1) }
        |    catch { case e: IllegalArgumentException => throw IllegalStateException(e) }
        |  def token(m: Map[String, String]): Either[String, String] =
        |    Right(m.get("k").getOrElse(throw IllegalArgumentException("k")))
        |  def plain(m: Map[String, String]): String = m.get("k").getOrElse(throw IllegalArgumentException("k"))
        |  def decode(m: Map[String, String]): Either[String, Long] =
        |    try Right(number(m)) catch { case e: IllegalArgumentException => Left(e.getMessage) }
        |  private def number(m: Map[String, String]): Long =
        |    m.get("n").flatMap(_.toLongOption).getOrElse(throw IllegalArgumentException("n"))
        |  private def escapes(k: String): Long = if (k.isEmpty) throw IllegalArgumentException(k) else 1L
        |  def leak(k: String): Long = escapes(k)
        |  def guarded(k: String): Either[String, Long] =
        |    try Right(escapes(k)) catch { case _: IllegalArgumentException => Left(k) }
        |}""".stripMargin
    )
    assertEquals(lines(found, "local-throw").size, 1, found.mkString("\n"))
    assert(lines(found, "local-throw").head.startsWith("IllegalArgumentException is thrown"), found.mkString("\n"))
    assertEquals(lines(found, "throw-in-default").size, 1, found.mkString("\n"))
    assertEquals(lines(found, "throwing-helper").map(_.takeWhile(_ != ' ')), List("number"), found.mkString("\n"))
  }

  test("a helper sharing an exception that other code throws as its failure channel is not reported") {
    val found = warnings(
      """object Parser {
        |  case class Failure(message: String) extends Exception(message)
        |  class Cursor(tokens: List[String]) {
        |    def fail(message: String): Nothing = throw Failure(message)
        |    def program(): Int                 = if (tokens.isEmpty) fail("empty") else tokens.size
        |  }
        |  def parse(source: String): Either[String, Int] =
        |    try Right(Cursor(tokenise(source)).program()) catch { case error: Failure => Left(error.message) }
        |  def extract(source: String): Either[String, Int] =
        |    try Right(Cursor(tokenise(source)).program()) catch { case Failure(message) => Left(message) }
        |  private def tokenise(source: String): List[String] =
        |    if (source.isEmpty) throw Failure("blank") else source.split(' ').toList
        |}""".stripMargin
    )
    assertEquals(lines(found, "throwing-helper"), Nil, found.mkString("\n"))
  }

  test("partial calls inside a handler are reported, and a Try around one with a total alternative is redundant") {
    val source =
      """import scala.util.control.NonFatal
                   |object Lints {
                   |  def lookup(m: Map[String, String]): Option[Int] = scala.util.Try(m("k").toInt).toOption
                   |  def code(m: Map[String, Map[String, String]]): Option[String] =
                   |    try m.get("e").map(_("c")) catch { case NonFatal(_) => None }
                   |  def first(xs: List[Int]): Option[Int] = try Some(xs.head) catch { case _: NoSuchElementException => None }
                   |  def unrelated(f: java.util.concurrent.CompletableFuture[Int]): Option[Int] = scala.util.Try(f.get()).toOption
                   |  def kept(s: String): Either[Throwable, Long] = scala.util.Try(s.toLong).toEither
                   |  def number(s: String): Option[Int] = scala.util.Try(s.toInt).toOption
                   |  def checked(s: String): Either[String, Int] = scala.util.Try(s.toInt).toEither.left.map(_ => s"bad $s")
                   |  def impure(it: Iterator[String]): Option[Int] = scala.util.Try(it.next().toInt).toOption
                   |}""".stripMargin
    val found = warnings(source)
    assertEquals(lines(found, "partial-in-try").size, 3, found.mkString("\n"))
    assert(lines(found, "partial-in-try").exists(_.contains("apply, toInt")), found.mkString("\n"))
    assertEquals(lines(found, "redundant-try").size, 3, found.mkString("\n"))
    val fixed = rewritten(source)
    assert(fixed.contains("def number(s: String): Option[Int] = s.toIntOption"), fixed)
    assert(fixed.contains("""def checked(s: String): Either[String, Int] = s.toIntOption.toRight(s"bad $s")"""), fixed)
    assert(fixed.contains("scala.util.Try(it.next().toInt).toOption"), fixed)
  }

  test("catch-alls turning a known throwing call into a result, and dropped causes, are reported") {
    val source =
      """import java.time.LocalDate
        |import java.time.format.DateTimeParseException
        |import scala.util.control.NonFatal
        |enum Failure extends Exception {
        |  case Transport(message: String, cause: Option[Throwable] = None)
        |  case Plain(message: String)
        |}
        |trait Errs[F[_]] { def handleError[A](fa: => F[A])(handler: PartialFunction[Throwable, F[A]]): F[A] }
        |object Lints {
        |  def date(t: String): Either[String, LocalDate] =
        |    try Right(LocalDate.parse(t)) catch { case NonFatal(e) => Left(e.getMessage) }
        |  def narrow(t: String): Either[String, LocalDate] =
        |    try Right(LocalDate.parse(t)) catch { case e: DateTimeParseException => Left(e.getMessage) }
        |  def tried(t: String): Option[LocalDate] = scala.util.Try(LocalDate.parse(t)).toOption
        |  def run(): Int = 1
        |  def plain(): Either[String, Int] = try Right(run()) catch { case NonFatal(e) => Left(e.getMessage) }
        |  def quiet(t: String): Unit = try { LocalDate.parse(t); () } catch { case NonFatal(_) => () }
        |  def handled[F[_]](errs: Errs[F], pure: Either[String, Int] => F[Either[String, Int]], t: String) =
        |    errs.handleError(pure(Right(Integer.parseInt(t)))) { case NonFatal(e) => pure(Left(e.getMessage)) }
        |  def transport(t: String): Either[Failure, Int] =
        |    try Right(Integer.parseInt(t)) catch { case NonFatal(_) => Left(Failure.Transport("x")) }
        |  def none(t: String): Either[Failure, Int] =
        |    try Right(Integer.parseInt(t)) catch { case _: NumberFormatException => Left(Failure.Transport("x", None)) }
        |  def kept(t: String): Either[Failure, Int] =
        |    try Right(Integer.parseInt(t)) catch { case e: NumberFormatException => Left(Failure.Transport("x", Some(e))) }
        |  def simple(t: String): Either[Failure, Int] =
        |    try Right(Integer.parseInt(t)) catch { case _: NumberFormatException => Left(Failure.Plain("x")) }
        |  def lifted[F[_]](errs: Errs[F], pure: Either[Failure, Int] => F[Either[Failure, Int]], t: String) =
        |    errs.handleError(pure(Right(Integer.parseInt(t)))) { case NonFatal(_) => pure(Left(Failure.Transport("x"))) }
        |}""".stripMargin
    val found = warnings(source)
    assertEquals(lines(found, "broad-catch").size, 4, found.mkString("\n"))
    assert(lines(found, "broad-catch").exists(_.contains("DateTimeParseException")), found.mkString("\n"))
    assertEquals(lines(found, "discarded-cause").size, 3, found.mkString("\n"))
    val positions = positioned(source).map((_, line, column) => line -> column)
    assertEquals(positions.distinct.size, positions.size, positioned(source).mkString("\n"))
  }

  test("documented exceptions follow what each call and platform actually throws") {
    def thrown(platform: Exceptions.Platform, method: String): List[String] =
      Exceptions.throws(platform).collectFirst { case (pattern, types) if pattern.r.matches(method) => types }
        .getOrElse(Nil)
    // ujson.read wraps its parse errors in a TraceException unless called with trace = false.
    assertEquals(
      thrown(Exceptions.Platform.JVM, "ujson.read").toSet,
      Set("upickle.core.TraceVisitor.TraceException", "ujson.ParsingFailedException")
    )
    assertEquals(thrown(Exceptions.Platform.JVM, "sttp.client4.Request.send"), List("sttp.client4.SttpClientException"))
    assert(thrown(Exceptions.Platform.JS, "sttp.client4.Request.send").contains("scala.scalajs.js.JavaScriptException"))
    assert(thrown(Exceptions.Platform.Native, "sttp.client4.Request.send").contains("java.lang.RuntimeException"))
  }

  test("boundaries where an exception is the expected outcome are left alone") {
    val found = warnings(
      """import java.net.URI
        |import java.nio.file.{Files, Path}
        |import scala.util.control.NonFatal
        |enum Failure extends Exception { case Timeout(message: String) }
        |class Credentials(val user: String) { require(user.nonEmpty) }
        |object Lints {
        |  def uri(value: String): Option[URI] = scala.util.Try(URI(value)).toOption
        |  def delete(path: Path): Either[String, Unit] =
        |    try Right(Files.delete(path)) catch { case e: java.io.IOException => Left(e.getMessage) }
        |  def notify(observer: String => Unit, value: String): Unit =
        |    try observer(value) catch { case NonFatal(_) => () }
        |  def failure(message: String): Failure = Failure.Timeout(message)
        |}""".stripMargin
    )
    val ids = Set(
      "local-throw",
      "throw-in-default",
      "throwing-helper",
      "partial-in-try",
      "redundant-try",
      "broad-catch",
      "discarded-cause"
    )
    assert(!found.exists(message => ids.exists(id => message.endsWith(s"[baumkuchen:$id]"))), found.mkString("\n"))
  }

  test("arrays compare by reference") {
    assertWarns(
      "object Lints { def check(left: Array[Int], right: Array[Int]) = left == right }",
      "arrays compare by reference"
    )
  }

  test("checks apply inside lambdas") {
    assertWarns(
      "object Lints { def check(rows: List[Array[Int]], left: Array[Int]) = rows.exists(row => row == left) }",
      "arrays compare by reference"
    )
  }

  test("null checks on arrays are left alone") {
    assertEquals(warnings("object Lints { def check(bytes: Array[Byte]) = bytes == null || null != bytes }"), Nil)
  }

  test("conversions to the same type do nothing") {
    val found = warnings(
      "object Lints { def check(count: Int, name: String, values: List[Int]) = (count.toInt, name.toString, values.toList, count.toLong) }"
    )
    assert(found.exists(_.contains("toInt on a value that is already Int")), found.mkString("\n"))
    assert(found.exists(_.contains("toString on a value that is already String")), found.mkString("\n"))
    assert(found.exists(_.contains("toList on a value that is already")), found.mkString("\n"))
    assert(!found.exists(_.contains("toLong")), found.mkString("\n"))
  }

  test("map with a function returning Unit should be foreach") {
    assertWarns("object Lints { def check(values: List[Int]) = values.map(value => println(value)) }", "use foreach")
  }

  test("futures mapped to Unit are left alone") {
    assertEquals(
      warnings(
        """import scala.concurrent.{ExecutionContext, Future}
          |object Lints { def check(value: Future[Int])(using ExecutionContext): Future[Unit] = value.map(_ => ()) }""".stripMargin
      ),
      Nil
    )
  }

  test("a future of a future is reported") {
    assertWarns(
      """import scala.concurrent.{ExecutionContext, Future}
        |object Lints { def nested(value: Future[Int])(using ExecutionContext) = value.map(count => Future(count)) }""".stripMargin,
      "a future of a future"
    )
  }

  test("positional arguments that are easy to misorder are reported") {
    val found = warnings(
      """object Geometry {
        |  def rectangle(width: Int, height: Int): Int                   = width * height
        |  def label(text: String, width: Int, centred: Boolean): String = if (centred) text else text.take(width)
        |  def mix(x: Double, y: Double): Double                          = x + y
        |  def colour(red: Int, green: Int, blue: Int, alpha: Int, gamma: Int): Int = red + green + blue + alpha + gamma
        |  def less(left: Int, right: Int): Boolean = left < right
        |  def record(name: String, count: Int, ratio: Double, mark: Char, size: Long): Int = count
        |  def command(name: String, help: String): String = name
        |}
        |object Calls {
        |  val width     = 3
        |  val height    = 4
        |  val ambiguous = Geometry.rectangle(3, 4)
        |  val named     = Geometry.rectangle(width = 3, height = 4)
        |  val matching  = Geometry.rectangle(width, height)
        |  val swapped   = Geometry.rectangle(height, width)
        |  val flag      = Geometry.label("x", 3, true)
        |  val namedFlag = Geometry.label("x", 3, centred = true)
        |  val generic   = Geometry.mix(1.0, 2.0)
        |  val library   = math.max(1, 2)
        |  val many      = Geometry.colour(1, 2, 3, 4, 5)
        |  val (red, green, blue, alpha, gamma, right) = (1, 2, 3, 4, 5, 6)
        |  val labelled    = Geometry.colour(red, green, blue, alpha, gamma)
        |  val repeated    = Geometry.less(right, right)
        |  val expressions = Geometry.rectangle(width + 1, height * 2)
        |  val distinct    = Geometry.record("a", 1, 2.0, 'c', 3L)
        |  val described   = Geometry.command("build", "Build the project")
        |  def minimum(left: Int, right: Int): Int = if (Geometry.less(right, left)) right else left
        |  val shifted     = 5
        |  val oneWay      = Geometry.less(right, shifted)
        |  val partial     = List(1).map(Geometry.rectangle(height, _))
        |  val expressed   = Geometry.rectangle(height, width * 2)
        |}""".stripMargin
    )
    assertEquals(
      found.filter(_.contains("share a type")),
      List(
        "width and height share a type; name the arguments so they cannot be swapped [baumkuchen:same-type-arguments]"
      )
    )
    assert(found.exists(_.contains("height is passed to width")), found.mkString("\n"))
    assert(found.exists(_.contains("width is passed to height")), found.mkString("\n"))
    assertEquals(found.count(_.contains("name the Boolean argument centred")), 1, found.mkString("\n"))
    assertEquals(found.count(_.contains("5 positional arguments")), 1, found.mkString("\n"))
    assertEquals(found.count(_.contains("is passed to")), 2, found.mkString("\n"))
  }

  test("type tests on open hierarchies") {
    val found = warnings(
      """trait Shape
        |sealed trait Colour
        |abstract class Animal
        |case class Square(side: Int) extends Shape
        |object Lints {
        |  def area(shape: Shape): Int = shape match { case square: Square => square.side; case _ => 0 }
        |  def open(value: Any): Boolean = value.isInstanceOf[Shape] || value.isInstanceOf[Square]
        |  def animal(value: Any): Int = value match { case _: Animal => 1; case _: Colour => 2; case _: Product => 3; case _ => 0 }
        |}""".stripMargin
    )
    assertEquals(found.count(_.contains("type test on Shape")), 1, found.mkString("\n"))
    assertEquals(found.count(_.contains("type test on Animal")), 1, found.mkString("\n"))
    assert(
      !found.exists(found => found.contains("Square") || found.contains("Colour") || found.contains("Product")),
      found.mkString("\n")
    )
  }

  test("type tests on types with an extractor are written as the extractor") {
    val found = warnings(
      """import scala.annotation.nowarn
        |case class Point(x: Int, y: Int)
        |case class Many(values: Int*)
        |case class Empty()
        |class Custom(val value: Int)
        |object Custom { def unapply(custom: Custom): Some[(Int, Int)] = Some((custom.value, custom.value)) }
        |class Partial(val value: Int)
        |object Partial { def unapply(partial: Partial): Option[Int] = Option.when(partial.value > 0)(partial.value) }
        |class Legacy(val value: Int)
        |object Legacy { @deprecated("use value", "1.0") def unapply(legacy: Legacy): Some[Int] = Some(legacy.value) }
        |enum Shape { case Circle(radius: Int); case Square }
        |trait Open
        |case class Box[A](value: A)
        |case class Three(a: Int, b: String, c: Double)
        |object Aliases { type Pt = Point; type Named = Point; val Named = Point }
        |object Lints {
        |  def kind(value: Any): Int = value match {
        |    case _: Point        => 1
        |    case _: Many         => 2
        |    case _: Empty        => 3
        |    case _: Custom       => 4
        |    case _: Shape.Circle => 5
        |    case Some(_: Point)  => 6
        |    case point: Point    => point.x
        |    case _: Open         => 7
        |    case _: String       => 8
        |    case _: Box[?]       => 9
        |    case _: Aliases.Pt   => 10
        |    case _: Aliases.Named => 11
        |    case _: Partial      => 12
        |    case _: Legacy       => 13
        |    case _: Three        => 14
        |    case _: (Int, String) => 15
        |    case _               => 0
        |  }
        |  @nowarn("msg=instead of the type test")
        |  def intended(value: Any): Boolean = value match {
        |    case _: Point => true
        |    case _        => false
        |  }
        |}""".stripMargin
    ).filter(_.contains("instead of the type test"))
    assertEquals(
      found.map(_.takeWhile(_ != ';')).sorted,
      List(
        "write Point(_, _) instead of the type test _: Point",
        "write Many(_*) instead of the type test _: Many",
        "write Empty() instead of the type test _: Empty",
        "write Custom(_, _) instead of the type test _: Custom",
        "write Shape.Circle(_) instead of the type test _: Shape.Circle",
        "write Box(_) instead of the type test _: Box[?]",
        "write (_, _) instead of the type test _: (Int, String)",
        "write Aliases.Named(_, _) instead of the type test _: Aliases.Named",
        "write Point(_, _) instead of the type test _: Point"
      ).sorted
    )
  }

  test("type tests on types with more fields than the wildcard limit are left alone") {
    val source =
      """case class Three(a: Int, b: String, c: Double)
                   |object Lints { def three(value: Any): Boolean = value match { case _: Three => true; case _ => false } }""".stripMargin
    assertEquals(warnings(source).filter(_.contains("instead of the type test")), Nil)
    assertEquals(
      warnings(source, "wildcards=3").filter(_.contains("instead of the type test")).map(_.takeWhile(_ != ';')),
      List("write Three(_, _, _) instead of the type test _: Three")
    )
  }

  test("patterns with more wildcards than the limit name the fields they match") {
    val source = """case class Wide(a: Int, b: String, c: Double, d: Long, e: Char)
                   |case class Pair(left: Int, right: Int)
                   |class Custom(val value: Int)
                   |object Custom { def unapply(custom: Custom): Some[(Int, Int, Int, Int)] = Some((1, 2, 3, 4)) }
                   |object Lints {
                   |  def wide(value: Any): String = value match {
                   |    case Wide(_, _, _, d, _)     => d.toString
                   |    case Wide(1, _, _, _, e)     => e.toString
                   |    case whole @ Wide(_, b, _, d, _) => whole.toString + b + d
                   |    case Wide(_, b, c, d, _)     => b + c + d
                   |    case Wide(_, _, _, _, _)     => "any"
                   |    case Pair(_, right)          => right.toString
                   |    case Custom(_, _, _, last)   => last.toString
                   |    case _                       => ""
                   |  }
                   |}""".stripMargin
    assertEquals(
      warnings(source).filter(_.contains("positional wildcards")).map(_.takeWhile(_ != ';')).sorted,
      List(
        "write Wide(d = d) instead of 4 positional wildcards",
        "write Wide(a = 1, e = e) instead of 3 positional wildcards",
        "write Wide(b = b, d = d) instead of 3 positional wildcards"
      ).sorted
    )
    assertEquals(warnings(source, "wildcards=3").filter(_.contains("positional wildcards")).size, 1)
  }

  test("the wildcard limit must be a count") {
    val (errors, _) = compile("Lints.scala", "object Lints", "wildcards=many")
    assertEquals(errors, List("baumkuchen option wildcards must be a non-negative count, not many"))
  }

  test("the ci mode makes the strict checks errors, unless a check is kept a warning") {
    val source = """trait Shape
                   |object Lints {
                   |  def check(left: Array[Int], right: Array[Int]) = left == right
                   |  def kind(value: Any): Int = value match { case _: Shape => 1; case _ => 0 }
                   |}""".stripMargin
    def levels(options: String*) = compile("Lints.scala", source, options*) match {
      case (errors, warnings) =>
        (errors.map(_.takeWhile(_ != ' ')).sorted, warnings.map(_.takeWhile(_ != ' ')).sorted)
    }
    assertEquals(levels("mode=ci"), (List("arrays"), List("type")))
    assertEquals(levels("mode=dev"), (Nil, List("arrays", "type")))
    assertEquals(levels(), (Nil, List("arrays", "type")))
    assertEquals(levels("mode=ci", "warn=array-equality"), (Nil, List("arrays", "type")))
    assertEquals(levels("error=*", "warn=open-type-test"), (List("arrays"), List("type")))
    assertEquals(
      compile("Lints.scala", "object Lints", "mode=release")._1,
      List("baumkuchen option mode must be one of dev, ci, not release")
    )
  }

  test("the strict checks are those whose findings have been real wherever they fired, and judgement calls warn") {
    assertEquals(
      Check.values.filter(_.strict).map(_.id).toSet,
      Set(
        "acyclic",
        "array-equality",
        "swapped-arguments",
        "nested-future",
        "long-arguments",
        "same-type-arguments",
        "boolean-argument",
        "extractor-type-test",
        "named-pattern",
        "redundant-conversion",
        "unit-map",
        "linear-in-loop",
        "local-throw",
        "throw-in-default",
        "throwing-helper",
        "partial-in-try",
        "redundant-try",
        "broad-catch",
        "discarded-cause"
      )
    )
  }

  test("generated sources are not linted") {
    val source = """import scala.concurrent.Future
                   |object Lints {
                   |  def check(left: Array[Int], right: Array[Int]) = left == right
                   |  def chain(rows: List[List[Int]]) = rows.map(_.map(_ + 1).filter(_ > 0))
                   |  def nested: Future[Future[Int]] = ???
                   |}""".stripMargin
    assertEquals(warningsAt("src/main/scala/Lints.scala", source).size, 3)
    assertEquals(warningsAt("src/generated/scala/Lints.scala", source), Nil)
    assertEquals(warningsAt("gen/Lints.scala", source, "generated=**/gen/**"), Nil)
    assertEquals(warningsAt("src/generated/scala/Lints.scala", source, "generated=**/gen/**").size, 3)
  }

  test("checks carry their id, and can be disabled, escalated or suppressed by it") {
    val source   = "object Lints { def check(left: Array[Int], right: Array[Int]) = left == right }"
    val expected = "arrays compare by reference; compare their elements with sameElements [baumkuchen:array-equality]"
    assertEquals(warnings(source), List(expected))
    assertEquals(warnings(source, "disable=array-equality"), Nil)
    assertEquals(compile("Lints.scala", source, "error=array-equality"), (List(expected), Nil))
    assertEquals(compile("Lints.scala", source, "error=*"), (List(expected), Nil))
    assertEquals(
      warnings(
        """import scala.annotation.nowarn
          |object Lints {
          |  @nowarn("msg=baumkuchen:array-equality")
          |  def check(left: Array[Int], right: Array[Int]) = left == right
          |}""".stripMargin
      ),
      Nil
    )
  }

  private def tagged(found: List[String], check: String): List[String] =
    found.filter(_.endsWith(s"[baumkuchen:$check]")).map(_.takeWhile(_ != ';')).sorted

  test("linear operations inside loops are reported, while the same calls outside a loop are not") {
    val found = warnings(
      """object Lints {
        |  def indexed(values: List[Int]): Int = (0 until 10).map(index => values(index)).sum
        |  def lengths(values: List[Int]): Int = {
        |    var total = 0
        |    var index = 0
        |    while (index < 10) { total += values.length; index += 1 }
        |    total
        |  }
        |  def member(values: Seq[Int], keys: Vector[Int]): Int = keys.count(key => values.contains(key))
        |  def appended(values: Vector[Int]): List[Int] = values.foldLeft(List.empty[Int])((list, value) => list :+ value)
        |  def concatenated(values: Vector[Int]): List[Int] =
        |    values.foldLeft(List.empty[Int])((list, value) => list ++ List(value))
        |  def fine(values: Vector[Int], keys: Set[Int]): Int = values.count(value => keys.contains(value) && values(0) > 0)
        |  def outside(values: List[Int]): Int = values(0) + values.length
        |  def constant(keys: Vector[String]): Int = keys.count(key => Seq("a", "b").contains(key))
        |  def optional(value: Option[Int], values: List[Int]): Option[Int] = value.map(index => values(index))
        |  def perElement(rows: Vector[List[Int]]): Int = rows.map(row => row.length + row(0)).sum
        |  def defined(values: Vector[Int], keys: Seq[Int]): Int =
        |    values.count { value => val local = keys.filter(_ > value); local.contains(value) }
        |  def distinct(values: Vector[Int]): List[Int] =
        |    values.foldLeft(List.empty[Int])((seen, value) => if (seen.contains(value)) seen else value :: seen)
        |  def immutable(values: Vector[Int], keys: scala.collection.immutable.ArraySeq[Int]): Int =
        |    values.count(value => keys.contains(value))
        |  def matched(pairs: Vector[(List[Int], Int)]): Int = pairs.map { case (row, index) => row.length + index }.sum
        |  def paired(values: Vector[(Int, Int)]): List[Int] =
        |    values.foldLeft(List.empty[Int]) { case (list, (value, _)) => list :+ value }
        |  def hoisted(values: Vector[Int], older: Vector[Int], newer: Vector[Int]): Int =
        |    values.count(value => (older ++ newer).exists(_ == value))
        |  def extended(values: Vector[Int], older: Vector[Int]): Vector[Vector[Int]] = values.map(value => older ++ Vector(value))
        |  def ranged(count: Int): Vector[Int] = Vector.range(0, count)
        |  def computed(values: Vector[Int]): Vector[Vector[Int]] = values.map(value => ranged(1) ++ ranged(2))
        |  def accumulated(values: Vector[Int]): Vector[Int] =
        |    values.foldLeft(Vector.empty[Int]) { case (done, value) => done :+ value }
        |  def fixed(keys: Vector[Int], first: Int, second: Int, extra: List[Int]): Vector[Int] =
        |    keys.filter(key => Seq(first, second).contains(key)).flatMap(key => List(first, second) ++ extra.take(key))
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "linear-in-loop"),
      List(
        "apply on a List walks it on every iteration",
        "length on a List walks it on every iteration",
        "contains on a Seq scans it on every iteration",
        "contains on a List scans it on every iteration",
        "contains on an ArraySeq scans it on every iteration",
        ":+ on a List copies it on every iteration",
        ":+ on a List copies it on every iteration",
        "++ on a List copies it on every iteration",
        "++ on a Vector rebuilds the same collection on every iteration"
      ).sorted
    )
  }

  test("primitives passed to generic parameters inside loops are reported as boxing") {
    val found = warnings(
      """import scala.collection.mutable
        |object Lints {
        |  def buffer(values: Array[Long]): Int = {
        |    val buffer = mutable.ArrayBuffer.empty[Long]
        |    values.foreach(value => buffer += value)
        |    buffer.size
        |  }
        |  def indices(values: Array[Int]): Int = {
        |    val buffer = mutable.ArrayBuffer.empty[Int]
        |    values.foreach(value => buffer += value)
        |    buffer.size
        |  }
        |  def weights(values: Array[Double], totals: mutable.Map[Double, Int]): Unit = values.foreach(value => totals(value) = 1)
        |  def table(values: Array[Long], keys: Map[Long, String]): Int = values.count(value => keys.contains(value))
        |  def printed(values: Array[Int]): Unit = values.foreach(value => println(value))
        |  def applied(values: Array[Int], f: Int => Int): Int = values.map(value => f(value)).sum
        |  def once(value: Int): Int = {
        |    val buffer = mutable.ArrayBuffer.empty[Int]
        |    buffer += value
        |    buffer.size
        |  }
        |  def cached(values: Array[String], counts: Map[String, Long]): Long = values.map(value => counts.getOrElse(value, 0L)).sum
        |  def uncached(values: Array[String], counts: Map[String, Long]): Long = values.map(value => counts.getOrElse(value, 1000L)).sum
        |  def tupled(values: Array[Int]): Array[(Int, Int)] = values.map(value => (value, value))
        |  def wrapped[A](value: A): List[A] = List(value)
        |  def generic(values: Array[Int]): Int = values.map(value => wrapped(value).size).sum
        |  def array(values: Array[Long], keys: Array[Long]): Int = values.count(value => keys.contains(value))
        |  def references(values: Array[String]): Int = {
        |    val buffer = mutable.ArrayBuffer.empty[String]
        |    values.foreach(value => buffer += value)
        |    buffer.size
        |  }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "loop-boxing"),
      List(
        "value is boxed for the generic parameter elem of += on every iteration",
        "1000L is boxed for the generic parameter default of getOrElse on every iteration",
        "value is boxed for the generic parameter key of update on every iteration",
        "value is boxed for the generic parameter key of contains on every iteration",
        "value is boxed for the generic parameter elem of contains on every iteration"
      ).sorted
    )
  }

  test("chains of strict collection transformations inside loops are reported once") {
    val found = warnings(
      """object Lints {
        |  def chain(rows: Vector[List[Int]]): Int = rows.map(values => values.map(_ + 1).filter(_ > 2).map(_ * 2).sum).sum
        |  def array(rows: Vector[Array[Int]]): Int = rows.map(values => values.filter(_ > 0).map(_ + 1).length).sum
        |  def once(values: List[Int]): Int = values.map(_ + 1).filter(_ > 2).sum
        |  def single(rows: Vector[List[Int]]): Int = rows.map(values => values.map(_ + 1).sum).sum
        |  def lazily(rows: Vector[List[Int]]): Int = rows.map(values => values.view.map(_ + 1).filter(_ > 2).sum).sum
        |  def iterated(rows: Vector[Iterator[Int]]): Int = rows.map(values => values.map(_ + 1).filter(_ > 2).sum).sum
        |  def optional(rows: Vector[Option[Int]]): Int = rows.map(value => value.map(_ + 1).filter(_ > 2).sum).sum
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "strict-chain"),
      List(
        "map then filter builds an intermediate collection",
        "filter then map builds an intermediate collection"
      ).sorted
    )
  }

  test("named patterns and named arguments are already named") {
    val found = warnings(
      """case class Bookmarks(userId: Long, folder: String, count: Int, order: Int, shared: Boolean)
        |case class Span(begin: Int, end: Int)
        |case class Stride(begin: Int, end: Int, step: Int = 1)
        |class Plain(val begin: Int, val end: Int)
        |case class Wide(begin: Long, end: Long)
        |case class Interval[A](begin: A, end: A)
        |object Ranged { def apply(begin: Long, end: Long): String = "both"; def apply(begin: Long): String = "open" }
        |final case class Header(unit: String, ranges: List[Long])
        |object Header {
        |  def apply(unit: String, first: Long, rest: Long*): Header = Header(unit, first :: rest.toList)
        |  def apply(first: Long, rest: Long*): Header = Header("bytes", first :: rest.toList)
        |  def apply(begin: Long, end: Long): Header = Header("bytes", List(begin, end))
        |  def apply(begin: Long): Header = Header("bytes", List(begin))
        |}
        |class Typed[A](val begin: Int, val end: Int, val tag: A)
        |class Built private (val begin: Int, val end: Int)
        |object Built { def apply(begin: Int, end: Int): Built = new Built(begin, end) }
        |object Lints {
        |  def owner(value: Any): Long = value match {
        |    case Bookmarks(userId = userId) => userId
        |    case _                          => 0L
        |  }
        |  def span: Span = Span(begin = 2, end = 4)
        |  def reordered: Span = Span(end = 4, begin = 2)
        |  def stride: Stride = Stride(begin = 2, end = 4)
        |  def plain: Plain = Plain(begin = 2, end = 4)
        |  def wide: Wide = Wide(begin = 2, end = 4)
        |  def interval: Interval[Int] = Interval(begin = 2, end = 4)
        |  def nested: List[Wide] = List(Wide(begin = 2, end = 4))
        |  def overloaded: String = Ranged(begin = 2, end = 4)
        |  def header: Header = Header(begin = 20, end = 30)
        |  def library: dotty.tools.dotc.util.Spans.Span = dotty.tools.dotc.util.Spans.Span(start = 2, end = 4)
        |  def typed: Typed[String] = Typed(begin = 2, end = 4, tag = "t")
        |  def built: Built = Built(begin = 2, end = 4)
        |  def created: Plain = new Plain(begin = 2, end = 4)
        |  def flags: Boolean = Bookmarks(1L, "a", count = 2, order = 3, shared = true).shared
        |}""".stripMargin
    )
    assertEquals(found.filter(_.contains("baumkuchen:named-pattern")), Nil)
    assertEquals(found.filter(_.contains("baumkuchen:same-type-arguments")), Nil)
    assertEquals(found.filter(_.contains("baumkuchen:boolean-argument")), Nil)
  }

  test("patterns the compiler writes, arguments of the wrong type and generic values are not misreported") {
    val found = warnings(
      """trait Source
        |class Account extends Source
        |case class Lane(name: String, source: Source, anonymous: Boolean, assigned: Int)
        |object Lints {
        |  def pick(first: Option[Int], fallback: Account): Option[Source] = for {
        |    value          <- first
        |    anonymous      = fallback
        |    client: Source = Option(anonymous).getOrElse(anonymous)
        |    other          <- first
        |  } yield client
        |  def lane(anonymous: Source, assigned: Int): Lane = Lane("anonymous", anonymous, anonymous = true, assigned)
        |  def same[A](obtained: A, expected: A): Boolean = obtained == expected
        |  def check(flag: Boolean): Boolean = same(flag, false)
        |}""".stripMargin,
      "-source:3.7"
    )
    assertEquals(found.filter(_.contains("[baumkuchen:")), Nil)
  }

  test("side effects a method returning an effect performs when called, rather than when the effect runs") {
    val found = warnings(
      """import java.util.concurrent.atomic.AtomicBoolean
        |trait Monad[F[_]] {
        |  def unit[A](value: A): F[A]
        |  def map[A, B](effect: F[A])(function: A => B): F[B]
        |  def eval[A](value: => A): F[A]
        |}
        |trait Entropy { def fill(bytes: Array[Byte]): Unit }
        |object Refined {
        |  opaque type Positive[A] = A
        |  def apply[A](value: A): Positive[A] = value
        |}
        |class Session {
        |  private val used  = AtomicBoolean(false)
        |  private var count = 0
        |  def complete[F[_]](monad: Monad[F]): F[Int] = if (!used.compareAndSet(false, true)) monad.unit(0) else monad.unit(1)
        |  def counted[F[_]](monad: Monad[F]): F[Int] = { count += 1; monad.unit(count) }
        |  def logged[F[_]](monad: Monad[F]): F[Int] = { println("x"); monad.unit(1) }
        |  def drawn[F[_]](monad: Monad[F], random: java.util.Random): F[Int] = { val value = random.nextInt(); monad.unit(value) }
        |  def begin[F[_]](monad: Monad[F], entropy: Entropy): F[String] = { val verifier = token(entropy); monad.unit(verifier) }
        |  def optional[F[_]](monad: Monad[F], value: Option[Int]): F[Int] = monad.unit(value.map { v => count += v; v }.getOrElse(0))
        |  def suspended[F[_]](monad: Monad[F]): F[Int] = monad.eval { count += 1; count }
        |  def mapped[F[_]](monad: Monad[F], effect: F[Int]): F[Int] = monad.map(effect) { value => used.set(true); value }
        |  def local[F[_]](monad: Monad[F]): F[Int] = {
        |    val buffer = scala.collection.mutable.ArrayBuffer.empty[Int]
        |    buffer.clear()
        |    var total = 0
        |    total += 1
        |    require(total > 0)
        |    monad.unit(total)
        |  }
        |  def chained[F[_]](monad: Monad[F]): F[Int] = complete(monad)
        |  def plain(): Int = { count += 1; count }
        |  def refined(value: Int): Refined.Positive[Int] = { count += 1; Refined(value) }
        |  def named(value: Int): (size: Int, total: Int) = { count += 1; (size = value, total = count) }
        |  private def token(entropy: Entropy): String = {
        |    val bytes = Array.ofDim[Byte](4)
        |    entropy.fill(bytes)
        |    bytes.mkString
        |  }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "eager-effect"),
      List(
        "begin returns F but calls token, which calls fill when called, not when the F runs",
        "complete returns F but calls compareAndSet when called, not when the F runs",
        "counted returns F but assigns count when called, not when the F runs",
        "drawn returns F but draws nextInt when called, not when the F runs",
        "logged returns F but calls println when called, not when the F runs",
        "optional returns F but assigns count when called, not when the F runs"
      ).sorted
    )
  }

  test("blocking work on shared pools, unbounded waits, orphan threads and hand-written joins") {
    val found = warnings(
      """import java.util.concurrent.CompletableFuture
        |import java.util.concurrent.TimeUnit
        |import scala.concurrent.ExecutionContext
        |import scala.concurrent.Future
        |import scala.concurrent.duration.Duration
        |object Threads {
        |  def line(reader: java.io.BufferedReader): CompletableFuture[String] = CompletableFuture.supplyAsync(() => reader.readLine())
        |  def pooled(reader: java.io.BufferedReader, executor: java.util.concurrent.Executor): CompletableFuture[String] =
        |    CompletableFuture.supplyAsync(() => reader.readLine(), executor)
        |  def computed(): CompletableFuture[Int] = CompletableFuture.supplyAsync(() => 1 + 1)
        |  def slept(): Future[Unit] = Future(Thread.sleep(1))(ExecutionContext.global)
        |  def wrapped(): Future[Unit] = Future(scala.concurrent.blocking(Thread.sleep(1)))(ExecutionContext.global)
        |  def waited(future: CompletableFuture[Int], process: Process): Int = { process.waitFor(); future.get() + future.join() }
        |  def timed(future: CompletableFuture[Int], process: Process): Int = {
        |    process.waitFor(1, TimeUnit.SECONDS)
        |    future.get(1, TimeUnit.SECONDS)
        |  }
        |  def awaited(future: Future[Int]): Int = scala.concurrent.Await.result(future, Duration.Inf)
        |  def limited(future: Future[Int]): Int = scala.concurrent.Await.result(future, Duration(1, "s"))
        |  def fired(): Unit = Thread(() => ()).start()
        |  def daemon(): Unit = { val worker = Thread(() => ()); worker.setDaemon(true); worker.start() }
        |  def joined(): Unit = { val worker = Thread(() => ()); worker.start(); worker.join(1000) }
        |  def kept(): Thread = { val worker = Thread(() => ()); worker.start(); worker }
        |  def deep(): Unit = { val worker = Thread(null, () => (), "deep", 1L << 30); worker.start(); worker.join() }
        |  def foreign(worker: Thread): Unit = worker.join()
        |  def completed(future: CompletableFuture[Int]): Option[Int] = Option.when(future.isDone)(future.join())
        |  def computed(task: java.util.concurrent.FutureTask[Int]): Int = { task.run(); task.get() }
        |  def killed(process: Process): Unit = { val _ = process.destroyForcibly(); val _ = process.waitFor() }
        |  def await(worker: Thread): Unit = worker.join()
        |  def awaitTermination(worker: Thread): Unit = worker.join()
        |  def task(): Runnable = new Runnable { def run(): Unit = () }
        |  def limited(future: CompletableFuture[Int]): Int = future.orTimeout(1, TimeUnit.SECONDS).join()
        |  def defaulted(future: CompletableFuture[Int]): Int = future.completeOnTimeout(0, 1, TimeUnit.SECONDS).get()
        |  def bounded(future: CompletableFuture[Int]): Int = { future.orTimeout(1, TimeUnit.SECONDS); future.join() }
        |  @annotation.tailrec
        |  def uninterruptibly[A](future: CompletableFuture[A], interrupted: Boolean = false): A =
        |    scala.util.Try(future.get()) match {
        |      case scala.util.Success(value) =>
        |        if (interrupted) Thread.currentThread.interrupt()
        |        value
        |      case scala.util.Failure(_: InterruptedException) => uninterruptibly(future, interrupted = true)
        |      case scala.util.Failure(error)                   => throw error
        |    }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "blocking-common-pool"),
      List(
        "a Future on the global pool runs sleep outside blocking { }, holding one of its threads per core",
        "supplyAsync without an executor runs readLine on the common pool, which has a thread per core"
      )
    )
    assertEquals(
      tagged(found, "unbounded-wait"),
      List(
        "get waits with no limit, so a stuck task hangs its caller",
        "get waits with no limit, so a stuck task hangs its caller",
        "join waits with no limit, so a stuck task hangs its caller",
        "join waits with no limit, so a stuck task hangs its caller",
        "result waits with no limit, so a stuck task hangs its caller",
        "waitFor waits with no limit, so a stuck task hangs its caller"
      )
    )
    assertEquals(
      tagged(found, "orphan-thread"),
      List(
        "the thread started here is never joined or interrupted",
        "worker is started but never joined or interrupted"
      )
    )
    assertEquals(
      tagged(found, "reimplemented-join"),
      List(
        "get retried through interrupts, restoring the interrupt flag afterwards, reimplements CompletableFuture.join"
      )
    )
  }

  test("closeable locals closed after statements that can fail first") {
    val found = warnings(
      """class Resource extends AutoCloseable { def use(): Int = 1; def close(): Unit = () }
        |class Backend { def send(): Int = 1; def close(): Unit = () }
        |object Closing {
        |  def leaky(): Int = { val resource = Resource(); val result = resource.use(); resource.close(); result }
        |  def structural(): Int = { val backend = Backend(); val result = backend.send(); backend.close(); result }
        |  def immediate(): Unit = { val resource = Resource(); resource.close() }
        |  def guarded(): Int = { val resource = Resource(); try resource.use() finally resource.close() }
        |  def managed(): Int = scala.util.Using.resource(Resource())(_.use())
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "unguarded-close"),
      List(
        "backend is closed only when the statements before close succeed",
        "resource is closed only when the statements before close succeed"
      )
    )
  }

  test("control flow decided by the message text of an error") {
    val source =
      """case class Rejected(message: String, code: Int)
        |object Classify {
        |  def timeout(error: Throwable): Int = if (error.getMessage.contains("timed out")) 1 else 0
        |  def same(error: Throwable): Int = if (error.getMessage == "boom") 1 else 0
        |  def rejected(rejection: Rejected): Int = if ("gone" == rejection.message) 1 else 0
        |  def guarded(rejection: Rejected): Int = rejection match {
        |    case Rejected(text, _) if text == "gone" => 1
        |    case Rejected(_, code) if code == 2      => 2
        |    case _                                   => 0
        |  }
        |  def caught(body: => Int): Int = try body catch { case error: Exception if error.getMessage.startsWith("x") => 0 }
        |  def asserted(error: Throwable): Unit = assert(!error.getMessage.contains("secret"))
        |  def shown(error: Throwable): String = error.getMessage + "!"
        |  def dynamic(error: Throwable, expected: String): Int = if (error.getMessage.contains(expected)) 1 else 0
        |  def unrelated(text: String): Int = if (text.contains("x")) 1 else 0
        |}""".stripMargin
    val lines =
      positioned(source).collect { case (message, line, _) if message.contains("message-classification") => line }
    assertEquals(lines.sorted, List(2, 3, 4, 6, 10))
  }

  test("private objects used only for their identity") {
    val found = warnings(
      """final case class Callback(code: String, binding: AnyRef)
        |class Session {
        |  private val binding: AnyRef = new Object()
        |  def callback(code: String): Callback = Callback(code, binding)
        |  def claim(callback: Callback): Boolean = callback.binding eq binding
        |}
        |class Constructed {
        |  private val binding = Object()
        |  def callback(code: String): Callback = new Callback(code, binding)
        |  def claim(callback: Callback): Boolean = !(callback.binding ne binding)
        |}
        |class Locked {
        |  private val lock = new Object()
        |  def run(): Int = lock.synchronized(1)
        |}
        |class Shared {
        |  private val marker: AnyRef = new Object()
        |  def same(other: AnyRef): Boolean = other eq marker
        |  def exposed: AnyRef = marker
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "identity-token"),
      List("binding is an Object() compared only by identity", "binding is an Object() compared only by identity")
    )
  }

  test("private fields that are only written, checked or described") {
    val found = warnings(
      """class Session(code: String) {
        |  private var attempts = 0
        |  private val state    = code.reverse
        |  private val label    = code.toUpperCase
        |  private var seen     = 0
        |  private val limit    = 3
        |  private val base     = 10
        |  private val derived  = base * 2
        |  require(state.nonEmpty)
        |  def attempt(): Unit = { attempts += 1; seen = seen + 1 }
        |  def remaining: Int = limit - seen + derived
        |  override def toString: String = s"Session($label)"
        |}
        |final case class Product(private val hidden: Int)
        |class Suite {
        |  private val fixture = List(1, 2)
        |  private val bound   = 2
        |  def test(name: String)(body: => Unit): Unit = ()
        |  test("sizes") { assert(fixture.size == 2) }
        |  require(List(1).forall(_ < bound))
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "write-only-field"),
      List(
        "attempts is written but never read",
        "label is only described, never used",
        "state is only checked, never used"
      )
    )
  }

  test("every check is suppressed by @nowarn or @SuppressWarnings naming its id on an enclosing definition") {
    val bodies = Map(
      "unbounded-wait"       -> "def wait(future: java.util.concurrent.CompletableFuture[Int]): Int = future.join()",
      "blocking-common-pool" ->
        "def line(reader: java.io.BufferedReader) = java.util.concurrent.CompletableFuture.supplyAsync(() => reader.readLine())",
      "orphan-thread"   -> "def fire(): Unit = Thread(() => ()).start()",
      "unguarded-close" ->
        "def leak(): Int = { val stream = java.io.ByteArrayInputStream(Array[Byte](1)); val read = stream.read(); stream.close(); read }",
      "message-classification" ->
        "def timeout(error: Throwable): Int = if (error.getMessage == \"timed out\") 1 else 0",
      "reimplemented-join" ->
        """def await(future: java.util.concurrent.CompletableFuture[Int]): Int =
          |    try future.get() catch { case _: InterruptedException => Thread.currentThread.interrupt(); 0 }""".stripMargin,
      "eager-effect" -> "def effect[F[_]](unit: Int => F[Int]): F[Int] = { println(1); unit(1) }"
    )
    bodies.foreach { (id, body) =>
      val plain = warnings(s"object Lints {\n  $body\n}")
      assert(plain.exists(_.contains(s"baumkuchen:$id")), s"$id\n${plain.mkString("\n")}")
      List(s"@scala.annotation.nowarn(\"msg=baumkuchen:$id\")", s"@SuppressWarnings(Array(\"baumkuchen:$id\"))")
        .foreach { annotation =>
          val found = warnings(s"object Lints {\n  $annotation\n  $body\n}")
          assert(!found.exists(_.contains(s"baumkuchen:$id")), s"$annotation\n${found.mkString("\n")}")
        }
    }
    val escalated = "object Lints {\n  @SuppressWarnings(Array(\"baumkuchen\"))\n  " + bodies("unbounded-wait") + "\n}"
    assertEquals(compile("Lints.scala", escalated, "error=unbounded-wait")._1, Nil)
    val token =
      "class Session { private val binding: AnyRef = new Object(); def same(other: AnyRef): Boolean = other eq binding }"
    assert(warnings(token).exists(_.contains("baumkuchen:identity-token")))
    assertEquals(
      warnings("@SuppressWarnings(Array(\"baumkuchen:identity-token\"))\n" + token).filter(_.contains("identity")),
      Nil
    )
  }

  test("the copies of the sources mutation testing compiles from its temporary directory are not checked") {
    val source = "object Lints {\n  def same(a: Array[Int], b: Array[Int]): Boolean = a == b\n}"
    assertEquals(tagged(warningsAt("src/main/scala/Lints.scala", source), "array-equality").size, 1)
    assertEquals(tagged(warningsAt("stryker4s-8113/src/main/scala/Lints.scala", source), "array-equality"), Nil)
  }
}
