package net.kurobako.bonsai.plugin

import java.nio.file.Files
import munit.FunSuite
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.effects.*

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
    val (errors, warnings, _, content) = report(path, source, options*)
    (errors, warnings.map(_._1), content)
  }

  /** The information, warnings and errors the checks report, tagged with their levels. */
  private def diagnostics(source: String, options: String*): List[String] = {
    val (errors, warnings, infos, _) = report("Lints.scala", source, options*)
    errors.map("error: " + _) ++ warnings.map("warning: " + _._1) ++ infos.map("info: " + _)
  }

  /** Warnings with their lines and columns, since a console reporter shows only one warning per position. */
  private def positioned(source: String): List[(String, Int, Int)] =
    report("Lints.scala", source)._2.map((message, line, column, _) => (message, line, column))

  /** Warnings with the lines their positions start and end on. */
  private def spanned(source: String): List[(String, Int, Int)] =
    report("Lints.scala", source)._2.map((message, line, _, end) => (message, line, end))

  private def report(path: String, source: String, options: String*)
      : (List[String], List[(String, Int, Int, Int)], List[String], String) =
    Compilations.run(Seq(path -> source), options) { (result, directory) =>
      (
        result.allErrors.map(_.message).toList,
        result.allWarnings.map(warning =>
          (warning.message, warning.pos.line, warning.pos.column, if (warning.pos.exists) warning.pos.endLine else -1)
        ).toList,
        result.removeBufferedMessages(using dotty.tools.dotc.core.Contexts.NoContext).collect {
          case info if info.level == dotty.tools.dotc.interfaces.Diagnostic.INFO => info.message
        },
        Files.readString(directory.resolve(path))
      )
    }

  private def assertWarns(source: String, fragment: String): Unit = {
    val found = warnings(source)
    assert(found.exists(_.contains(fragment)), found.mkString("\n"))
  }

  test("with -rewrite, argument names, extractors and dropped conversions are written in") {
    assume(Compilations.minor >= 7, "named patterns need Scala 3.7")
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
    found.filter(_.endsWith(s"[bonsai:$check]"))

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

  test("a helper whose exception another call in the same try throws through its own helper is not reported") {
    val found = warnings(
      """object Lexing {
        |  case class Failure(at: Int) extends RuntimeException
        |  class Cursor(tokens: Array[Char]) {
        |    private def fail(): Nothing = throw Failure(1)
        |    def program(): Int = if (tokens.isEmpty) fail() else 1
        |  }
        |  def parse(s: String): Either[Int, Int] = try Right(Cursor(lex(s)).program()) catch { case Failure(at) => Left(at) }
        |  private def lex(s: String): Array[Char] = if (s.contains('\u0000')) throw Failure(0) else s.toCharArray
        |}""".stripMargin
    )
    assertEquals(lines(found, "throwing-helper"), Nil, found.mkString("\n"))
  }

  test("a Try around a call that already returns an Option, flattened, conflates failure with absence") {
    val found = diagnostics(
      """import scala.util.Try
        |object Lints {
        |  def acquire(path: String): Option[String] = if (path.isEmpty) None else Some(path)
        |  def lock(path: String): Option[String] = Try(acquire(path)).toOption.flatten
        |  def orNone(path: String): Option[String] = Try(acquire(path)).getOrElse(None)
        |  def orOther(path: String): Option[String] = Try(acquire(path)).getOrElse(Some("other"))
        |  def sized(path: String): Option[Int] = Try(path.toInt).toOption
        |  def plain(path: String): Option[String] = acquire(path)
        |  def listed(xs: List[Option[Int]]): List[Int] = xs.flatten
        |  def either(path: String): Option[Int] = Try(path.toInt).toOption.flatMap(Some(_))
        |}""".stripMargin
    ).filter(_.contains("as the absence it returns itself"))
    assertEquals(found.size, 2, found.mkString("\n"))
    assert(found.head.startsWith("info: "), found.head)
    assert(found.head.contains("acquire"), found.head)
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
    def thrown(platform: Throwing.Platform, method: String): List[String] =
      Throwing.throws(platform).collectFirst { case (pattern, types) if pattern.r.matches(method) => types }
        .getOrElse(Nil)
    // ujson.read wraps its parse errors in a TraceException unless called with trace = false.
    assertEquals(
      thrown(Throwing.Platform.JVM, "ujson.read").toSet,
      Set("upickle.core.TraceVisitor.TraceException", "ujson.ParsingFailedException")
    )
    assertEquals(thrown(Throwing.Platform.JVM, "sttp.client4.Request.send"), List("sttp.client4.SttpClientException"))
    assert(thrown(Throwing.Platform.JS, "sttp.client4.Request.send").contains("scala.scalajs.js.JavaScriptException"))
    assert(thrown(Throwing.Platform.Native, "sttp.client4.Request.send").contains("java.lang.RuntimeException"))
  }

  test("a broad catch around partial calls does not advise a narrower catch that would miss their exceptions") {
    val found = warnings(
      """import java.time.LocalDate
        |import scala.util.control.NonFatal
        |object Lints {
        |  def mixed(t: String, m: Map[String, Int]): Either[String, Int] =
        |    try Right(LocalDate.parse(t).getYear + m(t)) catch { case NonFatal(e) => Left(e.getMessage) }
        |  def pure(t: String): Either[String, Int] =
        |    try Right(LocalDate.parse(t).getYear) catch { case NonFatal(e) => Left(e.getMessage) }
        |}""".stripMargin
    )
    val broad = lines(found, "broad-catch")
    assertEquals(broad.size, 2, found.mkString("\n"))
    val (withPartial, without) = broad.partition(_.contains("replace"))
    assertEquals(withPartial.size, 1, found.mkString("\n"))
    assert(withPartial.head.contains("apply throws exceptions a catch for what parse throws would miss"), withPartial)
    assert(withPartial.head.contains("replace it with get first"), withPartial)
    assert(!withPartial.head.contains("DateTimeParseException"), withPartial)
    assert(without.head.contains("catch DateTimeParseException, which parse throws"), without)
  }

  test("messages name one call in the singular and several in the plural") {
    val found = warnings(
      """object Lints {
        |  def one(xs: List[Int]): Either[String, Int] =
        |    try Right(xs.head) catch { case _: NoSuchElementException => Left("empty") }
        |  def two(xs: List[Int], ys: List[Int]): Either[String, Int] =
        |    try Right(xs.head + ys.last) catch { case _: NoSuchElementException => Left("empty") }
        |}""".stripMargin
    )
    val partial = lines(found, "partial-in-try")
    assertEquals(partial.size, 2, found.mkString("\n"))
    assert(partial.exists(_.startsWith("head throws for missing")), partial)
    assert(partial.exists(_.startsWith("head, last throw for missing")), partial)
  }

  test("a partial function of an Either or Option does not cover its receiver, one in a later argument list does") {
    val found = warnings(
      """object Lints {
        |  implicit class Recovering[A, B](private val either: Either[A, B]) {
        |    def recoverWith(pf: PartialFunction[A, Either[A, B]]): Either[A, B] = either match {
        |      case Left(error) if pf.isDefinedAt(error) => pf(error)
        |      case _                                   => either
        |    }
        |  }
        |  def attempt[A](body: => A)(handler: PartialFunction[Throwable, A]): A = try body catch handler
        |  def value(xs: List[String]): Either[Throwable, String] = Right(xs.head).recoverWith { case _ => Right("b") }
        |  def first(xs: List[String]): Option[String] = Some(xs.head).collect { case _ => "b" }
        |  def later(xs: List[String]): String = attempt(xs.head)({ case _ => "b" })
        |}""".stripMargin
    )
    val partial = lines(found, "partial-in-try")
    assertEquals(partial.size, 1, found.mkString("\n"))
    assert(partial.head.startsWith("head throws for missing"), partial)
  }

  test("error handlers of cats that drop the error for a unit are reported") {
    val found = warnings(
      """package cats.effect {
        |  class IO[A] {
        |    def handleError(f: Throwable => A): IO[A]              = this
        |    def handleErrorWith(f: Throwable => IO[A]): IO[A]      = this
        |    def recover(f: PartialFunction[Throwable, A]): IO[A]   = this
        |    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = ???
        |    def attempt: IO[Either[Throwable, A]]                 = ???
        |    def void: IO[Unit]                                     = ???
        |  }
        |  object IO {
        |    val unit: IO[Unit]            = new IO[Unit]
        |    def pure[A](a: A): IO[A]      = new IO[A]
        |  }
        |}
        |package demo {
        |  import cats.effect.IO
        |  object Lints {
        |    def log(error: Throwable): Unit = ()
        |    def swallowed(io: IO[Unit]): List[IO[Unit]] = List(
        |      io.handleErrorWith(_ => IO.unit),
        |      io.handleErrorWith(_ => IO.pure(())),
        |      io.handleError(_ => ()),
        |      io.recover { case _ => () },
        |      io.redeem(_ => (), _ => ()),
        |      io.attempt.void
        |    )
        |    def kept(io: IO[Unit], other: IO[Int]): List[IO[?]] = List(
        |      io.handleError(error => log(error)),
        |      io.handleErrorWith(error => IO.pure(log(error))),
        |      io.handleError { _ => println("failed"); () },
        |      io.recover { case _: java.io.IOException => () },
        |      other.handleError(_ => 0),
        |      other.attempt,
        |      io.void
        |    )
        |    @SuppressWarnings(Array("bonsai:discarded-effect-error"))
        |    def expected(io: IO[Unit]): IO[Unit] = io.handleErrorWith(_ => IO.unit)
        |  }
        |}""".stripMargin
    )
    val swallowed = lines(found, "discarded-effect-error")
    assertEquals(
      swallowed.map(_.takeWhile(_ != ' ')).sorted,
      List("handleErrorWith", "handleErrorWith", "handleError", "recover", "redeem", "attempt.void").sorted,
      found.mkString("\n")
    )
    assert(swallowed.forall(_.contains("@SuppressWarnings(Array(\"bonsai:discarded-effect-error\"))")), swallowed)
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
    assert(!found.exists(message => ids.exists(id => message.endsWith(s"[bonsai:$id]"))), found.mkString("\n"))
  }

  test("JDK types of the unsupported module are reported where the code names them") {
    val found = warnings(
      """import sun.misc.Signal
        |object Lints {
        |  def handler(signal: Signal): Unit = ()
        |}""".stripMargin
    )
    assertEquals(lines(found, "jdk-internal").size, 2, found.mkString("\n"))
    assert(
      lines(found, "jdk-internal").forall(_.contains("in the unsupported module jdk.unsupported")),
      found.mkString("\n")
    )
  }

  test("classes named by a constant string are reported when the JDK does not export their package") {
    val found = warnings(
      """object Lints {
        |  def load(): Class[?] = Class.forName("jdk.internal.misc.VM")
        |  def list(): Class[?] = Class.forName("java.util.ArrayList")
        |}""".stripMargin
    )
    assertEquals(lines(found, "jdk-internal").size, 1, found.mkString("\n"))
    assert(lines(found, "jdk-internal").forall(_.contains("not exported by java.base")), found.mkString("\n"))
  }

  test("lookups of members of a class named directly are reported") {
    val found = warnings(
      """object Lints {
        |  def field(): java.lang.reflect.Field = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe")
        |}""".stripMargin
    )
    assertEquals(lines(found, "jdk-internal").size, 2, found.mkString("\n"))
  }

  test("a JDK internal use is suppressed by its check name") {
    val found = warnings(
      """object Lints {
        |  @SuppressWarnings(Array("bonsai:jdk-internal"))
        |  def load(): Class[?] = Class.forName("sun.misc.Unsafe")
        |  def list(): Class[?] = Class.forName("java.util.ArrayList")
        |}""".stripMargin
    )
    assertEquals(lines(found, "jdk-internal").size, 0, found.mkString("\n"))
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

  test("iterators compare by reference and are consumed") {
    assertWarns(
      "object Lints { def check(left: Iterator[Int], right: Iterator[Int]) = left == right }",
      "iterators compare by reference"
    )
    assertEquals(warnings("object Lints { def check(left: List[Int], right: Vector[Int]) = left == right }"), Nil)
    assertEquals(warnings("object Lints { def check(left: Iterator[Int]) = left == null }"), Nil)
    assertEquals(
      warnings("object Lints { def check(left: Array[Int], right: Array[Int]) = left == right }"),
      List("arrays compare by reference; compare their elements with sameElements [bonsai:array-equality]")
    )
  }

  test("sameElements and corresponds on unordered collections are unpredictable") {
    assertWarns(
      "object Lints { def check(left: Set[Int], right: Set[Int]) = left.corresponds(right)(_ == _) }",
      "unordered collection"
    )
    assertWarns(
      "object Lints { def check(left: Map[Int, Int], right: Map[Int, Int]) = left.corresponds(right)(_ == _) }",
      "unordered collection"
    )
    assert(
      warnings("object Lints { def check(left: Set[Int], right: Set[Int]) = left.sameElements(right) }")
        .exists(_.contains("unordered collection"))
    )
    assertEquals(
      warnings("object Lints { def check(left: List[Int], right: List[Int]) = left.corresponds(right)(_ == _) }"),
      Nil
    )
    assert(
      !warnings(
        """import scala.collection.immutable.SortedSet
          |object Lints { def check(left: SortedSet[Int], right: SortedSet[Int]) = left.corresponds(right)(_ == _) }""".stripMargin
      ).exists(_.contains("unordered collection"))
    )
    assert(
      !warnings(
        """import scala.collection.immutable.VectorMap
          |object Lints {
          |  def check(left: VectorMap[Int, Int], right: VectorMap[Int, Int]) = left.corresponds(right)(_ == _)
          |}""".stripMargin
      ).exists(_.contains("unordered collection"))
    )
  }

  test("conversions to the same type do nothing") {
    val found = warnings(
      "object Lints { def check(count: Int, name: String, values: List[Int]) = (count.toInt, name.toString, values.toList, count.toLong) }"
    )
    assert(found.exists(_.contains("toInt on a value that is already Int")), found.mkString("\n"))
    assert(found.exists(_.contains("toString on a value that is already String")), found.mkString("\n"))
    assert(found.exists(_.contains("remove the identity collection conversion")), found.mkString("\n"))
    assert(!found.exists(_.contains("toLong")), found.mkString("\n"))
  }

  test("a conversion of the result of a polymorphic function is no wrapped conversion") {
    val source = """object Lints {
                   |  def converted(lift: [A] => A => A, value: Int): Double = lift(value).toDouble
                   |}""".stripMargin
    assertEquals(tagged(warnings(source), "wrapped-conversion"), Nil)
  }

  test("a Java big number wrapped in its Scala counterpart only to be converted is converted directly") {
    val source = """object Lints {
                   |  def wrapped(value: java.math.BigDecimal): Double  = BigDecimal(value).toDouble
                   |  def integer(value: java.math.BigInteger): Long    = BigInt(value).toLong
                   |  def implicitly(value: java.math.BigDecimal): Int  = (value: BigDecimal).toInt
                   |  def sum(left: java.math.BigDecimal, right: java.math.BigDecimal): Float =
                   |    BigDecimal(left.add(right)).toFloat
                   |  def direct(value: java.math.BigDecimal): Double   = value.doubleValue
                   |  def scala(value: BigDecimal): Double              = value.toDouble
                   |  def parsed(text: String): Double                  = BigDecimal(text).toDouble
                   |  def arithmetic(value: java.math.BigDecimal): BigDecimal = BigDecimal(value) + 1
                   |}""".stripMargin
    assertEquals(
      tagged(warnings(source), "wrapped-conversion"),
      List(
        "left.add(right).floatValue converts the java.math.BigDecimal directly",
        "value.doubleValue converts the java.math.BigDecimal directly",
        "value.intValue converts the java.math.BigDecimal directly",
        "value.longValue converts the java.math.BigInteger directly"
      )
    )
    assertEquals(
      rewritten(source).linesIterator.filter(_.contains("Value")).map(_.trim).toList,
      List(
        "def wrapped(value: java.math.BigDecimal): Double  = value.doubleValue",
        "def integer(value: java.math.BigInteger): Long    = value.longValue",
        "def implicitly(value: java.math.BigDecimal): Int  = value.intValue",
        "def direct(value: java.math.BigDecimal): Double   = value.doubleValue"
      )
    )
  }

  test("a function applied once to its method's own arguments, and a trait only lambdas implement, are indirection") {
    val source =
      """object Lints {
                   |  private trait Extremum { def apply(left: Long, right: Long, width: Int): Long }
                   |  private trait Sink { def accept(value: Long): Unit }
                   |  private trait Pair { def first(value: Long): Long; def second(value: Long): Long }
                   |  private trait Shared { def get(value: Long): Long }
                   |  private class Fixed extends Shared { def get(value: Long): Long = value }
                   |  private trait Often { def get(value: Long): Long }
                   |  def median(a: Long, b: Long, width: Int): Long = {
                   |    def pick(x: Long, y: Long, extremum: Extremum): Long =
                   |      extremum(x, y, width) + (if (x != y) y else x)
                   |    pick(a, b, (left, right, bits) => math.min(left, right)) +
                   |      pick(b, a, (left, right, bits) => math.max(left, right))
                   |  }
                   |  def functions(a: Long, b: Long): Long = {
                   |    def pick(x: Long, y: Long, choose: (Long, Long) => Long): Long = choose(x, y) + x
                   |    pick(a, b, math.min) + pick(b, a, math.max)
                   |  }
                   |  private def lookup(key: Long, fallback: => Long): Long = if (key > 0) key else fallback
                   |  def guarded(a: Long, b: Long): Long = {
                   |    def pick(x: Long, y: Long, branch: (Long, Long) => Long): Long = if (x != y) branch(x, y) else x
                   |    pick(a, b, math.min) + pick(b, a, math.max)
                   |  }
                   |  def matched(a: Long, b: Long): Long = {
                   |    def pick(x: Long, y: Long, arm: (Long, Long) => Long): Long = x match {
                   |      case 0L => arm(x, y)
                   |      case _  => x
                   |    }
                   |    pick(a, b, math.min) + pick(b, a, math.max)
                   |  }
                   |  def short(a: Long, b: Long): Boolean = {
                   |    def pick(x: Long, y: Long, rhs: (Long, Long) => Long): Boolean = x != y && rhs(x, y) == y
                   |    pick(a, b, math.min) || pick(b, a, math.max)
                   |  }
                   |  def attempted(a: Long, b: Long): Long = {
                   |    def pick(x: Long, y: Long, risky: (Long, Long) => Long): Long =
                   |      try risky(x, y) catch { case _: ArithmeticException => x }
                   |    pick(a, b, math.min) + pick(b, a, math.max)
                   |  }
                   |  def lazily(a: Long, b: Long): Long = {
                   |    def pick(x: Long, y: Long, fail: (Long, Long) => Long): Long = lookup(x, fail(x, y))
                   |    pick(a, b, math.min) + pick(b, a, math.max)
                   |  }
                   |  private val bump: Long => Long = _ + 1
                   |  def compared(a: Long): Boolean = {
                   |    def pick(x: Long, other: Long => Long): Boolean = other.equals(bump)
                   |    pick(a, v => math.abs(v)) && pick(a, v => math.negateExact(v))
                   |  }
                   |  def twice(a: Long, b: Long): Long = {
                   |    def pick(x: Long, y: Long, choose: (Long, Long) => Long): Long = choose(x, y) + choose(y, x)
                   |    pick(a, b, math.min) + pick(b, a, math.max)
                   |  }
                   |  def looped(a: Long, b: Long): Long = {
                   |    def pick(x: Long, y: Long, choose: (Long, Long) => Long): Long = (0 until 3).map(_ => choose(x, y)).sum
                   |    pick(a, b, math.min) + pick(b, a, math.max)
                   |  }
                   |  def computed(a: Long, b: Long): Long = {
                   |    def pick(x: Long, y: Long, choose: (Long, Long) => Long): Long = {
                   |      val sum = x + y
                   |      choose(sum, y)
                   |    }
                   |    pick(a, b, math.min) + pick(b, a, math.max)
                   |  }
                   |  def shaped(a: Long, b: Long): Long = {
                   |    def pick(x: Long, y: Long, choose: (Long, Long) => Long): Long = choose(x, y)
                   |    pick(a, b, (left, right) => if (left > right) left else right) + pick(b, a, (left, right) => left + right + 1)
                   |  }
                   |  def passed(a: Long, b: Long, choose: (Long, Long) => Long): Long = {
                   |    def pick(x: Long, y: Long, with_ : (Long, Long) => Long): Long = with_(x, y)
                   |    pick(a, b, choose) + pick(b, a, math.max)
                   |  }
                   |  def sink(value: Long): Unit = {
                   |    def run(into: Sink): Unit = into.accept(value)
                   |    run(received => println(received))
                   |  }
                   |  def pair(value: Long): Long = {
                   |    def run(of: Pair): Long = of.first(value) + of.second(value)
                   |    run(new Pair { def first(v: Long): Long = v; def second(v: Long): Long = v })
                   |  }
                   |  def shared(value: Long): Long = {
                   |    def run(of: Shared): Long = of.get(value)
                   |    run(Fixed()) + run(received => received)
                   |  }
                   |  def often(value: Long): Long = {
                   |    def run(of: Often): Long = of.get(value)
                   |    run(a => a) + run(a => a + 1) + run(a => a + 2)
                   |  }
                   |  def exposed(a: Long, b: Long, choose: (Long, Long) => Long): Long = choose(a, b)
                   |  def adapted(value: Float, bits: Long): Double = {
                   |    def widened(unit: Long => Long, x: Float): Float =
                   |      java.lang.Float.intBitsToFloat(unit(java.lang.Float.floatToRawIntBits(x).toLong).toInt)
                   |    def wide(unit: Long => Long): Double = java.lang.Double.longBitsToDouble(unit(bits))
                   |    widened(java.lang.Long.reverse, value) + widened(java.lang.Long.rotateLeft(_, 1), value) +
                   |      wide(java.lang.Long.reverse) + wide(java.lang.Long.rotateLeft(_, 1))
                   |  }
                   |}""".stripMargin
    assertEquals(
      tagged(warnings(source), "function-applied-once"),
      List(
        "choose is applied once, to arguments the callers of pick have, and only ever given lambdas that forward to a method",
        "extremum is applied once, to arguments the callers of pick have, and only ever given lambdas that forward to a method",
        "into is applied once, to arguments the callers of run have, and only ever given lambdas that forward to a method"
      )
    )
    // Extremum's function type would box its Longs, so the trait is kept unless its class is cold
    assertEquals(
      tagged(warnings(source), "lambda-trait"),
      List("Often is implemented only by lambdas", "Sink is implemented only by lambdas")
    )
    assertEquals(
      tagged(warnings(source, "hot=!Lints"), "lambda-trait"),
      List(
        "Extremum is implemented only by lambdas",
        "Often is implemented only by lambdas",
        "Sink is implemented only by lambdas"
      )
    )
    assertEquals(tagged(warnings(source, "hot=Lints"), "lambda-trait"), Nil)
  }

  test("a private or local method that only passes its parameters on to another is an alias, a public facade is not") {
    val found = warnings(
      """object Lints {
        |  private def target(a: Int, b: Int): Int = a + b
        |  private def alias(a: Int, b: Int): Int = target(a, b)
        |  private def swapped(a: Int, b: Int): Int = target(b, a)
        |  private def partial(a: Int): Int = target(a, 1)
        |  private def worked(a: Int, b: Int): Int = target(a, b) + 1
        |  private def twice(a: Int, b: Int): Int = target(a, b)
        |  private def twice(a: Int): Int = a
        |  def api(a: Int, b: Int): Int = target(a, b)
        |  def open(a: Int, b: Int): Int = a * b
        |  inline def fast(a: Int, b: Int): Int = open(a, b)
        |  inline def shim(a: Int, b: Int): Int = target(a, b)
        |  private def wrap[T](value: T): Option[T] = Option(value)
        |  private def wrapped[T](value: T): Option[T] = wrap[T](value)
        |  private def widened(a: Int, b: Int): Long = target(a, b)
        |  private def lazily(a: => Int): Int = target(a, 1)
        |  private def curried(a: Int)(b: Int): Int = target(a, b)
        |  def local(): Int = {
        |    def inner(a: Int): Int = a + 1
        |    def forward(a: Int): Int = inner(a)
        |    forward(1) + inner(2)
        |  }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "forwarding-alias"),
      List(
        "alias only passes its parameters to target",
        "forward only passes its parameters to inner",
        "wrapped only passes its parameters to wrap"
      ).sorted
    )
  }

  test("an object whose members all pass their parameters on to other objects is a facade to call directly") {
    val found = warnings(
      """object Lints {
        |  object Engine {
        |    def start(speed: Int): Int = speed * 2
        |    def stop(): Unit = ()
        |    def measure(bytes: Vector[Byte]): Int = bytes.size
        |  }
        |  object Facade {
        |    def start(speed: Int): Int = Engine.start(speed)
        |    def stop(): Unit = Engine.stop()
        |  }
        |  object Platform {
        |    def epochMillis: Long = Engine.start(0).toLong
        |    def monotonic(): Unit = Engine.stop()
        |  }
        |  object Keywords {
        |    private val values = Set("class", "def")
        |    def contains(name: String): Boolean = values(name)
        |  }
        |  object Conversions {
        |    implicit def widen(speed: Int): Long = Engine.start(speed).toLong
        |  }
        |  object Wrapper {
        |    def start(speed: Int): Int = Engine.start(speed + 1)
        |    def stop(): Unit = Engine.stop()
        |  }
        |  trait Decoder { def decode(bytes: Vector[Byte]): Int }
        |  object Disassembler extends Decoder {
        |    override def decode(bytes: Vector[Byte]): Int = Engine.measure(bytes)
        |  }
        |  object Bridged {
        |    import Engine.*
        |    def start(speed: Int): Int = Engine.start(speed)
        |    def stop(): Unit = Engine.stop()
        |  }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "forwarding-alias"),
      List(
        "Bridged only passes its parameters to other objects",
        "Facade only passes its parameters to other objects"
      )
    )
  }

  test("a parameter marked unused that nothing needs is leftover, unless the method must keep its shape") {
    assume(Compilations.minor >= 7, "the compiler keeps @unused on parameters from Scala 3.7")
    val found = warnings(
      """import scala.annotation.unused
        |object Lints {
        |  private def run(a: Int, @unused b: Int): Int = a
        |  def user: Int = run(1, 2)
        |  private def kept(a: Int, @unused b: Int): Int = a
        |  val table: Seq[(Int, Int) => Int] = Seq(kept)
        |  private def pick(a: Int, @unused b: Int): Int = a
        |  val eta: (Int, Int) => Int = pick
        |  private def evidence(a: Int)(using @unused numeric: Numeric[Int]): Int = a
        |  def evident: Int = evidence(1)(using summon[Numeric[Int]])
        |  def open(a: Int, @unused b: Int): Int = a
        |  private def used(a: Int, b: Int): Int = a + b
        |  def outer(): Int = {
        |    def local(@unused ignored: String, a: Int): Int = a
        |    local("x", 1)
        |  }
        |  def mapped(values: List[Int]): List[Int] = {
        |    def keep(@unused tag: Int, value: Int): Int = value
        |    values.map(keep(0, _))
        |  }
        |}
        |class Base { protected def hook(a: Int, @unused b: Int): Int = a }
        |class Sub extends Base { override protected def hook(a: Int, @unused b: Int): Int = a + 1 }""".stripMargin
    )
    assertEquals(
      tagged(found, "unused-private-param"),
      List(
        "b of run is marked @unused and nothing uses it",
        "ignored of local is marked @unused and nothing uses it",
        "tag of keep is marked @unused and nothing uses it"
      ).sorted
    )
  }

  test("a private case class built once, passed to a method that unpacks it at once, only bundles parameters") {
    val found = warnings(
      """object Lints {
        |  private case class Settings(width: Int, height: Int, depth: Int, colour: Int)
        |  def draw(width: Int): Int = render(Settings(width, 2, 3, 4))
        |  private def render(settings: Settings): Int = {
        |    val Settings(width, height, depth, colour) = settings
        |    width + height + depth + colour
        |  }
        |  private case class Bound(a: Int, b: Int, c: Int, d: Int)
        |  def bound(): Int = take(Bound(1, 2, 3, 4))
        |  private def take(bound: Bound): Int = {
        |    val a = bound.a
        |    val b = bound.b
        |    val c = bound.c
        |    val d = bound.d
        |    a + b + c + d
        |  }
        |  private case class Reused(a: Int, b: Int, c: Int, d: Int)
        |  def reused(): Int = both(Reused(1, 2, 3, 4))
        |  private def both(reused: Reused): Int = {
        |    val Reused(a, b, c, d) = reused
        |    a + b + c + d + reused.a
        |  }
        |  private case class Twice(a: Int, b: Int, c: Int, d: Int)
        |  def twice(): Int = open(Twice(1, 2, 3, 4)) + open(Twice(5, 6, 7, 8))
        |  private def open(twice: Twice): Int = {
        |    val Twice(a, b, c, d) = twice
        |    a + b + c + d
        |  }
        |  private case class Small(a: Int, b: Int)
        |  def small(): Int = pair(Small(1, 2))
        |  private def pair(small: Small): Int = {
        |    val Small(a, b) = small
        |    a + b
        |  }
        |  private case class Plain(a: Int, b: Int, c: Int, d: Int)
        |  def plain(): Int = read(Plain(1, 2, 3, 4))
        |  private def read(plain: Plain): Int = plain.a + plain.b + plain.c + plain.d
        |  private case class Stored(a: Int, b: Int, c: Int, d: Int)
        |  private val stored = Stored(1, 2, 3, 4)
        |  def keep(): Int = unpack(stored)
        |  private def unpack(stored: Stored): Int = {
        |    val Stored(a, b, c, d) = stored
        |    a + b + c + d
        |  }
        |  def second(): Int = unpack(Stored(5, 6, 7, 8))
        |}
        |case class Exposed(a: Int, b: Int, c: Int, d: Int)
        |object Public {
        |  def exposed(): Int = unpack(Exposed(1, 2, 3, 4))
        |  private def unpack(exposed: Exposed): Int = {
        |    val Exposed(a, b, c, d) = exposed
        |    a + b + c + d
        |  }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "parameter-object-unpacked"),
      List(
        "Bound is built at one call and unpacked by take at once",
        "Settings is built at one call and unpacked by render at once"
      )
    )
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

  test("parameters of inline methods: functions only called, arguments run per use, and abstract inline receivers") {
    val found = warnings(
      """import scala.language.implicitConversions
        |trait Algebra { inline def add(left: Long, right: Long): Long }
        |trait Kernel { def apply(value: Int): Int }
        |class Wrapped(val value: Int)
        |object Lints {
        |  implicit def wrap(value: Int): Wrapped = Wrapped(value)
        |  inline def mapped(values: Array[Int], f: Int => Int): Unit = {
        |    var index = 0
        |    while (index < values.length) { values(index) = f(values(index)); index += 1 }
        |  }
        |  inline def kerneled(value: Int, kernel: Kernel): Int     = kernel(value)
        |  inline def stored(f: Int => Int): Int => Int             = f
        |  inline def passed(f: Int => Int): Int                    = List(1).map(f).sum
        |  inline def expanded(inline f: Int => Int): Int           = f(1)
        |  inline def twice(inline value: Int): Int                 = value + value
        |  inline def guarded(flag: Boolean, inline value: Int): Int = if (flag) value else 0
        |  inline def fallback(flag: Boolean)(inline value: => Int): Int = if (flag) value else 0
        |  inline def once(inline value: Int): Int                  = value * 2
        |  inline def sum(algebra: Algebra, value: Long): Long      = algebra.add(value, value)
        |  inline def inlined(inline algebra: Algebra, value: Long): Long = algebra.add(value, value)
        |  inline def text(inline value: String): String = { net.kurobako.bonsai.plugin.Macros.checked(value); value }
        |  inline def described(inline value: Int): String = scala.compiletime.codeOf(value) + value.toString
        |  inline def forwarded(inline value: Int): Int = twice(value)
        |  inline def chosen(inline flag: Boolean, inline value: Int): Int = inline if (flag) value + 1 else value - 1
        |  inline def dispatched(kind: Int, inline value: Int): Int =
        |    kind match { case 0 => value; case 1 => value + 1; case _ => value * 3 }
        |  inline def repeated(kind: Int, inline value: Int): Int = kind match { case 0 => value + value; case _ => value }
        |  inline def doubled(inline value: Wrapped): Int = value.value + value.value
        |  inline def dot(inline accumulator: Int, inline width: Int, inline lanes: Int): Int =
        |    width + width + width + lanes * lanes + accumulator
        |  inline def named(inline width: Int): Int = dot(1, lanes = 32 / width, width = width)
        |  def calls(values: List[Int], count: Int): Int =
        |    twice(values.size) + twice(count) + twice(count + 1) + guarded(count > 0, { println(count); count }) +
        |      guarded(count > 1, values.head) + fallback(count > 2) { println(count); count } + once(values.size) +
        |      dispatched(count, values.size) + repeated(count, values.size) + doubled(5)
        |  def checks(values: List[Int]): Int =
        |    text(s"size ${values.size}").length + described(values.size).length + forwarded(values.size) +
        |      chosen(true, values.size)
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "inline-function-parameter"),
      List(
        "f is only called, so each call site allocates a closure for it and calls it through apply",
        "kernel is only called, so each call site allocates a closure for it and calls it through apply"
      ).sorted
    )
    assertEquals(
      tagged(found, "inline-parameter-reuse"),
      List(
        "the expansion of forwarded evaluates this argument to value 2 times on some path",
        "the expansion of guarded evaluates this argument to value only where its use is reached, so its effects run " +
          "only there",
        "the expansion of repeated evaluates this argument to value 2 times on some path",
        "the expansion of twice evaluates this argument to value 2 times on some path"
      )
    )
    assertEquals(
      tagged(found, "inline-instance-parameter"),
      List("add is an abstract inline method, which expands only on an inline algebra")
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
        |  def blend(first: Int, second: Int, third: Int, fourth: Int, fifth: Int): Int = first
        |  def pair(operand1: Int, operand2: Int): Int = operand1
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
        |  val ordinal     = Geometry.blend(1, 2, 3, 4, 5)
        |  val indexed     = Geometry.pair(1, 2)
        |  val local       = {
        |    def area(width: Int, height: Int) = width * height
        |    area(3, 4)
        |  }
        |}""".stripMargin
    )
    assertEquals(
      found.filter(_.contains("share a type")),
      List(
        "width and height share a type; name the arguments, or take them together as a named tuple or a case class, " +
          "so they cannot be swapped [bonsai:same-type-arguments]"
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
      """object Lints {
        |  private trait Shape
        |  private sealed trait Colour
        |  private abstract class Animal
        |  private case class Square(side: Int) extends Shape
        |  private trait State
        |  private trait LocalView extends State
        |  private trait ForeignView extends munit.Assertions
        |  private def local(state: State): Int = state match { case _: LocalView => 1; case _ => 0 }
        |  private def foreign(state: munit.Assertions): Int = state match { case _: ForeignView => 1; case _ => 0 }
        |  private def checked(state: munit.Assertions): Boolean = state.isInstanceOf[ForeignView]
        |  private def area(shape: Shape): Int = shape match { case square: Square => square.side; case _ => 0 }
        |  private def open(value: Any): Boolean = value.isInstanceOf[Shape] || value.isInstanceOf[Square]
        |  private def animal(value: Any): Int =
        |    value match { case _: Animal => 1; case _: Colour => 2; case _: Product => 3; case _ => 0 }
        |}""".stripMargin
    )
    assertEquals(found.count(_.contains("type test on Shape")), 1, found.mkString("\n"))
    assertEquals(found.count(_.contains("type test on Animal")), 1, found.mkString("\n"))
    assertEquals(found.count(_.contains("type test on LocalView")), 1, found.mkString("\n"))
    assertEquals(found.count(_.contains("type test on ForeignView")), 0, found.mkString("\n"))
    assert(
      !found.exists(found => found.contains("Square") || found.contains("Colour") || found.contains("Product")),
      found.mkString("\n")
    )
  }

  test("type tests on types that code outside the compilation can extend are not reported") {
    val found = warnings(
      """trait Shape
        |abstract class Animal
        |private trait Packaged
        |object Lints {
        |  trait Nested
        |  protected[Lints] trait Qualified
        |  private trait Hidden
        |  private[Lints] abstract class Secret
        |  def open(value: Any): Int =
        |    value match { case _: Shape => 1; case _: Animal => 2; case _: Packaged => 3; case _: Nested => 4; case _ => 0 }
        |  def hidden(value: Any): Int = value match { case _: Hidden => 1; case _: Secret => 2; case _: Qualified => 3; case _ => 0 }
        |  def local(value: Any): Int = { trait Inner; value match { case _: Inner => 1; case _ => 0 } }
        |}
        |private object Closed {
        |  trait Inside
        |  def inside(value: Any): Int = value match { case _: Inside => 1; case _ => 0 }
        |}""".stripMargin
    )
    val reported = found.filter(_.contains("type test on")).map(_.takeWhile(_ != ',')).sorted
    assertEquals(
      reported,
      List("type test on Hidden", "type test on Inner", "type test on Qualified", "type test on Secret")
    )
  }

  test("type tests on a parameter that an override of a library method fixes are not reported") {
    val found = diagnostics(
      """import scala.concurrent.ExecutionContext
        |object Lints {
        |  private trait Rejectable extends Runnable { def reject(e: Throwable): Unit }
        |  private trait Job
        |  private trait Retrying extends Job
        |  private final class Pool extends ExecutionContext {
        |    override def execute(r: Runnable): Unit = r match { case t: Rejectable => t.reject(null); case _ => () }
        |    override def reportFailure(t: Throwable): Unit = ()
        |  }
        |  private def run(job: Job): Int = job match { case r: Retrying => 1; case _ => 0 }
        |}""".stripMargin
    )
    assertEquals(found.count(_.contains("type test on Rejectable")), 0, found.mkString("\n"))
    assertEquals(found.count(_.contains("type test on Retrying")), 1, found.mkString("\n"))
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
        "write Three(_, _, _) instead of the type test _: Three",
        "write (_, _) instead of the type test _: (Int, String)",
        "write Aliases.Named(_, _) instead of the type test _: Aliases.Named",
        "write Point(_, _) instead of the type test _: Point"
      ).sorted
    )
  }

  test("type tests on case classes name their extractor at any arity, others only within the wildcard limit") {
    val source =
      """case class Three(a: Int, b: String, c: Double)
                   |class Triple(val a: Int, val b: Int, val c: Int)
                   |object Triple { def unapply(triple: Triple): Some[(Int, Int, Int)] = Some((triple.a, triple.b, triple.c)) }
                   |object Lints {
                   |  def three(value: Any): Int = value match { case _: Three => 1; case _: Triple => 2; case _ => 0 }
                   |}""".stripMargin
    def found(options: String*) =
      warnings(source, options*).filter(_.contains("instead of the type test")).map(_.takeWhile(_ != ';')).sorted
    assertEquals(found(), List("write Three(_, _, _) instead of the type test _: Three"))
    assertEquals(
      found("wildcards=3"),
      List(
        "write Three(_, _, _) instead of the type test _: Three",
        "write Triple(_, _, _) instead of the type test _: Triple"
      )
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
    assertEquals(errors, List("bonsai option wildcards must be a non-negative count, not many"))
  }

  test("a bound must be a member and a non-negative count") {
    val (errors, _) = compile("Lints.scala", "object Lints", "bounded=Format.widths:many", "bounded=Format.widths")
    assertEquals(
      errors,
      List(
        "bonsai option bounded must be <member>:<max>, not Format.widths",
        "bonsai option bounded must be <member>:<max>, not Format.widths:many"
      )
    )
  }

  test("the option fold must be one of its spellings") {
    val (errors, _) = compile("Lints.scala", "object Lints", "option-fold=getOrElse")
    assertEquals(errors, List("bonsai option option-fold must be one of fold, map-get-or-else, not getOrElse"))
    assertEquals(compile("Lints.scala", "object Lints", "option-fold=fold"), (Nil, Nil))
    assertEquals(compile("Lints.scala", "object Lints", "option-fold=map-get-or-else"), (Nil, Nil))
  }

  test("the ci mode makes the strict checks errors, unless a check is kept a warning") {
    val source = """object Lints {
                   |  private trait Shape
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
      List("bonsai option mode must be one of dev, ci, not release")
    )
  }

  test("the strict checks are those whose findings have been real wherever they fired, and judgement calls warn") {
    assertEquals(
      Check.all.filter(_.strict).map(_.id).toSet,
      Set(
        "acyclic",
        "array-equality",
        "iterator-equality",
        "unordered-comparison",
        "swapped-arguments",
        "nested-future",
        "long-arguments",
        "same-type-arguments",
        "boolean-argument",
        "extractor-type-test",
        "named-pattern",
        "redundant-conversion",
        "wrapped-conversion",
        "unit-map",
        "linear-in-loop",
        "local-throw",
        "throw-in-default",
        "throwing-helper",
        "partial-in-try",
        "redundant-try",
        "broad-catch",
        "discarded-cause",
        "shadowed-extension"
      ) ++ Laws.ids
    )
    assertEquals(
      Check.all.filter(_.severity == Check.Severity.Error).map(_.id).toSet,
      Set("accidental-self-call", "unrelated-type-test")
    )
  }

  test("sources are linted wherever they are, generated or not, but mutation-testing copies are not") {
    val source = """import scala.concurrent.Future
                   |object Lints {
                   |  def check(left: Array[Int], right: Array[Int]) = left == right
                   |  def parse(s: String) = "e+".r.matches(s)
                   |  def nested: Future[Future[Int]] = ???
                   |}""".stripMargin
    assertEquals(warningsAt("src/main/scala/Lints.scala", source, "hot=Lints").size, 3)
    assertEquals(warningsAt("src/generated/scala/Lints.scala", source, "hot=Lints").size, 3)
    assertEquals(warningsAt("target/stryker4s-1234/src/main/scala/Lints.scala", source, "hot=Lints"), Nil)
  }

  test("a file a tool wrote is not linted, whether a ScalaPB header or an @generated tag marks it") {
    val code = """object Lints {
                 |  def name(value: Option[String]): String = value.map(_.trim).getOrElse("")
                 |}""".stripMargin
    assertEquals(tagged(warnings(code), "map-get-or-else-to-fold").size, 1)
    assertEquals(warnings(s"// Generated by the Scala Plugin for the Protocol Buffer Compiler.\n$code"), Nil)
    assertEquals(warnings(s"// @generated\n$code"), Nil)
    assertEquals(warnings(s"/* @generated by the tool */\n$code"), Nil)
    assertEquals(tagged(warnings(s"package demo\n// @generated\n$code"), "map-get-or-else-to-fold").size, 1)
  }

  test("a definition annotated as generated by a tool is not linted, and the others are") {
    val code = """import javax.annotation.processing.Generated
                 |@Generated(value = Array("tool"), date = "", comments = "")
                 |object Lints {
                 |  def name(value: Option[String]): String = value.map(_.trim).getOrElse("")
                 |}
                 |object Hand {
                 |  def name(value: Option[String]): String = value.map(_.trim).getOrElse("")
                 |}""".stripMargin
    assertEquals(tagged(warnings(code), "map-get-or-else-to-fold").size, 1)
  }

  test("code under a generated glob is not linted, and generated-disable names the checks that stay off") {
    val code = """object Lints {
                 |  def name(value: Option[String]): String = value.map(_.trim).getOrElse("")
                 |  def check(left: Array[Int], right: Array[Int]) = left == right
                 |}""".stripMargin
    assertEquals(warningsAt("gen/Lints.scala", code, "generated=**/gen/**"), Nil)
    assertEquals(warningsAt("src/Lints.scala", code, "generated=**/gen/**").size, 2)
    assertEquals(warningsAt("gen/Lints.scala", code, "generated=**/gen/**", "generated-disable=").size, 2)
    val kept = warningsAt("gen/Lints.scala", code, "generated=**/gen/**", "generated-disable=array-equality")
    assertEquals(tagged(kept, "array-equality"), Nil)
    assertEquals(tagged(kept, "map-get-or-else-to-fold").size, 1)
  }

  test("checks carry their id, and can be disabled, escalated or suppressed by it") {
    val source   = "object Lints { def check(left: Array[Int], right: Array[Int]) = left == right }"
    val expected = "arrays compare by reference; compare their elements with sameElements [bonsai:array-equality]"
    assertEquals(warnings(source), List(expected))
    assertEquals(warnings(source, "disable=array-equality"), Nil)
    assertEquals(compile("Lints.scala", source, "error=array-equality"), (List(expected), Nil))
    assertEquals(compile("Lints.scala", source, "error=*"), (List(expected), Nil))
    assertEquals(
      warnings(
        """import scala.annotation.nowarn
          |object Lints {
          |  @nowarn("msg=bonsai:array-equality")
          |  def check(left: Array[Int], right: Array[Int]) = left == right
          |}""".stripMargin,
        "disable=nowarn-suppression"
      ),
      Nil
    )
  }

  private def tagged(found: List[String], check: String): List[String] =
    found.view.filter(_.endsWith(s"[bonsai:$check]")).map(_.takeWhile(_ != ';')).toList.sorted

  test("an inline method from another compilation that calls a polymorphic function value has no callee to read") {
    val library =
      """package lib
        |type Callback[A]   = Either[Throwable, A] => Unit
        |type Suspend[F[_]] = [A] => (Callback[A] => Unit) => F[A]
        |trait Ops[F[_]](suspend: Suspend[F]) {
        |  inline def task[A](inline f: => A): F[A] = suspend { cb => cb(Right(f)) }
        |}""".stripMargin
    val use =
      """object Lints {
        |  def side(): Int = { println("x"); 1 }
        |  def run(ops: lib.Ops[Option]): Option[Int] = ops.task[Int](side())
        |}""".stripMargin
    Compilations.run(Seq("Ops.scala" -> library), Nil) { (compiled, directory) =>
      assert(!compiled.hasErrors)
      val classpath = s"${sys.props("bonsai.plugin.classpath")}:${directory.resolve("classes")}"
      Compilations.run(Seq("Lints.scala" -> use), Seq(s"-classpath:$classpath")) { (reporter, _) =>
        assert(!reporter.hasErrors)
      }
    }
  }

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
        |    values.count(value => (older ++ newer).exists(_ > value))
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

  test("existence tests for equality inside loops are reported as contains is") {
    val found = warnings(
      """object Lints {
        |  def exists(values: Vector[Int], keys: List[Int]): Int = values.count(value => keys.exists(_ == value))
        |  def named(values: Vector[Int], keys: Seq[Int]): Int = values.count(value => keys.exists(key => key == value))
        |  def forall(values: Vector[Int], keys: List[Int]): Int = values.count(value => keys.forall(_ != value))
        |  def array(values: Vector[Int], keys: Array[Int]): Int = values.count(value => keys.exists(_ == value))
        |  def ordered(values: Vector[Int], keys: List[Int]): Int = values.count(value => keys.exists(_ > value))
        |  def negated(values: Vector[Int], keys: List[Int]): Int = values.count(value => keys.exists(_ != value))
        |  def dependent(values: Vector[Int], keys: List[Int]): Int = values.count(value => keys.exists(key => key == key + value))
        |  def keyed(values: Vector[Int], keys: Set[Int]): Int = values.count(value => keys.exists(_ == value))
        |  def ranged(values: Vector[Int]): Int = values.count(value => (0 until 8).exists(_ == value))
        |  def iterated(rows: Vector[List[Int]]): Int = rows.count(row => row.exists(_ == 1))
        |  def outside(keys: List[Int], value: Int): Boolean = keys.exists(_ == value)
        |  def optional(values: Vector[Int], key: Option[Int]): Int = values.count(value => key.exists(_ == value))
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "linear-in-loop"),
      List(
        "exists on a List scans it on every iteration",
        "exists on a Seq scans it on every iteration",
        "forall on a List scans it on every iteration",
        "exists on an ArrayOps scans it on every iteration"
      ).sorted
    )
  }

  test("the advice for boxing folds names a tail recursive method, which needs no var") {
    val source =
      """package demo.kernel
        |object Lanes {
        |  def sum(values: Array[Long]): Long = values.foldLeft(0L)(_ + _)
        |  def indexed(values: Array[Long]): Array[(Long, Int)] = values.zipWithIndex
        |}""".stripMargin
    assertEquals(
      warnings(source, "hot=demo.kernel.**").filter(_.contains("boxing-fold")).map(_.takeWhile(_ != '[').trim).sorted,
      List(
        "foldLeft with a Long accumulator boxes it on every step; use a @tailrec method over primitives",
        "zipWithIndex builds a boxed (Long, Int) for every element; use a @tailrec method over primitive arrays"
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

  test("boxing in loops is reported in hot classes only when the build marks a hot list") {
    val oracle =
      """class OracleSuite {
        |  def check(): Unit = for (r <- 0 until 4) assert((0 until 16).foldLeft(0f)(_ + _ * r) == 0f)
        |}""".stripMargin
    val loop =
      """import scala.collection.mutable
        |class VMLoop {
        |  val m = mutable.Map.empty[String, Long]
        |  def run(xs: Array[Long]): Unit = xs.foreach(x => m("k") = x)
        |}""".stripMargin
    assertEquals(tagged(warnings(oracle), "loop-boxing").size, 1)
    assertEquals(tagged(warnings(oracle, "hot=**.VM*", "hot=!**.*Suite"), "loop-boxing"), Nil)
    assertEquals(tagged(warnings(loop, "hot=**.VM*", "hot=!**.*Suite"), "loop-boxing").size, 1)
    assertEquals(tagged(warnings(loop), "loop-boxing").size, 1)
    assertEquals(tagged(warnings(loop, "hot=**.Other*"), "loop-boxing"), Nil)
  }

  test("regexes compiled from literals on every call, or on every iteration, are reported in hot classes") {
    val source =
      """class Lints {
        |  val field              = "a".r
        |  lazy val deferred      = java.util.regex.Pattern.compile("b")
        |  val table              = List("x").map(_ => "c".r)
        |  "d".r.matches("d")
        |  def parse(s: String)   = "e+".r.matches(s)
        |  def lambda(xs: List[String]) = xs.map(x => "f+".r.matches(x))
        |  def compile(s: String) = java.util.regex.Pattern.compile("g+", 0).matcher(s)
        |  def grouped            = "(h)".r("group")
        |  def local(s: String)   = { val pattern = "i+".r; pattern.matches(s) }
        |  def raw(s: String)     = raw"\d+".r.matches(s)
        |  def escapes(s: String) = s"\\s+".r.matches(s)
        |  def built(s: String, n: Int) = s"x{$n}".r.matches(s)
        |  def dynamic(s: String) = s.r
        |  def reused(s: String)  = field.matches(s)
        |  def split(lines: Vector[String])     = lines.map(_.split(",\\s*"))
        |  def comma(lines: Vector[String])     = lines.map(_.split(","))
        |  def escaped(lines: Vector[String])   = lines.map(_.split("\\|"))
        |  def spaced(lines: Vector[String])    = lines.map(_.split(raw"\s+"))
        |  def piped(lines: Vector[String])     = lines.map(_.split(raw"\|"))
        |  def dot(lines: Vector[String])       = lines.map(_.split("."))
        |  def replaced(lines: Vector[String])  = lines.map(_.replaceAll("j+", "k"))
        |  def first(lines: Vector[String])     = lines.map(_.replaceFirst("l+", "m"))
        |  def matched(lines: Vector[String])   = lines.count(_.matches("[0-9]+"))
        |  def plain(lines: Vector[String])     = lines.map(_.replace("n", "o"))
        |  def once(line: String)               = line.replaceAll("p+", "q")
        |  def variable(lines: Vector[String], separator: String) = lines.map(_.split(separator))
        |}""".stripMargin
    assertEquals(tagged(warnings(source), "regex-in-def"), Nil)
    assertEquals(tagged(warnings(source, "hot=Other"), "regex-in-def"), Nil)
    assertEquals(
      tagged(warnings(source, "hot=Lints"), "regex-in-def"),
      List(
        "\"e+\".r compiles the pattern on every call",
        "\"f+\".r compiles the pattern on every call",
        "java.util.regex.Pattern.compile(\"g+\", 0) compiles the pattern on every call",
        "\"(h)\".r compiles the pattern on every call",
        "\"i+\".r compiles the pattern on every call",
        "raw\"\\d+\".r compiles the pattern on every call",
        "s\"\\\\s+\".r compiles the pattern on every call",
        "split compiles its literal regex on every iteration",
        "split compiles its literal regex on every iteration",
        "split compiles its literal regex on every iteration",
        "replaceAll compiles its literal regex on every iteration",
        "replaceFirst compiles its literal regex on every iteration",
        "matches compiles its literal regex on every iteration"
      ).sorted
    )
  }

  test("function types Scala does not specialise for their primitive type arguments are reported on parameters") {
    val found = warnings(
      """class Visitor(visit: (Long, Long, Int) => Unit, val each: Long => String)
        |case class Holder(f: (Int, Int, Int) => Int)
        |trait Walk { def walk(visit: (Long, Int, Boolean) => Unit): Unit }
        |object Lints extends Walk {
        |  def walk(visit: (Long, Int, Boolean) => Unit): Unit       = visit(1L, 2, true)
        |  val ternary: (Long, Long, Long) => Long                   = (a, b, c) => a
        |  val inferred                                              = (a: Double, b: Double, c: Double) => a
        |  var updated: (Long, Int, Boolean) => Unit                 = (_, _, _) => ()
        |  def unary(f: Long => Long): Long                          = f(1L)
        |  def binary(f: (Int, Double) => Boolean): Boolean          = f(1, 2.0)
        |  def floats(f: (Float, Float) => Float): Float             = f(1, 2)
        |  def short(f: Short => Int): Int                           = f(1)
        |  def char(f: Int => Char): Char                            = f(1)
        |  def text(f: String => Long): Long                         = f("")
        |  def references(f: (String, String, String) => String)    = f("", "", "")
        |  def thunk(f: () => Long): Long                            = f()
        |  def effect(f: String => Unit): Unit                       = f("")
        |  def predicate(f: String => Boolean): Boolean              = f("")
        |  def flags(f: (Boolean, Boolean) => Boolean): Boolean      = f(true, false)
        |  def tested(f: (String, Long) => Boolean): Boolean         = f("", 1L)
        |  inline def inlined(inline f: (Long, Long, Long) => Unit)  = f(1L, 2L, 3L)
        |  def take(f: (Long, Int, Boolean) => Unit): Unit           = f(1L, 2, true)
        |  def lambda(): Unit                                        = take((a, b, c) => ())
        |  def local(): Int = { val g: (Byte, Byte) => Int = (a, b) => a + b; g(1, 2) }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "unspecialised-function"),
      List(
        "Function3[Long, Long, Int, Unit] is not specialised and boxes its primitive arguments on every call",
        "Function1[Long, String] is not specialised and boxes its primitive arguments on every call",
        "Function3[Int, Int, Int, Int] is not specialised and boxes its primitive arguments on every call",
        "Function3[Long, Int, Boolean, Unit] is not specialised and boxes its primitive arguments on every call",
        "Function2[Float, Float, Float] is not specialised and boxes its primitive arguments on every call",
        "Function1[Short, Int] is not specialised and boxes its primitive arguments on every call",
        "Function1[Int, Char] is not specialised and boxes its primitive arguments on every call",
        "Function1[String, Long] is not specialised and boxes its primitive result on every call",
        "Function3[Long, Int, Boolean, Unit] is not specialised and boxes its primitive arguments on every call",
        "Function2[String, Long, Boolean] is not specialised and boxes its primitive arguments on every call"
      ).sorted
    )
    val source = """class Suite(visit: (Long, Long, Int) => Unit)
                   |object Lints {
                   |  def take(f: (Long, Long, Int) => Unit): Unit = f(1L, 2L, 3)
                   |}""".stripMargin
    assertEquals(tagged(warnings(source), "unspecialised-function").size, 2)
    assertEquals(tagged(warnings(source, "hot=!Suite"), "unspecialised-function").size, 1)
    assertEquals(tagged(warnings(source, "hot=!Lints", "hot=!Suite"), "unspecialised-function"), Nil)
  }

  test("function-typed parameters of enum cases are not reported") {
    val found = warnings(
      """object Lints {
        |  enum Op {
        |    case Fold(f: (Long, Long, Long) => Long)
        |    case Pick(f: Short => Int)
        |  }
        |}""".stripMargin
    )
    assertEquals(tagged(found, "unspecialised-function"), Nil)
  }

  test("a function value a loop invokes is reported, as a parameter or a member, but not one called once") {
    val found = warnings(
      """object Lints {
        |  val scale: Short => Long = s => s.toLong
        |  def run(xs: Array[Short]): Long = {
        |    val step: Short => Int = s => s + 1
        |    var i     = 0
        |    var total = 0L
        |    while (i < xs.length) {
        |      total += step(xs(i)) + scale(xs(i))
        |      i += 1
        |    }
        |    total
        |  }
        |  def unrolled(xs: Array[Short]): Int = {
        |    val step: Short => Int = s => s + 1
        |    step(xs(0)) + step(xs(1))
        |  }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "unspecialised-function"),
      List(
        "Function1[Short, Int] is not specialised and boxes its primitive arguments on every call",
        "Function1[Short, Long] is not specialised and boxes its primitive arguments on every call"
      ).sorted
    )
  }

  test("a function parameter outside a hot class is reported only when a loop invokes it") {
    val found = warnings(
      """class Runner {
        |  def run(f: Short => Int, xs: Array[Short]): Int = {
        |    var i     = 0
        |    var total = 0
        |    while (i < xs.length) {
        |      total += f(xs(i))
        |      i += 1
        |    }
        |    total
        |  }
        |  def apply(f: Char => Int): Int = f('a')
        |}
        |object Lints {
        |  def go(f: Byte => Int): Int = f(1)
        |}""".stripMargin,
      "hot=Lints"
    )
    assertEquals(
      tagged(found, "unspecialised-function"),
      List(
        "Function1[Byte, Int] is not specialised and boxes its primitive arguments on every call",
        "Function1[Short, Int] is not specialised and boxes its primitive arguments on every call"
      ).sorted
    )
  }

  test("appending to an accumulator that copies on every append is reported") {
    val found = warnings(
      """import scala.collection.immutable.ArraySeq
        |import scala.collection.immutable.Queue
        |object Lints {
        |  def hex(tokens: Vector[String]): Either[String, ArraySeq[Byte]] =
        |    tokens.foldLeft[Either[String, ArraySeq[Byte]]](Right(ArraySeq.empty)) {
        |      case (Right(bytes), token) => Right(bytes ++ ArraySeq(token.toByte))
        |      case (left, _)             => left
        |    }
        |  def mapped(tokens: Vector[String]): Either[String, ArraySeq[Byte]] =
        |    tokens.foldLeft[Either[String, ArraySeq[Byte]]](Right(ArraySeq.empty))((result, token) =>
        |      result.map(bytes => bytes :+ token.toByte)
        |    )
        |  def text(words: Vector[String]): String            = words.foldLeft("")((line, word) => line + word)
        |  def arrays(rows: Vector[Array[Int]]): Array[Int]    = rows.foldLeft(Array.empty[Int])(_ ++ _)
        |  def joined(words: Vector[String]): String = {
        |    var line  = ""
        |    var index = 0
        |    while (index < words.length) { line += words(index); index += 1 }
        |    line
        |  }
        |  def appended(values: Vector[Int]): ArraySeq[Int] = {
        |    var all = ArraySeq.empty[Int]
        |    values.foreach(value => all :+= value)
        |    all
        |  }
        |  def concatenated(values: Vector[ArraySeq[Int]]): ArraySeq[Int] = {
        |    var all = ArraySeq.empty[Int]
        |    for (value <- values) all = all ++ value
        |    all
        |  }
        |  def vectors(values: Vector[Int]): Vector[Int]      = values.foldLeft(Vector.empty[Int])(_ :+ _)
        |  def queued(values: Vector[Int]): Queue[Int]        = values.foldLeft(Queue.empty[Int])(_ :+ _)
        |  def sum(values: Vector[Int]): Int                  = values.foldLeft(0)(_ + _)
        |  def fresh(words: Vector[String]): Vector[String]   = words.map(word => word + "!")
        |  def once(left: ArraySeq[Int], right: ArraySeq[Int]) = left ++ right
        |  def local(rows: Vector[Vector[String]]): Vector[String] =
        |    rows.map { row => var line = ""; line = line + row.size; line }
        |  def lists(values: Vector[Int]): List[Int]          = values.foldLeft(List.empty[Int])(_ :+ _)
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "accumulating-concat"),
      List(
        "++ on the ArraySeq accumulator copies it on every step, which is quadratic",
        ":+ on the ArraySeq accumulator copies it on every step, which is quadratic",
        "+ on the String accumulator copies it on every step, which is quadratic",
        "++ on the Array accumulator copies it on every step, which is quadratic",
        "+ on the String accumulator copies it on every step, which is quadratic",
        ":+ on the ArraySeq accumulator copies it on every step, which is quadratic",
        "++ on the ArraySeq accumulator copies it on every step, which is quadratic"
      ).sorted
    )
    assert(tagged(found, "linear-in-loop").contains(":+ on a List copies it on every iteration"))
  }

  test("tuples destructured from branches inside loops are reported") {
    val found = warnings(
      """object Lints {
        |  def branches(values: Vector[Int], flag: Boolean): Int = values.map { value =>
        |    val (low, high) = if (flag) (value, value + 1) else (value + 1, value)
        |    low + high
        |  }.sum
        |  def matched(values: Vector[Int]): Int = {
        |    var total = 0
        |    var index = 0
        |    while (index < values.size) {
        |      val (sign, size) = values(index) match {
        |        case 0          => (0, 0)
        |        case v if v > 0 => (1, v)
        |        case v          => { val n = -v; (-1, n) }
        |      }
        |      total += sign * size
        |      index += 1
        |    }
        |    total
        |  }
        |  def nested(values: Vector[Int], flag: Boolean): Int = values.map { value =>
        |    val (a, b) = if (flag) (1, value) else if (value > 0) (value, 4) else throw IllegalStateException("x")
        |    a + b
        |  }.sum
        |  def outside(flag: Boolean): Int = { val (a, b) = if (flag) (1, 2) else (3, 4); a + b }
        |  def direct(values: Vector[Int]): Int = values.map { value => val (a, b) = (value, value + 1); a + b }.sum
        |  def named(values: Vector[(Int, Int)], fallback: (Int, Int), flag: Boolean): Int =
        |    values.map { pair => val (a, b) = if (flag) pair else fallback; a + b }.sum
        |  def mixed(values: Vector[(Int, Int)], flag: Boolean): Int =
        |    values.map { pair => val (a, b) = if (flag) pair else (0, 0); a + b }.sum
        |  def kept(values: Vector[Int], flag: Boolean): Int =
        |    values.map { value => val pair = if (flag) (value, 1) else (1, value); pair._1 }.sum
        |  def extracted(values: Vector[Int]): Int =
        |    values.map { value => val Seq(a, b) = (Seq(value, value + 1): @unchecked); a + b }.sum
        |  def thrown(values: Vector[Int], flag: Boolean): Int =
        |    values.map { value => val (a, b) = if (flag) (value, 1) else throw IllegalStateException("x"); a + b }.sum
        |}""".stripMargin,
      "hot=Lints"
    )
    assertEquals(
      tagged(found, "tuple-destructure-in-loop"),
      List.fill(3)(
        "the tuple destructured here is built in every branch and allocated where they merge, which the JIT cannot scalar-replace"
      )
    )
  }

  test("functions passed to inline parameters of inline methods are loop bodies") {
    val found = warnings(
      """trait Sinks { inline def block(inline body: Int => Unit): Unit }
        |object Lanes {
        |  inline def foreachActive(mask: Long)(inline body: Int => Unit): Unit = {
        |    var lane = 0
        |    while (lane < 64) { if ((mask >>> lane & 1L) != 0L) body(lane); lane += 1 }
        |  }
        |  inline def once(inline body: Int => Unit, eager: Int => Unit): Unit = { body(0); eager(1) }
        |  inline def let(value: Int)(inline body: Int => Unit): Unit = body(value)
        |  inline def forward(mask: Long)(inline body: Int => Unit): Unit = foreachActive(mask)(body)
        |  inline def each(values: Vector[Int])(inline body: Int => Unit): Unit = values.foreach(value => body(value))
        |  inline def single(inline body: Int => Unit): Unit = let(0)(body)
        |  def call(body: Int => Unit): Unit = body(0)
        |  def swapped(mask: Long, flag: Boolean, sink: Array[Int]): Unit = foreachActive(mask) { lane =>
        |    val (low, high) = if (flag) (lane, 1) else (1, lane)
        |    sink(lane) = low + high
        |  }
        |  def forwarded(mask: Long, flag: Boolean, sink: Array[Int]): Unit =
        |    forward(mask) { lane => val (low, high) = if (flag) (lane, 1) else (1, lane); sink(lane) = low + high }
        |  def traversed(values: Vector[Int], flag: Boolean, sink: Array[Int]): Unit =
        |    each(values) { lane => val (low, high) = if (flag) (lane, 1) else (1, lane); sink(lane) = low + high }
        |  def bound(flag: Boolean, sink: Array[Int]): Unit =
        |    let(1) { lane => val (low, high) = if (flag) (lane, 1) else (1, lane); sink(lane) = low + high }
        |  def passed(flag: Boolean, sink: Array[Int]): Unit =
        |    single { lane => val (low, high) = if (flag) (lane, 1) else (1, lane); sink(lane) = low + high }
        |  def eager(flag: Boolean, sink: Array[Int]): Unit =
        |    once(_ => (), { lane => val (low, high) = if (flag) (lane, 1) else (1, lane); sink(lane) = low + high })
        |  def plain(flag: Boolean, sink: Array[Int]): Unit =
        |    call { lane => val (low, high) = if (flag) (lane, 1) else (1, lane); sink(lane) = low + high }
        |}""".stripMargin,
      "hot=Lanes"
    )
    assertEquals(tagged(found, "tuple-destructure-in-loop").size, 3, found.mkString("\n"))
  }

  test("a function passed to an abstract inline method is not a loop body, and one passed to a concrete loop is") {
    val found = warnings(
      """case class Point(x: Int, y: Int)
        |trait Sinks { inline def block(inline body: Int => Unit): Unit }
        |object Lanes {
        |  inline def foreachActive(mask: Long)(inline body: Int => Unit): Unit = {
        |    var lane = 0
        |    while (lane < 64) { if ((mask >>> lane & 1L) != 0L) body(lane); lane += 1 }
        |  }
        |  inline def abstracted(inline sinks: Sinks, flag: Boolean, sink: Array[Int]): Unit =
        |    sinks.block { lane => val (low, high) = if (flag) (lane, 1) else (1, lane); sink(lane) = low + high }
        |  inline def hoistable(inline sinks: Sinks, base: Int, sink: Array[Int]): Unit =
        |    sinks.block { lane => val offset = Point(base, 1); sink(lane) = offset.x }
        |  def concrete(mask: Long, base: Int, sink: Array[Int]): Unit =
        |    foreachActive(mask) { lane => val offset = Point(base, 1); sink(lane) = offset.x }
        |}""".stripMargin,
      "hot=Lanes"
    )
    assertEquals(tagged(found, "invariant-in-loop").size, 1, found.mkString("\n"))
    assertEquals(tagged(found, "tuple-destructure-in-loop"), Nil, found.mkString("\n"))
  }

  test("a warning on an expression spanning several lines is shown at its first line, so its rendering stays small") {
    val table = (1 to 400).map(index => s"    \"row$index\"").mkString(",\n")
    val found = spanned(s"object Lints {\n  def rows: Array[String] = Array[String](\n$table\n  )\n}")
      .filter(_._1.contains("literal-table-in-def"))
    assertEquals(found.map((_, line, end) => (line, end)), List((1, 1)), found.mkString("\n"))
  }

  test("tables of constants built on every call are reported once, at the outermost table") {
    val source =
      """enum Shape { case Square, Wide }
        |object Lints {
        |  val shapes                          = Vector((2, 2), (3, 3))
        |  def matrices: Vector[(Int, Int)]    = Vector((2, 2), (3, 3), (2, 3))
        |  def names: List[String]             = List("x", "y", "z")
        |  def kinds: Vector[Shape]            = Vector(Shape.Square, Shape.Wide)
        |  def nested: List[List[Int]]         = List(List(1, 2), List(3))
        |  def lanes(values: Vector[Int]): Int = values.map(value => List((1, 2), (3, 4)).size + value).sum
        |  def single: List[Int]               = List(1)
        |  def computed(x: Int): List[Int]     = List(x, 2)
        |  def tested(name: String): Boolean   = Set("a", "b")(name)
        |  def tagged: Array[Int]              = Array(1, 2, 3)
        |  def keyed: Map[String, List[Int]]   = Map("a" -> List(1, 2), "b" -> List(3))
        |  def picked(points: Vector[(Double, Double)]): Vector[(Double, Double)] = Vector(points(0), points(1))
        |  def operands(kind: String): List[String] = kind match {
        |    case "VOP2" => List("VDST", "SRC0", "VSRC1")
        |    case _      => { List("VDST", "SRC0") ++ List("A", "B", "C") }
        |  }
        |  def picks(flag: Boolean): List[Int]     = if (flag) List(1, 2, 3) else List(4, 5, 6)
        |  def same(mods: Set[Int]): Boolean       = mods == Set(1, 2, 3)
        |  def within(mods: Set[Int]): Boolean     = mods.subsetOf(Set(4, 5, 6))
        |}
        |enum Form(val names: List[String]) { case Add extends Form(List("a", "b", "c")) }
        |object Generated {
        |  val forms = Vector(first(), second())
        |  private def first(): List[String]  = List("x", "y", "z")
        |  private def second(): List[String] = third()
        |  private def third(): List[String]  = List("u", "v", "w")
        |  private def shared(): List[String] = List("p", "q", "r")
        |  val initial = shared()
        |  def again: List[String] = shared()
        |}""".stripMargin
    val cold = List(
      "Vector((2, 2), (3, 3), (2, 3)) is a table of constants built on every call",
      "Map(\"a\" -> List(1, 2), \"b\" -> List(3)) is a table of constants built on every call",
      "List(List(1, 2), List(3)) is a table of constants built on every call",
      "List((1, 2), (3, 4)) is a table of constants built on every call"
    )
    val tiny = List(
      "Array(1, 2, 3) is a table of constants built on every call",
      "List(\"x\", \"y\", \"z\") is a table of constants built on every call",
      "List(\"A\", \"B\", \"C\") is a table of constants built on every call",
      "List(\"p\", \"q\", \"r\") is a table of constants built on every call"
    )
    assertEquals(tagged(warnings(source), "literal-table-in-def"), cold.sorted)
    assertEquals(tagged(warnings(source, "hot=Other"), "literal-table-in-def"), cold.sorted)
    assertEquals(tagged(warnings(source, "hot=Lints", "hot=Generated"), "literal-table-in-def"), (cold ++ tiny).sorted)
    assertEquals(tagged(warnings(source), "invariant-in-loop"), Nil)
  }

  test("a few constants are a table only in hot code, while a long table is one anywhere and is named by its factory") {
    val source =
      """object Lints {
        |  def arguments: List[String] = List("auth", "status", "--json")
        |  def months: Map[String, Int] = Map(
        |    "Jan" -> 1, "Feb" -> 2, "Mar" -> 3, "Apr" -> 4, "May" -> 5, "Jun" -> 6,
        |    "Jul" -> 7, "Aug" -> 8, "Sep" -> 9, "Oct" -> 10, "Nov" -> 11, "Dec" -> 12
        |  )
        |  def letters: List[String] = List("a", "b", "c", "d", "e")
        |}""".stripMargin
    val found = tagged(warnings(source), "literal-table-in-def")
    assertEquals(
      found.map(_.takeWhile(_ != '(')).sorted,
      List("List", "Map")
    )
    assert(found.exists(_.startsWith("Map(...) is a table of constants")), found.mkString("\n"))
    assert(tagged(warnings(source, "hot=Lints"), "literal-table-in-def").exists(_.startsWith("List(\"auth\"")))
    assert(!found.exists(_.startsWith("List(\"auth\"")), found.mkString("\n"))
  }

  test("a settled match whose arms depend on the loop is reported only in hot code, where restructuring pays") {
    val source =
      """enum Format { case F32, F16 }
                   |object Lints {
                   |  def formats(values: Vector[Float], format: Format, scale: Float): Float =
                   |    values.map(value => format match { case Format.F32 => value; case Format.F16 => value * scale }).sum
                   |  def widths(values: Vector[Int], format: Format): Int =
                   |    values.map(value => format match { case Format.F32 => 4; case Format.F16 => 2 }).sum
                   |  def long(values: Vector[Float], format: Format, scale: Float): Float =
                   |    values.map { value =>
                   |      val step1 = value * 1
                   |      val step2 = value * 2
                   |      val step3 = value * 3
                   |      val step4 = value * 4
                   |      val step5 = value * 5
                   |      val step6 = value * 6
                   |      val step7 = value * 7
                   |      val step8 = value * 8
                   |      val step9 = value * 9
                   |      val step10 = value * 10
                   |      val step11 = value * 11
                   |      format match { case Format.F32 => value + step1; case Format.F16 => value * scale + step11 }
                   |    }.sum
                   |}""".stripMargin
    assertEquals(tagged(warnings(source), "invariant-in-loop").size, 1)
    assertEquals(
      tagged(warnings(source, "hot=Lints"), "invariant-in-loop"),
      List(
        "the match on format does not depend on the loop",
        "the match on format does not depend on the loop, but its cases do"
      )
    )
  }

  test("a check on an expression is suppressed by @nowarn ascribed to it, which then counts as used") {
    val source =
      """import scala.annotation.nowarn
        |enum Format { case F32, F16 }
        |object Lints {
        |  def widths(values: Vector[Int], format: Format): Int =
        |    values.map(value =>
        |      (format match {
        |        case Format.F32 => 4
        |        case Format.F16 => 2
        |      }): @nowarn("msg=bonsai:invariant-in-loop")
        |    ).sum
        |}""".stripMargin
    assertEquals(warnings(source, "-Wunused:nowarn", "disable=nowarn-suppression"), Nil)
    val nested =
      """import scala.annotation.nowarn
        |object Lints {
        |  def run(values: Vector[Int], barrier: Option[Int]): Vector[Either[String, Int]] =
        |    values.map { value =>
        |      (for {
        |        a <- Right[String, Int](value)
        |        b <- barrier match {
        |          case Some(word) => Right(word + a)
        |          case None       => Right(a)
        |        }
        |      } yield b): @nowarn("msg=bonsai:invariant-in-loop")
        |    }
        |}""".stripMargin
    assertEquals(warnings(nested, "-Wunused:nowarn", "hot=Lints", "disable=nowarn-suppression"), Nil)
    assertEquals(warnings(nested.replace(": @nowarn(\"msg=bonsai:invariant-in-loop\")", ""), "hot=Lints").size, 1)
  }

  test("a check on an expression is suppressed by @SuppressWarnings ascribed to it") {
    def loop(annotation: String) =
      s"""enum Format { case F32, F16 }
         |object Lints {
         |  def widths(values: Vector[Int], format: Format): Int =
         |    values.map(value =>
         |      (format match {
         |        case Format.F32 => 4
         |        case Format.F16 => 2
         |      })$annotation
         |    ).sum
         |}""".stripMargin
    assertEquals(warnings(loop(""), "hot=Lints").size, 1)
    assertEquals(warnings(loop(": @SuppressWarnings(Array(\"bonsai:invariant-in-loop\"))"), "hot=Lints"), Nil)
    assertEquals(warnings(loop(": @SuppressWarnings(Array(\"bonsai\"))"), "hot=Lints"), Nil)
    assertEquals(warnings(loop(": @SuppressWarnings(Array(\"bonsai:unbounded-wait\"))"), "hot=Lints").size, 1)
    assertEquals(
      compile("Lints.scala", loop(": @SuppressWarnings(Array(\"bonsai\"))"), "hot=Lints", "error=*")._1,
      Nil
    )
    val close =
      """object Lints {
        |  def leak(): Int = {
        |    val stream = java.io.ByteArrayInputStream(Array[Byte](1))
        |    val read  = stream.read()
        |    stream.close()%s
        |    read
        |  }
        |}""".stripMargin
    assert(warnings(close.format("")).exists(_.contains("bonsai:unguarded-close")))
    assertEquals(
      warnings(
        close.format(": @SuppressWarnings(Array(\"bonsai:unguarded-close\"))")
      ).filter(_.contains("bonsai:")),
      Nil
    )
  }

  test("@nowarn naming only bonsai messages is reported, and rewritten to @SuppressWarnings") {
    val definition =
      """import scala.annotation.nowarn
        |object Lints {
        |  @nowarn("msg=bonsai:array-equality")
        |  def check(left: Array[Int], right: Array[Int]) = left == right
        |}""".stripMargin
    val found = warnings(definition)
    assertEquals(tagged(found, "nowarn-suppression").size, 1, found.mkString("\n"))
    assert(
      found.exists(_.contains("""use @SuppressWarnings(Array("bonsai:<id>")) instead of @nowarn""")),
      found.mkString("\n")
    )
    assertEquals(tagged(found, "array-equality"), Nil)
    assertEquals(
      rewritten(definition).linesIterator.toList.drop(2).head,
      """  @SuppressWarnings(Array("bonsai:array-equality"))"""
    )
    val expression =
      """object Lints {
        |  def leak(): Int = {
        |    val stream = java.io.ByteArrayInputStream(Array[Byte](1))
        |    val read  = stream.read()
        |    stream.close(): @scala.annotation.nowarn("msg=bonsai:unguarded-close")
        |    read
        |  }
        |}""".stripMargin
    val inline = warnings(expression)
    assertEquals(tagged(inline, "nowarn-suppression").size, 1, inline.mkString("\n"))
    assertEquals(tagged(inline, "unguarded-close"), Nil)
    assert(
      rewritten(expression).contains("""stream.close(): @SuppressWarnings(Array("bonsai:unguarded-close"))"""),
      rewritten(expression)
    )
  }

  test("@nowarn filters that match the report itself cannot suppress it, so it is an error") {
    List("bonsai", "bonsai:nowarn-suppression").foreach { filter =>
      val source =
        s"""import scala.annotation.nowarn
           |object Lints {
           |  @nowarn("msg=$filter")
           |  def check(left: Array[Int], right: Array[Int]) = left == right
           |}""".stripMargin
      val found = diagnostics(source)
      assertEquals(found.count(_.startsWith("error: ")), 1, found.mkString("\n"))
      assert(found.exists(_.endsWith("[bonsai:nowarn-suppression]")), found.mkString("\n"))
      assert(!diagnostics(source, "disable=nowarn-suppression").exists(_.startsWith("error: ")), filter)
    }
  }

  test("@nowarn without a bonsai filter is not reported") {
    def definition(annotation: String) =
      s"""import scala.annotation.nowarn
         |object Lints {
         |  $annotation
         |  def check(left: Array[Int], right: Array[Int]) = left == right
         |}""".stripMargin
    List(
      "@nowarn",
      "@nowarn()",
      "@nowarn(\"msg=something else\")",
      "@nowarn(\"cat=deprecation\")",
      "@nowarn(\"msg=bonsai:array-equality&cat=deprecation\")"
    ).foreach { annotation =>
      assertEquals(tagged(warnings(definition(annotation)), "nowarn-suppression"), Nil, annotation)
    }
    val expression =
      """import scala.annotation.nowarn
        |object Lints {
        |  def number(s: String): Int = s.toInt: @nowarn("msg=something else")
        |}""".stripMargin
    assertEquals(tagged(warnings(expression), "nowarn-suppression"), Nil)
    assertEquals(warnings(definition("@nowarn(\"msg=bonsai:array-equality\")"), "disable=nowarn-suppression"), Nil)
  }

  private val refinement =
    """import scala.language.implicitConversions
      |object constraint {
      |  sealed trait AtLeast[N]
      |  sealed trait GreaterThan[N]
      |  infix opaque type where[A, P] = A
      |  object where {
      |    implicit def literal[P](value: Int & Singleton): Int where P = value
      |    implicit def unrefine[A, P](value: A where P): A = value
      |  }
      |  infix type >=[A, N] = A where AtLeast[N]
      |  extension (value: Int where AtLeast[0]) {
      |    def +(other: Int >= 0): Int where AtLeast[0] = value + (other: Int)
      |    def toLong: Long where AtLeast[0]          = (value: Int).toLong
      |  }
      |  extension (value: Int where GreaterThan[0]) def -(other: Int where GreaterThan[0]): Int = (value: Int) - (other: Int)
      |}
      |""".stripMargin

  test("an extension a member of the type's upper bound replaces never runs") {
    val bounded                  = refinement.replace("opaque type where[A, P] = A", "opaque type where[A, P] <: A = A")
    def shadowed(source: String) = tagged(warnings(source), "shadowed-extension")
    assertEquals(
      shadowed(bounded),
      List(
        "the extension + on Int where constraint.AtLeast[(0 : Int)] never runs: Int has the member +(x: Int): Int, " +
          "which calls take before any extension",
        "the extension - on Int where constraint.GreaterThan[(0 : Int)] never runs: Int has the member -(x: Int): " +
          "Int, which calls take before any extension",
        "the extension toLong on Int where constraint.AtLeast[(0 : Int)] never runs: Int has the member toLong=> " +
          "Long, which calls take before any extension"
      )
    )
    assertEquals(shadowed(refinement), Nil)
    assertEquals(
      tagged(
        warnings("object Lints {\n  type Count = Int\n  extension (count: Count) def toLong: Long = 1L\n}"),
        "shadowed-extension"
      )
        .size,
      1
    )
  }

  test("a method whose whole body calls itself recurses until the stack overflows, and fails the build") {
    val found = diagnostics(
      """object Lints {
        |  def apply(value: Long): Long          = apply(value)
        |  def shifted(value: Int): Int          = shifted(value - 1)
        |  def guarded(value: Int): Int          = if (value > 0) guarded(value - 1) else 0
        |  def other(lints: Inner, n: Int): Int  = lints.other(lints, n)
        |  class Inner { def other(lints: Inner, n: Int): Int = n }
        |  class Chain(next: Chain) { def size(n: Int): Int = next.size(n) }
        |  def overloaded(value: Int): Int       = overloaded(value.toLong).toInt
        |  def overloaded(value: Long): Long     = value
        |}""".stripMargin
    ).filter(_.endsWith("[bonsai:accidental-self-call]")).map(_.takeWhile(_ != ',')).sorted
    assertEquals(
      found,
      List(
        "error: apply does nothing but call itself",
        "error: shifted does nothing but call itself"
      )
    )
  }

  test("a conversion that bypasses a checked extension, and a refinement inferred from mixed values, are information") {
    val source = refinement +
      """object Lints {
        |  import constraint.*
        |  def plain(n: Int >= 0, k: Int): Int = n * k
        |  def checked(n: Int >= 0, k: Int >= 0): Int >= 0 = n + k
        |  def bypassed(n: Int >= 0, k: Int): Int = n - k
        |  @scala.annotation.targetName("scaleChecked") def scale(n: Int where GreaterThan[0]): Int = 1
        |  def scale(n: Int): Int = 2
        |  def scaled(m: Int >= 0): Int = scale(m)
        |  def mixed(n: Int >= 0) = n + 1
        |  def defaulted(n: Int >= 0 = 1): Int = n
        |  def longs(n: Long >= 0, k: Long): Long = n - k
        |  def copied(n: Int >= 0) = n
        |  def written(n: Int >= 0): Int >= 0 = n + 1
        |}""".stripMargin
    val all = diagnostics(source, "checked=constraint.where")
    assertEquals(all.filter(_.startsWith("error")), Nil)
    val found = all.filter(_.contains("[bonsai:"))
    assertEquals(
      found.map(_.takeWhile(_ != ';')).sorted,
      List(
        "info: - on Int >= (0 : Int) converts it and runs Int.-, unchecked: the checked extension - takes other " +
          "refinements",
        "info: mixed is inferred as Int where constraint.AtLeast[(0 : Int)] from refined and plain values, so later " +
          "calls on it resolve by that refinement",
        "info: scale takes the unchecked overload, since this argument's refinement Int >= (0 : Int) does not conform " +
          "to the overload taking a checked type"
      )
    )
    assertEquals(diagnostics(source).filter(_.contains("[bonsai:")), Nil)
  }

  test("comparisons that box an opaque primitive, and for floats are not IEEE equality, are reported") {
    val source =
      """object units {
        |  opaque type Tagged[A] = A
        |  object Tagged {
        |    def apply[A](value: A): Tagged[A] = value
        |    given [A]: CanEqual[Tagged[A], A] = CanEqual.derived
        |    given [A]: CanEqual[A, Tagged[A]] = CanEqual.derived
        |  }
        |}
        |object Lints {
        |  import units.*
        |  def floats(a: Tagged[Double], b: Tagged[Double]): Boolean = a == b
        |  def literals(a: Tagged[Double]): Int = a match { case 0.0 => 1; case _ => 0 }
        |  def ints(a: Tagged[Int]): Int = a match { case 1 => 1; case _ => 0 }
        |  def explicit(a: Tagged[Int], b: Tagged[Int]): Boolean = a.equals(b)
        |  def wider(a: Tagged[Int], b: Any): Boolean = a == b
        |  def bound(a: Tagged[Int]): Int = a match { case value => 0 }
        |}""".stripMargin
    def boxed(options: String*) = tagged(warnings(source, options*), "boxed-opaque-equality")
    assertEquals(
      boxed(),
      List(
        "on units.Tagged[Double], an opaque Double, this matches each literal case with Objects.equals on boxed values, " +
          "not a switch, where -0.0 differs from 0.0 and NaN equals NaN"
      )
    )
    assertEquals(boxed("hot=Lints").size, 4)
  }

  test("a type test no value of the static type can pass is an error, inside tuple patterns too") {
    val found = diagnostics(
      """object Lints {
        |  class Form; case class Move(to: Int) extends Form; case class Load(from: Int) extends Form
        |  class Other
        |  trait Marked
        |  def moved(pair: (Load, Int)): Int = pair match { case (_: Move, n) => n; case _ => 0 }
        |  def other(form: Form): Int = (form, 1) match { case (_: Other, n) => n; case _ => 0 }
        |  def marked(load: Load): Int = (load, 1) match { case (_: Marked, n) => n; case _ => 0 }
        |  def open(form: Form): Int = (form, 1) match { case (_: Marked, n) => n; case _ => 0 }
        |  def fine(form: Form): Int = (form, 1) match { case (_: Move, n) => n; case _ => 0 }
        |}""".stripMargin
    ).filter(_.endsWith("[bonsai:unrelated-type-test]")).map(_.takeWhile(_ != ',')).sorted
    assertEquals(
      found,
      List(
        "error: no Lints.Form is a Lints.Other",
        "error: no Lints.Load is a Lints.Move"
      )
    )
  }

  test("a type test on the value an option or either yields is against that value, not the fields of a product") {
    val found = diagnostics(
      """object Lints {
        |  class Coding; case class Chunked(codings: Coding); case class Addr(name: Coding, port: Int)
        |  def chunked(found: Option[Chunked]): Int = found match { case Some(chunked: Chunked) => 1; case _ => 0 }
        |  def routed(found: Either[String, Addr]): Int = found match { case Right(addr: Addr) => addr.port; case _ => 0 }
        |  def coded(found: Option[Chunked]): Int = found match { case Some(_: Coding) => 1; case _ => 0 }
        |}""".stripMargin
    ).filter(_.endsWith("[bonsai:unrelated-type-test]")).map(_.takeWhile(_ != ',')).sorted
    assertEquals(found, List("error: no Lints.Chunked is a Lints.Coding"))
  }

  test("a case class is only closed when final, since a class can extend it and mix a trait in") {
    val found = diagnostics(
      """object Lints {
        |  trait Marked
        |  case class Load(from: Int); final case class Pinned(from: Int)
        |  class Mixed extends Load(0) with Marked
        |  def loaded(load: Load): Int = load match { case _: Marked => 1; case _ => 0 }
        |  def pinned(pin: Pinned): Int = pin match { case _: Marked => 1; case _ => 0 }
        |}""".stripMargin
    ).filter(_.endsWith("[bonsai:unrelated-type-test]")).map(_.takeWhile(_ != ',')).sorted
    assertEquals(found, List("error: no Lints.Pinned is a Lints.Marked"))
  }

  test("tests against constants written in place are reported for the concept they leave unnamed") {
    val found = warnings(
      """enum Isa { case Gfx9, Gfx11, Rdna4 }
        |object Lints {
        |  def directive(name: String): Int = name match {
        |    case alias if Set(".set", ".equ")(alias) => 1
        |    case _                                   => 0
        |  }
        |  def halves(isa: Isa): Boolean                         = Set(Isa.Gfx11, Isa.Rdna4)(isa)
        |  def guard(binding: String): Boolean                   = Set("Pg", "UPg")(binding)
        |  def operand(binding: String, known: Boolean): Boolean = Set("UPg", "Pg").contains(binding) || known
        |  def parts(parts: Vector[String]): Int                 = parts.count(part => Set("x", "y", "z").contains(part))
        |  def rows(rows: Vector[Vector[Int]]): Int              = rows.map(row => row.count(Set(1, 2))).sum
        |  def widths(widths: Vector[Int]): Vector[Int]          = widths.filter(Set(8, 16))
        |  def once(name: String): Boolean                       = Set("a", "b")(name)
        |  def scoped(name: String, outer: Int): Int             = outer match { case 1 if Set("c", "d")(name) => 1; case _ => 0 }
        |  def computed(name: String, other: String): Boolean    = Set("e", other)(name)
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "inline-set-membership"),
      List(
        "Set(\".set\", \".equ\") in a guard tests what the pattern binds",
        "Set(Isa.Gfx11, Isa.Rdna4) names a category of Isa",
        "Set(\"Pg\", \"UPg\") appears 2 times in this file",
        "Set(\"UPg\", \"Pg\") appears 2 times in this file",
        "what does Set(\"x\", \"y\", \"z\") stand for? it is rebuilt and hashed on every iteration",
        "Set(1, 2) is a table rebuilt on every iteration"
      ).sorted
    )
    assertEquals(tagged(found, "invariant-in-loop"), Nil)
  }

  test("allocations, matches and traversals inside loops that do not depend on the loop are reported") {
    val found = warnings(
      """import scala.collection.mutable
        |case class Point(x: Int, y: Int)
        |enum Format { case F32, F16 }
        |class Counter(start: Int) { var count = start }
        |class Walker(start: Int, end: Int) { val seen = mutable.Set(start, end) }
        |class Cell { var value = 0 }
        |class Emitter { private val emitted = mutable.Buffer.empty[Int]; def fresh(v: Int): Int = { emitted += v; emitted.size } }
        |trait Context { def emit(v: Int): Unit; final def temp(v: Int): Int = { emit(v); v } }
        |object Lints {
        |  val table = Vector("x").map(_ => "b+".r)
        |  def id(name: String): Int = name.length
        |  def sets(values: Vector[Long]): Int = values.count(value => Set(1L, 2L, 4L, 8L).contains(value))
        |  def ids(values: Vector[Int], operands: Vector[String]): Int = values.map(value => operands.map(id(_)).size + value).sum
        |  def formats(values: Array[Float], format: Format, scale: Int): Float = {
        |    var total = 0f
        |    var index = 0
        |    while (index < values.length) {
        |      total += (format match { case Format.F32 => values(index); case Format.F16 => values(index) * scale })
        |      index += 1
        |    }
        |    total
        |  }
        |  def points(values: Vector[Int], x: Int): Vector[Point]     = values.map(value => Point(x, 1))
        |  def regex(lines: Vector[String], prefix: String): Int      = lines.count(line => prefix.r.matches(line))
        |  def converted(values: Vector[Int], keys: List[Int]): Int   = values.count(value => keys.toSet(value))
        |  def nested(rows: Vector[Vector[Int]], key: Int): Int       =
        |    rows.map(row => row.count(value => List(key, 2).contains(value))).sum
        |  def element(values: Vector[Int]): Vector[Point]            = values.map(value => Point(value, 1))
        |  def mapped(values: Vector[Int], keys: Vector[Int]): Int    = values.map(value => keys.map(_ + value).sum).sum
        |  def accessors(values: Vector[Int], point: Point): Int      = values.map(value => point.x + value * 2).sum
        |  def strings(values: Vector[Int], name: String): Int        = values.map(value => new String(name).length).sum
        |  def buffers(values: Vector[Int]): Int                      = values.map(value => mutable.ArrayBuffer(1, 2).size).sum
        |  def mutated(values: Vector[Int]): Int = {
        |    val seen = mutable.ArrayBuffer.empty[Int]
        |    values.map { value => seen += value; seen.toVector.size }.sum
        |  }
        |  def reassigned(values: Vector[Int], start: List[Int]): Int = {
        |    var keys = start
        |    values.count { value => keys = value :: keys; keys.toSet(value) }
        |  }
        |  def deferred(values: Vector[Int], cache: Option[Set[Long]]): Int =
        |    values.count(value => cache.getOrElse(Set(1L, 2L)).contains(value.toLong))
        |  def lazily(values: Vector[Int]): Int = values.count { value => lazy val keys = Set(1, 2); value > 0 && keys(value) }
        |  def thrown(values: Vector[Int], message: String): Int =
        |    values.map(value => if (value < 0) throw IllegalArgumentException(message) else value).sum
        |  def counters(values: Vector[Int]): Int = values.map(value => Counter(1).count).sum
        |  def once(x: Int): Point = Point(x, 1)
        |  def matched(values: Vector[Int]): Int = values.map(value => value match { case 0 => 1; case _ => 2 }).sum
        |  def hoisted(values: Vector[Int], older: Vector[Int], newer: Vector[Int]): Int =
        |    values.count(value => (older ++ newer).exists(_ > value))
        |  def literal(lines: Vector[String]): Int = lines.count(line => "a+".r.matches(line))
        |  def interpolated(values: Vector[Int], name: String): Vector[String] = values.map(value => s"$name!")
        |  def wrapped(values: Vector[Option[Int]], key: Int): Int = values.count(value => value == Some(key))
        |  def single(values: Vector[Int], flag: Boolean): Vector[Int] = values.flatMap(value => if (flag) Vector(2) else Nil)
        |  def walkers(values: Vector[Int]): Int = values.map(value => Walker(1, 2).seen.size).sum
        |  def widths(values: Vector[Int], format: Format): Int =
        |    values.map(value => format match { case Format.F32 => 4; case Format.F16 => 2 }).sum
        |  def sampled(runs: Vector[Int], kinds: Vector[Int], random: scala.util.Random): Int =
        |    runs.map(run => kinds.map(kind => random.nextInt(kind)).sum).sum
        |  def waits(cells: Vector[Cell], rounds: Int): Int = (0 until rounds).map(_ => cells.map(_.value).sum).sum
        |  def emitted(rows: Vector[Int], inputs: Vector[Int], emitter: Emitter): Int =
        |    rows.map(row => inputs.map(emitter.fresh).sum).sum
        |  def lowered(rows: Vector[Int], inputs: Vector[Int])(using context: Context): Int =
        |    rows.map(row => inputs.map(context.temp).sum).sum
        |}""".stripMargin,
      "hot=Lints"
    )
    assertEquals(
      tagged(found, "invariant-in-loop"),
      List(
        "\"b+\".r does not depend on the loop",
        "operands.map(id(_)) does not depend on the loop",
        "the match on format does not depend on the loop",
        "the match on format does not depend on the loop, but its cases do",
        "Point(x, 1) does not depend on the loop",
        "prefix.r does not depend on the loop",
        "keys.toSet does not depend on the loop",
        "List(key, 2) does not depend on the loop"
      ).sorted
    )
    assertEquals(
      tagged(found, "linear-in-loop"),
      List("++ on a Vector rebuilds the same collection on every iteration")
    )
    assertEquals(tagged(found, "regex-in-def"), List("\"a+\".r compiles the pattern on every call"))
  }

  test("searches by key inside loops are reported where the owner of the sequence already keys it") {
    val found = warnings(
      """case class Form(name: String, id: Int)
        |object Database {
        |  val forms: Vector[Form]            = Vector.empty
        |  lazy val byForm: Map[String, Form] = forms.map(form => form.name -> form).toMap
        |}
        |class Layout(val format: String)
        |object Layouts {
        |  val layouts: List[Layout]                                = Nil
        |  private val byFormat: Map[String, Layout]                = Map.empty
        |  def forFormat(isa: Int, format: String): Option[Layout] = byFormat.get(format)
        |}
        |object Plain { val forms: Vector[Form] = Vector.empty }
        |object Ids   { val forms: Vector[Form] = Vector.empty; val byName: Map[String, Form] = Map.empty }
        |object Lints {
        |  def names(keys: Vector[String]): Vector[Option[Form]] = keys.map(key => Database.forms.find(_.name == key))
        |  def present(keys: Vector[String]): Int                = keys.count(key => Database.forms.exists(form => key == form.name))
        |  def positions(keys: Vector[String]): Vector[Int]      = keys.map(key => Database.forms.indexWhere(_.name == key))
        |  def first(keys: Vector[String]): Vector[Form]         = keys.map(key => Database.forms.filter(_.name == key).head)
        |  def layouts(formats: Vector[String]): Vector[Option[Layout]] =
        |    formats.map(format => Layouts.layouts.find(_.format == format))
        |  def plain(keys: Vector[String]): Vector[Option[Form]] = keys.map(key => Plain.forms.find(_.name == key))
        |  def ids(keys: Vector[Int]): Vector[Option[Form]]      = keys.map(key => Ids.forms.find(_.id == key))
        |  def once(key: String): Option[Form]                   = Database.forms.find(_.name == key)
        |  def prefixed(keys: Vector[String]): Vector[Option[Form]] =
        |    keys.map(key => Database.forms.find(_.name.startsWith(key)))
        |}""".stripMargin
    )
    assertEquals(
      lines(found, "keyed-lookup-in-loop").sorted,
      List(
        "find over Database.forms scans it on every iteration; use byForm instead [bonsai:keyed-lookup-in-loop]",
        "exists over Database.forms scans it on every iteration; use byForm instead [bonsai:keyed-lookup-in-loop]",
        "indexWhere over Database.forms scans it on every iteration; use byForm instead [bonsai:keyed-lookup-in-loop]",
        "filter over Database.forms scans it on every iteration; use byForm instead [bonsai:keyed-lookup-in-loop]",
        "find over Layouts.layouts scans it on every iteration; use forFormat instead [bonsai:keyed-lookup-in-loop]",
        "find over Plain.forms relates its elements to the loop variable and scans them on every iteration; build an index of them first [bonsai:keyed-lookup-in-loop]",
        "find over Ids.forms relates its elements to the loop variable and scans them on every iteration; build an index of them first [bonsai:keyed-lookup-in-loop]"
      ).sorted
    )
  }

  test("folds and per-element tuples that box primitives are reported in the classes the build marks hot") {
    val source =
      """package demo.kernel
                   |object Lanes {
                   |  def sum(values: Array[Long]): Long        = values.foldLeft(0L)(_ + _)
                   |  def ranged(count: Int): Double             = (0 until count).foldLeft(0.0)((total, index) => total + index)
                   |  def indices(values: Vector[Long]): Long    = values.indices.foldLeft(0L)((total, index) => total + values(index))
                   |  def pairs(values: IndexedSeq[Long]): (Long, Long) =
                   |    values.foldLeft((0L, 0L))((pair, value) => (pair._1 + value, pair._2.max(value)))
                   |  def optional(values: Array[Int]): Option[Int] =
                   |    values.foldLeft(Option.empty[Int])((best, value) => best.map(_.max(value)).orElse(Some(value)))
                   |  def reduced(values: Array[Double]): Double       = values.reduce(_ + _)
                   |  def scanned(values: Array[Float]): Array[Float]  = values.scanLeft(0f)(_ + _)
                   |  def right(values: Vector[Long]): Long            = values.foldRight(0L)(_ + _)
                   |  def streamed(values: Iterator[Double]): Double   = values.foldLeft(0.0)(_ + _)
                   |  def tabulated: IndexedSeq[(Int, Long)]           = IndexedSeq.tabulate(32)(index => (index, index.toLong))
                   |  def indexed(values: Array[Long]): Array[(Long, Int)] = values.zipWithIndex
                   |  def zipped(left: Array[Int], right: Array[Int]): Array[(Int, Int)] = left.zip(right)
                   |  def lanes(values: Vector[Int]): Int              = values.zipWithIndex.map(_._2).sum
                   |  def counted(values: Array[Int]): Int             = values.foldLeft(0)(_ + _)
                   |  def errors(values: Vector[String]): Either[String, Long] =
                   |    values.foldLeft[Either[String, Long]](Right(0L))((total, value) => total.map(_ + value.length))
                   |  def texts(values: Vector[String]): Option[String] =
                   |    values.foldLeft(Option.empty[String])((first, value) => first.orElse(Some(value)))
                   |  def listed(values: List[Long]): Long              = values.foldLeft(0L)(_ + _)
                   |  def names(values: Vector[String]): Vector[(String, Int)] = values.zipWithIndex
                   |}""".stripMargin
    val expected = List(
      "foldLeft with a Long accumulator boxes it on every step",
      "foldLeft with a Double accumulator boxes it on every step",
      "foldLeft with a Long accumulator boxes it on every step",
      "foldLeft with a (Long, Long) accumulator boxes it on every step",
      "foldLeft with an Option[Int] accumulator boxes it on every step",
      "reduce with a Double accumulator boxes it on every step",
      "scanLeft with a Float accumulator boxes it on every step",
      "foldRight with a Long accumulator boxes it on every step",
      "foldLeft with a Double accumulator boxes it on every step",
      "tabulate builds a boxed (Int, Long) for every element",
      "zipWithIndex builds a boxed (Long, Int) for every element",
      "zipWithIndex builds a boxed (Int, Int) for every element",
      "zip builds a boxed (Int, Int) for every element"
    ).sorted
    assertEquals(tagged(warnings(source, "hot=demo.kernel.**"), "boxing-fold"), expected)
    assertEquals(tagged(warnings(source, "hot=demo.*.Lanes"), "boxing-fold"), expected)
    assertEquals(tagged(warnings(source, "hot=**.Lanes", "hot=demo.other.*"), "boxing-fold"), expected)
    assertEquals(tagged(warnings(source, "hot=demo.kern*.*"), "boxing-fold"), expected)
    assertEquals(tagged(warnings(source), "boxing-fold"), Nil)
    assertEquals(tagged(warnings(source, "hot=demo.other.**"), "boxing-fold"), Nil)
    assertEquals(tagged(warnings(source, "hot=demo.*"), "boxing-fold"), Nil)
    assertEquals(tagged(warnings(source, "hot=demo.**", "hot=!**.Lanes"), "boxing-fold"), Nil)
    assertEquals(tagged(warnings(source, "hot=demo.**", "hot=!**.Other"), "boxing-fold"), expected)
    assertEquals(
      tagged(
        warnings(
          source.replace("object Lanes", "@SuppressWarnings(Array(\"bonsai:boxing-fold\")) object Lanes"),
          "hot=**"
        ),
        "boxing-fold"
      ),
      Nil
    )
  }

  test("named patterns and named arguments are already named") {
    assume(Compilations.minor >= 7, "named patterns need Scala 3.7")
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
    assertEquals(found.filter(_.contains("bonsai:named-pattern")), Nil)
    assertEquals(found.filter(_.contains("bonsai:same-type-arguments")), Nil)
    assertEquals(found.filter(_.contains("bonsai:boolean-argument")), Nil)
  }

  test("patterns the compiler writes, arguments of the wrong type and generic values are not misreported") {
    assume(Compilations.minor >= 5, "-source:3.7 is accepted from Scala 3.5")
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
    assertEquals(found.filter(_.contains("[bonsai:")), Nil)
  }

  test("side effects a method returning an effect performs when called, rather than when the effect runs") {
    assume(Compilations.minor >= 7, "named tuples need Scala 3.7")
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

  test("side effects the shared tables describe are eager in a method returning an effect") {
    val found = warnings(
      """import java.util.concurrent.atomic.AtomicInteger
        |import java.util.concurrent.atomic.LongAdder
        |import scala.collection.mutable.ArrayBuffer
        |trait Monad[F[_]] { def unit[A](value: A): F[A] }
        |class Box { var value = 0 }
        |class Session {
        |  private val cell    = AtomicInteger(0)
        |  private val adder   = LongAdder()
        |  private val history = ArrayBuffer.empty[Int]
        |  def clock[F[_]](monad: Monad[F]): F[Long] = monad.unit(System.nanoTime())
        |  def roll[F[_]](monad: Monad[F]): F[Double] = monad.unit(Math.random())
        |  def bumped[F[_]](monad: Monad[F]): F[Int] = monad.unit(cell.incrementAndGet())
        |  def counted[F[_]](monad: Monad[F]): F[Unit] = { adder.increment(); monad.unit(()) }
        |  def remembered[F[_]](monad: Monad[F]): F[Int] = { history += 1; monad.unit(history.size) }
        |  def appended[F[_]](monad: Monad[F], sink: StringBuilder): F[Int] = { sink.append("x"); monad.unit(sink.length) }
        |  def aliased[F[_]](monad: Monad[F]): F[Int] = { val alias = history; alias.clear(); monad.unit(0) }
        |  def written[F[_]](monad: Monad[F], path: java.nio.file.Path): F[Int] = {
        |    java.nio.file.Files.writeString(path, "x")
        |    monad.unit(1)
        |  }
        |  def read[F[_]](monad: Monad[F]): F[Int] = monad.unit(cell.get() + history.size + adder.sum().toInt)
        |  def decoded[F[_]](monad: Monad[F]): F[Int] = monad.unit(java.nio.ByteBuffer.allocate(4).putInt(1).position())
        |  def own[F[_]](monad: Monad[F]): F[Int] = {
        |    val sink = new StringBuilder
        |    sink.append("x")
        |    val box = new Box
        |    box.value = 1
        |    val queue = scala.collection.mutable.Queue.empty[Int]
        |    queue.enqueue(box.value)
        |    monad.unit(sink.length + queue.size)
        |  }
        |  def each[F[_]](monad: Monad[F], values: Option[Int]): F[Int] = { values.foreach(_ => ()); monad.unit(1) }
        |  def checked[F[_]](monad: Monad[F], size: Int): F[Int] = { require(size > 0); assert(size < 10); monad.unit(size) }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "eager-effect"),
      List(
        "aliased returns F but calls clear when called, not when the F runs",
        "appended returns F but calls append when called, not when the F runs",
        "bumped returns F but calls incrementAndGet when called, not when the F runs",
        "clock returns F but draws nanoTime when called, not when the F runs",
        "counted returns F but calls increment when called, not when the F runs",
        "remembered returns F but calls += when called, not when the F runs",
        "roll returns F but draws random when called, not when the F runs",
        "written returns F but calls writeString when called, not when the F runs"
      ).sorted
    )
  }

  test("queries of a fresh buffer and lookups of loggers are no side effects, while printing and logging are") {
    val found = warnings(
      """import java.io.ByteArrayOutputStream
        |import java.io.PrintStream
        |trait Console[F[_]] { def error(message: String): F[Unit] }
        |trait Monad[F[_]] { def unit[A](value: A): F[A] }
        |object Reports {
        |  def render[F[_]](console: Console[F])(throwable: Throwable): F[Unit] = {
        |    val buffer = new ByteArrayOutputStream()
        |    val stream = new PrintStream(buffer)
        |    throwable.printStackTrace(stream)
        |    console.error(buffer.toString)
        |  }
        |  def level[F[_]](monad: Monad[F]): F[String] = monad.unit(java.util.logging.Logger.getLogger("x").getLevel.getName)
        |  def announce[F[_]](monad: Monad[F], logger: java.util.logging.Logger): F[Unit] = { logger.info("x"); monad.unit(()) }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "eager-effect"),
      List(
        "announce returns F but calls info when called, not when the F runs",
        "render returns F but calls printStackTrace when called, not when the F runs"
      ).sorted
    )
  }

  test("java.net.http values are read, while the requests they send are side effects") {
    val found = warnings(
      """import java.net.http.HttpClient
        |import java.net.http.HttpRequest
        |import java.net.http.HttpResponse
        |trait Monad[F[_]] { def unit[A](value: A): F[A] }
        |object Exchanges {
        |  def status[F[_]](monad: Monad[F], response: HttpResponse[String]): F[Int] = monad.unit(response.statusCode())
        |  def headers[F[_]](monad: Monad[F], response: HttpResponse[String]): F[Int] = monad.unit(response.headers().map().size())
        |  def publisher[F[_]](monad: Monad[F]): F[HttpRequest.BodyPublisher] = monad.unit(HttpRequest.BodyPublishers.noBody())
        |  def send[F[_]](monad: Monad[F], client: HttpClient, request: HttpRequest): F[Int] =
        |    monad.unit(client.send(request, HttpResponse.BodyHandlers.ofString()).statusCode())
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "eager-effect"),
      List("send returns F but calls send when called, not when the F runs").sorted
    )
  }

  test("a type constructor bounded by a collection is a container rather than an effect") {
    val found = warnings(
      """import scala.collection.mutable.Builder
        |trait Monad[F[_]] { def unit[A](value: A): F[A] }
        |object Collecting {
        |  def build[A, T[X] <: Iterable[X]](builder: Builder[A, T[A]], value: A): T[A] = { builder += value; builder.result() }
        |  def gather[F[_]](monad: Monad[F], builder: Builder[Int, List[Int]]): F[List[Int]] = { builder += 1; monad.unit(builder.result()) }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "eager-effect"),
      List("gather returns F but calls += when called, not when the F runs").sorted
    )
  }

  test("by-value arguments of a call run when the call is made, while by-name ones run with the effect") {
    val found = warnings(
      """import java.io.InputStream
        |import java.net.http.HttpRequest
        |import scala.util.Try
        |trait MonadError[F[_]] {
        |  def fromTry[T](value: Try[T]): F[T]
        |  def defer[T](value: => T): F[T]
        |}
        |object Bodies {
        |  def read[F[_]](monad: MonadError[F], stream: InputStream): F[Array[Byte]] =
        |    monad.fromTry {
        |      val body = Try(stream.readAllBytes())
        |      stream.close()
        |      body
        |    }
        |  def tag[F[_]](monad: MonadError[F], builder: HttpRequest.Builder): F[Unit] = {
        |    builder.header("a", "b")
        |    monad.defer(())
        |  }
        |  def later[F[_]](monad: MonadError[F], stream: InputStream): F[Unit] = monad.defer(stream.close())
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "eager-effect"),
      List(
        "read returns F but calls close when called, not when the F runs",
        "tag returns F but calls header when called, not when the F runs"
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
        |  def late(): Unit = { val lagging = Thread(() => ()); lagging.start(); lagging.setDaemon(true) }
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
        "lagging is started but never joined or interrupted",
        "the thread started here is never joined or interrupted"
      )
    )
    assertEquals(
      tagged(found, "reimplemented-join"),
      List(
        "get retried through interrupts, restoring the interrupt flag afterwards, reimplements CompletableFuture.join"
      )
    )
  }

  test("a polymorphic function applied in a future on the global pool is no blocking call") {
    val found = warnings(
      """import scala.concurrent.Future
        |import scala.concurrent.ExecutionContext.Implicits.global
        |object Pooled {
        |  def lifted(lift: [A] => A => A, input: Int): Future[Int] = Future(lift(input))
        |}""".stripMargin
    )
    assertEquals(tagged(found, "blocking-common-pool"), Nil)
  }

  test("a one-shot memo cell is not an unbounded wait, but a cell it never runs still is") {
    val found = warnings(
      """import java.util.concurrent.ConcurrentHashMap
        |import java.util.concurrent.ExecutionException
        |import java.util.concurrent.FutureTask
        |object Memo {
        |  private val entries = ConcurrentHashMap[String, FutureTask[Int]]()
        |  def value(key: String, compute: () => Int): Int = {
        |    val once = Option(entries.get(key)).getOrElse {
        |      val fresh = FutureTask[Int](() => compute())
        |      Option(entries.putIfAbsent(key, fresh)).getOrElse { fresh.run(); fresh }
        |    }
        |    try once.get() catch { case error: ExecutionException => entries.remove(key, once); throw error.getCause }
        |  }
        |  def stored(key: String): Int = {
        |    val held = Option(entries.get(key)).getOrElse(throw new IllegalStateException(key))
        |    held.get()
        |  }
        |}""".stripMargin
    )
    assertEquals(tagged(found, "unbounded-wait"), List("get waits with no limit, so a stuck task hangs its caller"))
  }

  test("waits on futures invokeAll completed, joins in a close that interrupts, and threads an executor starts") {
    val found = warnings(
      """import java.util.concurrent.Callable
        |import java.util.concurrent.ExecutorService
        |import scala.jdk.CollectionConverters.*
        |object Pools {
        |  def all(pool: ExecutorService, tasks: java.util.List[Callable[Int]]): Int =
        |    pool.invokeAll(tasks).asScala.toVector.map(_.get).sum
        |  def held(pool: ExecutorService, tasks: java.util.List[Callable[Int]]): Int = {
        |    val futures = pool.invokeAll(tasks)
        |    futures.asScala.map(future => future.get()).sum
        |  }
        |  def submitted(pool: ExecutorService, task: Callable[Int]): Int = pool.submit(task).get()
        |}
        |class Workers(threads: Vector[Thread]) extends AutoCloseable {
        |  def close(): Unit = { threads.foreach(_.interrupt()); threads.foreach(_.join()) }
        |}
        |class Waiting(threads: Vector[Thread]) extends AutoCloseable {
        |  def close(): Unit = threads.foreach(_.join())
        |}
        |class Daemons extends java.util.concurrent.Executor {
        |  def execute(task: Runnable): Unit = { val thread = Thread(task); thread.setDaemon(true); thread.start() }
        |}
        |object Contexts extends scala.concurrent.ExecutionContext {
        |  def execute(task: Runnable): Unit = Thread(task).start()
        |  def reportFailure(cause: Throwable): Unit = ()
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "unbounded-wait"),
      List(
        "get waits with no limit, so a stuck task hangs its caller",
        "join waits with no limit, so a stuck task hangs its caller"
      )
    )
    assertEquals(tagged(found, "orphan-thread"), Nil)
  }

  test("a losing once-guard waits for work its winner counts down, and a close stops its join only after closing") {
    val found = warnings(
      """import java.nio.channels.ServerSocketChannel
        |import java.util.concurrent.CountDownLatch
        |import java.util.concurrent.atomic.AtomicBoolean
        |object Latches {
        |  private val done  = AtomicBoolean(false)
        |  private val latch = CountDownLatch(1)
        |  def release(work: () => Unit): Unit =
        |    if (done.compareAndSet(false, true)) try work() finally latch.countDown()
        |    else latch.await()
        |  def start(gate: CountDownLatch): Unit = gate.await()
        |}
        |object Gates {
        |  private val done  = AtomicBoolean(false)
        |  private val latch = CountDownLatch(1)
        |  def release(work: () => Unit): Unit =
        |    if (done.compareAndSet(false, true)) work()
        |    else latch.await()
        |}
        |class Listener(listener: ServerSocketChannel, acceptThread: Thread) extends AutoCloseable {
        |  def close(): Unit = { listener.close(); acceptThread.join() }
        |}
        |class Late(listener: ServerSocketChannel, acceptThread: Thread) extends AutoCloseable {
        |  def close(): Unit = { acceptThread.join(); listener.close() }
        |}
        |class Logged(log: AutoCloseable, worker: Thread) extends AutoCloseable {
        |  def close(): Unit = { log.close(); worker.join() }
        |}
        |class Unclosed(worker: Thread) extends AutoCloseable {
        |  def close(): Unit = worker.join()
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "unbounded-wait"),
      List(
        "await waits with no limit, so a stuck task hangs its caller",
        "await waits with no limit, so a stuck task hangs its caller",
        "join waits with no limit, so a stuck task hangs its caller",
        "join waits with no limit, so a stuck task hangs its caller",
        "join waits with no limit, so a stuck task hangs its caller"
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
        |  def registered(): Int = scala.util.Using.Manager { use =>
        |    val resource = use(Resource())
        |    val result   = resource.use()
        |    resource.close()
        |    result
        |  }.get
        |  def twice(check: (=> Unit) => Unit): Int = {
        |    val resource = Resource()
        |    check(resource.close())
        |    val result = resource.use()
        |    resource.close()
        |    result
        |  }
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

  test("private fields only written or checked are reported, and reads in describing methods count") {
    val found = warnings(
      """class Session(code: String) {
        |  private var attempts = 0
        |  private val state    = code.reverse
        |  private val label    = code.toUpperCase
        |  private var seen     = 0
        |  private val limit    = 3
        |  private val base     = 10
        |  private val derived  = base * 2
        |  private val digest   = code.length
        |  require(state.nonEmpty)
        |  def attempt(): Unit = { attempts += 1; seen = seen + 1 }
        |  def remaining: Int = limit - seen + derived
        |  override def toString: String = s"Session($label)"
        |  override def hashCode: Int = digest
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
      "eager-effect"           -> "def effect[F[_]](unit: Int => F[Int]): F[Int] = { println(1); unit(1) }",
      "implied-condition"      -> "def pick(a: Boolean): Int = if (a) { if (a) 1 else 2 } else 3",
      "regex-in-def"           -> "def parse(s: String): Boolean = \"a+\".r.matches(s)",
      "unspecialised-function" -> "def visit(f: (Long, Long, Int) => Unit): Unit = f(1L, 2L, 3)",
      "wrapped-conversion"     -> "def wrapped(value: java.math.BigDecimal): Double = BigDecimal(value).toDouble",
      "lambda-trait"           ->
        "private trait Sink { def accept(value: Long): Unit }\n  def sink(value: Long): Unit = { val to: Sink = received => println(received); to.accept(value) }",
      "function-applied-once" ->
        "def total(a: Long, b: Long): Long = { def pick(x: Long, y: Long, choose: (Long, Long) => Long): Long = choose(x, y); pick(a, b, math.min) + pick(b, a, math.max) }",
      "forwarding-alias" ->
        "private def alias(a: Int, b: Int): Int = target(a, b)\n  private def target(a: Int, b: Int): Int = a + b\n  def total: Int = alias(1, 2)",
      "unused-private-param" ->
        "private def run(a: Int, @scala.annotation.unused b: Int): Int = a\n  def user: Int = run(1, 2)",
      "parameter-object-unpacked" ->
        """private case class Settings(a: Int, b: Int, c: Int, d: Int)
          |  def draw(): Int = render(Settings(1, 2, 3, 4))
          |  private def render(settings: Settings): Int = {
          |    val Settings(a, b, c, d) = settings
          |    a + b + c + d
          |  }""".stripMargin,
      "accumulating-concat"       -> "def text(words: Vector[String]): String = words.foldLeft(\"\")(_ + _)",
      "tuple-destructure-in-loop" ->
        "def pairs(values: Vector[Int], flag: Boolean): Vector[Int] = values.map { v => val (a, b) = if (flag) (v, 1) else (1, v); a + b }",
      "invariant-in-loop" ->
        "def ids(values: Vector[Int], operands: Vector[String]): Int = values.map(_ + operands.map(_.length).size).sum",
      "inline-set-membership" -> "def parts(parts: Vector[String]): Int = parts.count(part => Set(\"x\", \"y\")(part))",
      "literal-table-in-def"  -> "def shapes: Vector[(Int, Int)] = Vector((2, 2), (3, 3))",
      "inline-function-parameter" -> "inline def run(f: Int => Int): Int = f(1)",
      "inline-parameter-reuse"    ->
        "def use(values: List[Int]): Int = twice(values.size)\n  inline def twice(inline value: Int): Int = value + value",
      "inline-instance-parameter" ->
        "inline def twice(algebra: Algebra): Long = algebra.add(1L)\n  trait Algebra { inline def add(left: Long): Long }",
      "keyed-lookup-in-loop" ->
        """object Db {
          |    case class Form(name: String)
          |    val forms: Vector[Form]       = Vector()
          |    val byName: Map[String, Form] = Map()
          |    def names(keys: Vector[String]): Vector[Option[Form]] = keys.map(key => forms.find(_.name == key))
          |  }""".stripMargin,
      "identical-branches" ->
        "def pick(a: Boolean, b: Boolean, text: String): String = if (a) text.trim else if (b) text.trim else text",
      "repeated-branch-call" ->
        """def write(kind: Int, n: Int): Int = kind match {
          |    case 0 => emit(n, 1, 0)
          |    case 1 => emit(n, 1, n)
          |    case _ => emit(n, 1, n + 1)
          |  }
          |  def emit(a: Int, b: Int, c: Int): Int = a + b + c""".stripMargin,
      "binding-in-one-branch" ->
        "def total(flag: Boolean, xs: List[Int]): Int = { val sum = xs.map(_ * 2).sum; if (flag) sum + 1 else 0 }",
      "hand-written-lazy" ->
        """final class Node(key: String) {
          |    private var element: String = null
          |    def symbol: String = { if (element == null) element = key.trim; element }
          |  }""".stripMargin,
      "string-round-trip" ->
        "def trip(a: String, b: String): Option[String] = s\"$a:$b\" match { case s\"$x:$_\" => Some(x); case _ => None }"
    )
    val heated = Set("regex-in-def", "tuple-destructure-in-loop")
    bodies.filter((id, _) => id != "unused-private-param" || Compilations.minor >= 7).foreach { (id, body) =>
      val hot   = Option.when(heated(id))("hot=Lints").toList
      val plain = warnings(s"object Lints {\n  $body\n}", hot*)
      assert(plain.exists(_.contains(s"bonsai:$id")), s"$id\n${plain.mkString("\n")}")
      List(s"@scala.annotation.nowarn(\"msg=bonsai:$id\")", s"@SuppressWarnings(Array(\"bonsai:$id\"))")
        .foreach { annotation =>
          val found = warnings(s"object Lints {\n  $annotation\n  $body\n}", hot*)
          assert(!found.exists(_.contains(s"bonsai:$id")), s"$annotation\n${found.mkString("\n")}")
        }
    }
    val escalated = "object Lints {\n  @SuppressWarnings(Array(\"bonsai\"))\n  " + bodies("unbounded-wait") + "\n}"
    assertEquals(compile("Lints.scala", escalated, "error=unbounded-wait")._1, Nil)
    val token =
      "class Session { private val binding: AnyRef = new Object(); def same(other: AnyRef): Boolean = other eq binding }"
    assert(warnings(token).exists(_.contains("bonsai:identity-token")))
    assertEquals(
      warnings("@SuppressWarnings(Array(\"bonsai:identity-token\"))\n" + token).filter(_.contains("identity")),
      Nil
    )
  }

  test("the copies of the sources mutation testing compiles from its temporary directory are not checked") {
    val source = "object Lints {\n  def same(a: Array[Int], b: Array[Int]): Boolean = a == b\n}"
    assertEquals(tagged(warningsAt("src/main/scala/Lints.scala", source), "array-equality").size, 1)
    assertEquals(tagged(warningsAt("stryker4s-8113/src/main/scala/Lints.scala", source), "array-equality"), Nil)
  }
}
