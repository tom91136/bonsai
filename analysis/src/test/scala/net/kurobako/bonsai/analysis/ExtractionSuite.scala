package net.kurobako.bonsai.analysis

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarEntry
import java.util.jar.JarOutputStream
import java.util.jar.Manifest as JarManifest
import net.kurobako.bonsai.java.*
import net.kurobako.bonsai.testing.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

class ExtractionSuite extends AnalysisSuite {

  // Corrupt bytes near the end fail only the unpickling of that class, not the reader's setup.
  private def readBroken(sources: Seq[(String, String)], broken: String): Either[Vector[String], Vector[String]] =
    Scratch.using("bonsai-") { directory =>
      val classes = Files.createDirectories(directory.resolve("classes"))
      sources.foreach((name, source) => Files.writeString(directory.resolve(name), source): Unit)
      val paths = sources.map((name, _) => directory.resolve(name).toString)
      val _     = Driver().process(
        Array("-classpath", classpath.mkString(File.pathSeparator), "-d", classes.toString) ++ paths,
        StoreReporter()
      )
      val tasty = classes.resolve(s"$broken.tasty")
      val bytes = Files.readAllBytes(tasty)
      Files.write(tasty, bytes.patch(bytes.length - 24, Array.fill(12)(Byte.MaxValue), 12)): Unit
      def brief(lines: Vector[String]) = lines.map(_.takeWhile(_ != ':'))
      Extraction.read(Manifest(
        Vector(Output("m", Output.Scope.Main, classes)),
        classpaths,
        Set.empty,
        jdk = targetJdk
      )).fold(
        problems => Left(brief(problems)),
        result => Right(result.graph.nodes.keySet.toVector.sorted ++ brief(result.problems))
      )
    }

  test("a call chain deeper than the default recursion fuel is read") {
    val chain  = (1 to 600).map(index => s".append(\"$index\")").mkString
    val result =
      extract(program("Deep", s"object Deep { val text: String = new java.lang.StringBuilder()$chain.toString }"))
    assert(result.graph.nodes.contains("Deep"), result.graph.nodes.keySet.mkString(", "))
  }

  private def archive(classes: Path, jar: Path, title: String, version: String): Path = {
    val manifest = JarManifest()
    Seq(
      "Manifest-Version"         -> "1.0",
      "Implementation-Vendor-Id" -> "org.example",
      "Implementation-Title"     -> title,
      "Implementation-Version"   -> version
    ).foreach(manifest.getMainAttributes.putValue(_, _))
    Using.resource(JarOutputStream(Files.newOutputStream(jar), manifest)) { out =>
      Using.resource(Files.walk(classes))(_.toList.asScala.filter(Files.isRegularFile(_)).foreach { entry =>
        out.putNextEntry(JarEntry(classes.relativize(entry).toString))
        out.write(Files.readAllBytes(entry))
        out.closeEntry()
      })
    }
    jar
  }

  test("a build whose only class cannot be read fails, while readable classes keep the rest of the build") {
    assertEquals(readBroken(Seq("A.scala" -> "class A"), "A"), Left(Vector("class file A could not be read")))
    assertEquals(
      readBroken(Seq("A.scala" -> "class A", "B.scala" -> "class B"), "A"),
      Right(Vector("B", "class file A could not be read"))
    )
  }

  test("a manifest without a jdk is refused, with the way to regenerate it") {
    Scratch.using("bonsai-") { directory =>
      val classes = Files.createDirectories(directory.resolve("classes"))
      val read    = Extraction.read(Manifest(Vector(Output("m", Output.Scope.Main, classes)), classpaths, Set.empty))
      assert(
        read.left.exists(_.exists(error => error.contains("no jdk") && error.contains("regenerate"))),
        read.toString
      )
    }
  }

  test("sources without classes fail rather than read as a clean build") {
    Scratch.using("bonsai-") { directory =>
      val classes                       = Files.createDirectories(directory.resolve("classes"))
      val source                        = Files.writeString(directory.resolve("A.scala"), "class A")
      def read(sources: Vector[Output]) = Extraction.read(
        Manifest(
          Vector(Output("m", Output.Scope.Main, classes)),
          classpaths,
          Set.empty,
          sources = sources,
          jdk = targetJdk
        )
      )
      assert(read(Vector(Output("m", Output.Scope.Main, source))).left.exists(_.exists(_.contains("no classes"))))
      assert(read(Vector.empty).isRight)
      assert(read(Vector(Output("m", Output.Scope.Test, source))).isRight)
      val full = Files.createDirectories(directory.resolve("full"))
      Files.writeString(full.resolve("A.class"), ""): Unit
      val outputs = Vector(Output("m", Output.Scope.Main, classes), Output("m", Output.Scope.Main, full))
      assert(Extraction.read(
        Manifest(
          outputs,
          classpaths,
          Set.empty,
          sources = Vector(Output("m", Output.Scope.Main, source)),
          jdk = targetJdk
        )
      ).left.forall(!_.exists(_.contains("no classes"))))
    }
  }

  test("modules whose classpaths hold different jars of one artifact each resolve their own") {
    Scratch.using("bonsai-") { directory =>
      def library(platform: String, name: String): Path = {
        val classes = compile(
          directory.resolve(platform),
          Seq(s"$name.scala" -> s"package lib\nobject $name { def apply(): Int = 1 }"),
          Vector.empty
        )
        archive(classes, directory.resolve(platform).resolve(s"lib_${platform}_3-1.0.jar"), "lib", "1.0")
      }
      val libraries = Map("jvm" -> library("jvm", "Jvm"), "native" -> library("native", "Native"))
      val outputs   = libraries.toVector.map { (module, jar) =>
        val name    = module.capitalize
        val sources = Seq("Use.scala" -> s"object Use$name { val value = lib.$name() }")
        Output(module, Output.Scope.Main, compile(directory.resolve(s"$module-use"), sources, Vector(jar)))
      }
      val manifest = Manifest(outputs, libraries.view.mapValues(_ +: classpath).toMap, Set.empty, jdk = targetJdk)
      val result   = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      assertEquals(result.graph.nodes.keySet.count(node => node.contains("UseJvm") || node.contains("UseNative")), 2)
    }
  }

  test("the merged classpath keeps the newest version of each artifact, where its first copy was") {
    Scratch.using("bonsai-") { directory =>
      def api(version: String, members: String): Path = {
        val classes = compile(
          directory.resolve(s"api-$version"),
          Seq("Api.scala" -> s"package lib\nobject Api { $members }"),
          Vector.empty
        )
        archive(classes, directory.resolve(s"api-$version.jar"), "api", version)
      }
      val older = api("1.9.0", "def apply(): Int = 1")
      val newer = api("1.10.0", "def apply(): Int = 1; def extra(): Int = 2")
      val first = compile(
        directory.resolve("first-use"),
        Seq("First.scala" -> "object UseFirst { val value = lib.Api() }"),
        Vector(older)
      )
      val second = compile(
        directory.resolve("second-use"),
        Seq("Second.scala" -> "object UseSecond { val value = lib.Api.extra() }"),
        Vector(newer)
      )
      val outputs  = Vector(Output("first", Output.Scope.Main, first), Output("second", Output.Scope.Main, second))
      val manifest =
        Manifest(
          outputs,
          Map("first" -> (older +: classpath), "second" -> (newer +: classpath)),
          Set.empty,
          jdk = targetJdk
        )
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      assertEquals(result.graph.nodes.keySet.count(_.contains("UseSecond")), 1)
    }
  }

  test("artifacts whose manifests share a vendor and title stay apart on the merged classpath") {
    Scratch.using("bonsai-") { directory =>
      def core(name: String, version: String, member: String): Path = {
        val classes = compile(
          directory.resolve(name),
          Seq(s"$member.scala" -> s"package lib\nobject $member { def apply(): Int = 1 }"),
          Vector.empty
        )
        archive(classes, directory.resolve(s"$name-core_3-$version.jar"), "core", version)
      }
      val config  = core("config", "0.18.7", "Config")
      val format  = core("format", "3.11.5", "Format")
      val outputs = Vector("Config" -> config, "Format" -> format).map { (member, jar) =>
        val sources = Seq("Use.scala" -> s"object Use$member { val value = lib.$member() }")
        Output(member, Output.Scope.Main, compile(directory.resolve(s"$member-use"), sources, Vector(jar)))
      }
      val manifest =
        Manifest(
          outputs,
          Map("Config" -> (config +: classpath), "Format" -> (format +: classpath)),
          Set.empty,
          jdk = targetJdk
        )
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      assertEquals(result.graph.nodes.keySet.count(node => node.contains("UseConfig") || node.contains("UseFormat")), 2)
    }
  }

  test("a library body that refers to a package the classpath lacks is one problem, not a failed extraction") {
    Scratch.using("bonsai-") { directory =>
      val missing = compile(
        directory.resolve("missing"),
        Seq(
          "Result.scala" ->
            """package missing
            |sealed abstract class Result[+A] { def toEither: Either[String, A] }
            |object Result {
            |  final case class Ok[+A](value: A) extends Result[A] { def toEither: Either[String, A] = Right(value) }
            |  final case class No(error: String) extends Result[Nothing] { def toEither: Either[String, Nothing] = Left(error) }
            |  def error(message: String): Result[Nothing] = No(message)
            |}
            |trait Decoder[A] { def read(default: Option[A], text: String): Result[A] }""".stripMargin
        ),
        Vector.empty
      )
      val helper = archive(
        compile(
          directory.resolve("helper"),
          Seq(
            "Helper.scala" ->
              """package helper
                |import missing.*
                |object Helper {
                |  val decoder: Decoder[Int] = (default, text) => if (text.isEmpty) Result.No("empty") else Result.Ok(text.length)
                |  def parse(text: String, default: Int): Result[Int] = decoder.read(Option(default), text) match {
                |    case Result.Ok(value) if value > default => Result.error(s"too long: $value")
                |    case other                               => other
                |  }
                |}""".stripMargin
          ),
          Vector(missing)
        ),
        directory.resolve("helper-1.0.jar"),
        "helper",
        "1.0"
      )
      val sources = Seq(
        "Use.scala" ->
          """object Use {
          |  private lazy val parsed = helper.Helper.parse("x", 1).toEither.fold(error => throw IllegalStateException(error), identity)
          |  def apply(value: Int): Int = value + parsed
          |}""".stripMargin
      )
      val output   = Output("m", Output.Scope.Main, compile(directory.resolve("use"), sources, Vector(helper, missing)))
      val manifest = Manifest(Vector(output), Map("m" -> (helper +: classpath)), Set.empty, jdk = targetJdk)
      val result   = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assert(result.graph.nodes.keySet.exists(_.contains("Use")), result.graph.nodes.keySet.toString)
      assertEquals(
        result.problems.filter(_.startsWith("could not read library bodies")),
        Vector(s"could not read library bodies of helper.Helper in $helper")
      )
    }
  }

  test("eviction keeps the newest version of an artifact where its first copy was") {
    val older  = Path.of("/cache/org/example/tool/1.9.0/tool-1.9.0.jar")
    val other  = Path.of("/cache/org/example/other/2.0.0/other-2.0.0.jar")
    val newest = Path.of("/cache/org/example/tool/1.10.0/tool-1.10.0.jar")
    val output = Path.of("/work/classes")
    assertEquals(Classpath.evicted(Vector(older, other, newest, output)), Vector(newest, other, output))
  }

  test("eviction identifies a jar outside a repository layout by its manifest and unversioned name") {
    Scratch.using("bonsai-") { directory =>
      def jar(path: String, version: String) = {
        val file = directory.resolve(path)
        Files.createDirectories(file.getParent)
        archive(Files.createDirectories(directory.resolve(s"empty-$version")), file, "scala-library", version)
      }
      val boot  = jar("boot/scala-3.8.4/lib/scala-library.jar", "3.8.4")
      val rules = jar("cache/org/scala-lang/scala-library/3.9.0/scala-library-3.9.0.jar", "3.9.0")
      val rest  = jar("cache/org/scala-lang/scala-library/3.10.0/scala-library-3.10.0.jar", "3.10.0")
      for order <- Seq(Vector(boot, rules, rest), Vector(rest, boot, rules), Vector(rules, rest, boot))
      do assertEquals(Classpath.evicted(order), Vector(rest))
    }
  }

  test("megamorphic sites list implementations by simple name unless two collide, count vals, and skip universals") {
    val result = extract(
      Seq(
        "Shapes.scala" ->
          """package shapes
          |trait Figure { def area: Int; def sides: Int }
          |class Circle extends Figure { def area = 1; val sides = 0; override def toString = "circle" }
          |class Square extends Figure { def area = 2; def sides = 4; override def toString = "square" }
          |class Triangle extends Figure { def area = 3; val sides = 3 }
          |class Plot { def area: Int = 4 }
          |trait Named { def name: String }
          |object A { class Impl extends Named { def name = "a" } }
          |object B { class Impl extends Named { def name = "b" } }
          |class Other extends Named { def name = "c" }
          |object Limits { val most = 1 }
          |object Main {
          |  def limits(shapes: List[Figure]): Int = shapes.map(_ => Limits.most).sum
          |  def areas(shapes: List[Figure]): Int = shapes.map(_.area).sum
          |  def sides(shapes: List[Figure]): Int = shapes.map(_.sides).sum
          |  def shown(shapes: List[Figure]): Int = shapes.map(_.toString.length).sum
          |  def names(values: List[Named]): Int = values.map(_.name.length).sum
          |}""".stripMargin
      ),
      hot = Vector("**")
    )
    assertEquals(
      result.megamorphic.map(site => (site.caller, site.callee, site.implementations.sorted)).toSet,
      Set(
        ("shapes.Main.areas", "Figure.area", Vector("Circle", "Square", "Triangle")),
        ("shapes.Main.sides", "Figure.sides", Vector("Circle", "Square", "Triangle")),
        ("shapes.Main.names", "Named.name", Vector("A.Impl", "B.Impl", "shapes.Other"))
      )
    )
  }

  test("local methods are not reported on their own") {
    val result = extract(Seq(
      "Search.scala" ->
        """object Search {
          |  def pairs(values: Vector[Int]): Int = values.map(value => values.count(_ == value)).sum
          |  def outer(values: Vector[Int]): Int = {
          |    def inner(items: Vector[Int]): Int = items.map(item => items.count(_ == item)).sum
          |    inner(values)
          |  }
          |}""".stripMargin
    ))
    assertEquals(result.complexities.map(_.name), Vector("Search.pairs"))
  }

  private lazy val library = extract(
    Seq(
      "Api.scala" ->
        """package lib
          |case class Token private (value: String)
          |case class Scoped private[lib] (value: String)
          |case class Blank()
          |object Blank { val shared: Option[Blank] = Some(Blank()) }
          |case class Money(amount: Int)
          |object Money {
          |  def of(amount: Int): Money = new Money(amount)
          |  def parse(amount: Int): Option[Int] = Some(amount)
          |  def safe(amount: Int): Option[Money] = Option.when(amount >= 0)(Money(amount))
          |}""".stripMargin
    ),
    libraries = Set("m")
  )

  private def products[A](names: Set[String])(of: Algebra.Product => A): Map[String, A] =
    library.algebra.products.view.collect { case product if names(product.name) => product.name -> of(product) }
      .toMap

  test("private constructors make construction private and keep a library's case class out of its API") {
    assertEquals(
      products(Set("lib.Token", "lib.Scoped"))(product => (product.access, product.origin == Algebra.Origin.Exported)),
      Map("lib.Token" -> (Algebra.Access.Private, false), "lib.Scoped" -> (Algebra.Access.Private, false))
    )
  }

  test("only companion methods returning the class in an effect from its fields are factories") {
    assertEquals(
      products(Set("lib.Blank", "lib.Money"))(_.access),
      Map("lib.Blank" -> Algebra.Access.Open, "lib.Money" -> Algebra.Access.Factory(Vector("Money.safe")))
    )
  }

  private lazy val hierarchies = extract(
    Seq(
      "Hierarchies.scala" ->
        """sealed trait Shape
        |final class Plain(val size: Int) extends Shape
        |case class Round(radius: Int) extends Shape
        |case object Dot extends Shape
        |enum Tone { case Soft(level: Int); case Loud(level: Int) }
        |sealed trait Signal
        |object Signal { case object Stop extends Signal; case class Go(speed: Int) extends Signal { import scala.math.max } }
        |sealed trait Mixed
        |object Mixed { case object Inside extends Mixed }
        |case object Outside extends Mixed
        |sealed trait Spread
        |object Spread { case object Near extends Spread; case object Far extends Spread }""".stripMargin,
      "src/generated/Generated.scala" ->
        "sealed trait Generated; case object First extends Generated; case object Second extends Generated"
    ),
    generated = GeneratedSources
  )

  test("only case class alternatives of a sum carry fields") {
    assertEquals(
      hierarchies.algebra.sums.find(_.name == "Shape").map(_.variants.map(variant =>
        variant.name -> variant.fields.map(_.name)
      ).toMap),
      Some(Map("Plain" -> Vector.empty, "Round" -> Vector("radius"), "Dot" -> Vector.empty))
    )
  }

  test("enum candidates ignore imports in case bodies, exclude enums, and nest all-companion cases") {
    assertEquals(
      hierarchies.algebra.enumerations.map(enumeration => enumeration.name -> enumeration.nested).toSet,
      Set("Signal" -> true, "Mixed" -> false, "Spread" -> true, "Generated" -> false)
    )
  }

  test("instantiations count main and test uses, mark type parameters as varied and ignore Nothing") {
    val result = extract(
      Seq(
        "Generics.scala" ->
          """case class Box[A](value: A)
            |case class Bag[A](value: A)
            |object Picker {
            |  def pick()[A](value: A): A = value
            |  def first[A](values: List[A]): Option[A] = values.headOption
            |  extension [A](value: A) def triple[B, C](second: B, third: C): (A, B, C) = (value, second, third)
            |}
            |import Picker.triple
            |object Main {
            |  val boxes = List(Box(1), Box(2))
            |  def bag[B](value: B): Bag[List[B]] = Bag(List(value))
            |  val bags = List(Bag(1))
            |  val picks = List(Picker.pick()[Int](1), Picker.first(Nil), Picker.first(List("a")))
            |  val triples = 1.triple("a", 2L)
            |}
            |val (box, count) = identity((Box(3), 1))""".stripMargin
      ),
      Seq("BoxCheck.scala" -> "class BoxCheck { val box = Box(4) }")
    )
    assertEquals(
      result.smells.instantiations.map(instantiation =>
        (instantiation.owner, instantiation.arguments, instantiation.varied, instantiation.testUses)
      ).toSet,
      Set(
        ("Box", Map("Int" -> 4), false, 1),
        ("Bag", Map("Int" -> 1), true, 0),
        ("Picker.pick", Map("Int" -> 1), false, 0),
        ("Picker.first", Map("String" -> 1), false, 0),
        ("Picker.triple", Map("Int" -> 1), false, 0),
        ("Picker.triple", Map("String" -> 1), false, 0),
        ("Picker.triple", Map("Long" -> 1), false, 0)
      )
    )
  }

  test("results are tracked for calls in main and test code, but not for Unit methods") {
    val result = extract(
      Seq(
        "Tools.scala" ->
          """object Tools {
            |  def compute(): Int = 1
            |  def log(): Unit = ()
              |}
            |object Main {
            |  def use(): Int = Tools.compute() + Tools.compute()
            |  def say(): Unit = { Tools.log(); Tools.log() }
            |}""".stripMargin
      ),
      Seq("ToolsCheck.scala" -> "class ToolsCheck { val value = Tools.compute() }")
    )
    assertEquals(
      result.smells.usages.map(usage =>
        (usage.method.name, usage.result, usage.calls, usage.discarded, usage.testCalls)
      ),
      Vector(("Tools.compute()", "Int", 3, 0, 1))
    )
  }

  test("an effect of Unit is no result to use, while an effect of a value is") {
    val result = extract(
      Seq(
        "Effects.scala" ->
          """trait Unit0[F[_]] {
            |  def write(value: Int): F[Unit]
            |  def read(): F[Int]
            |}
            |object Main {
            |  def run[F[_]](unit: Unit0[F]): F[Unit] = { unit.write(1); unit.write(2); unit.read(); unit.read(); unit.write(3) }
            |  def twice[F[_]](unit: Unit0[F]): F[Unit] = run(unit)
            |}""".stripMargin
      ),
      Seq.empty
    )
    assertEquals(result.smells.usages.map(_.result).toSet, Set("F[Int]"))
  }

  test("explicit default arguments are matched to the getters of the called overload, constructor or method") {
    val result = extract(Seq(
      "Defaults.scala" ->
        """object Net {
          |  def send(port: Int = 80): Int = port
          |  def send(port: Int, retries: Int): Int = port + retries
          |}
          |class Client { def connect(host: String, port: Int = 80): String = host }
          |case class Preferences(name: String = "x", retries: Int = 3)
          |object Preferences { def make(label: String, count: Int = 5): Preferences = Preferences(label, count) }
          |object Codec { def group(name: String, minimum: Option[String] = Some("1.0")): String = name }
          |object Main {
          |  val sent = Net.send(80, 1)
          |  val connected = Client().connect("a", 80)
          |  val built = new Preferences("x", 3)
          |  val made = Preferences.make("x", 5)
          |  val grouped = Codec.group(minimum = Some("1.0"), name = "b".trim)
          |}
          |val (connected, count) = identity((Client().connect("b", 80), 1))""".stripMargin
    ))
    assertEquals(
      result.smells.defaults.map(default =>
        (default.owner, default.parameter, default.value, default.sites.map(_.test))
      ).toSet,
      Set(
        ("Client.connect(host, port)", "port", "80", Vector(false, false)),
        ("Preferences", "retries", "3", Vector(false)),
        ("Preferences.make(label, count)", "count", "5", Vector(false)),
        ("Codec.group(name, minimum)", "minimum", "Some(\"1.0\")", Vector(false))
      )
    )
  }

  test("members record inheritance and visibility, leaving out local methods and abstract types") {
    val result = extract(Seq(
      "Members.scala" ->
        """trait Figure { def area: Int; def perimeter: Int; type Item }
          |class Square extends Figure { def area = 1; def perimeter = 4; type Item = Int; protected def hook: Int = 2 }
          |object Tools { def run(): Int = { def helper: Int = 1; helper } }
          |object Main { def main(args: Array[String]): Unit = { val shape: Figure = Square(); println(shape.area + Tools.run()) } }""".stripMargin
    ))
    val members = result.members.members.map(member => member.name -> (member.inherited, member.public)).toMap
    assertEquals(
      members.filter((name, _) => name.startsWith("Square.")),
      Map(
        "Square.area"      -> (true, true),
        "Square.perimeter" -> (false, true),
        "Square.Item"      -> (false, true),
        "Square.hook"      -> (false, false)
      )
    )
    assert(!members.contains("Figure.Item") && !members.keys.exists(_.contains("helper")), members.keys.mkString("\n"))
  }

  test("visible nested types are exposed by visible signatures and parents, not by private members") {
    val result = extract(Seq(
      "Outer.scala" ->
        """object Outer {
          |  class Inner
          |  object Visible
          |  class Base
          |  class Impl extends Base
          |  private class Hidden { def value: Int = 1 }
          |  private def make: Inner = Inner()
          |  def size: Int = make.hashCode + Hidden().value
          |}""".stripMargin
    ))
    assertEquals(
      result.members.nested.map(nested => nested.node -> nested.exposed).toMap,
      Map("Outer.Inner" -> false, "Outer.Visible" -> false, "Outer.Base" -> true, "Outer.Impl" -> false)
    )
  }

  test("companions and enum cases share one node, and file imports link each top-level definition") {
    val graph = extract(Seq(
      "Nodes.scala" ->
        """import Helpers.*
          |object Pair { def of(value: Int): Pair = Pair(value) }
          |case class Pair(value: Int)
          |class Plain
          |@SuppressWarnings(Array("bonsai:unused")) object Plain
          |enum Tone { case Soft(level: Int); case Loud }
          |object Helpers { val unit = 1 }
          |val limit = 2""".stripMargin
    )).graph
    assertEquals(graph.nodes.keySet, Set("Pair", "Plain", "Tone", "Helpers", "limit"))
    assertEquals(graph.nodes("Pair").line, 3)
    assertEquals(graph.nodes("Plain").suppressed, LintId.resolve("bonsai:unused"))
    assertEquals(graph.edges.get("limit"), Some(Set("Helpers")))
  }

  test("construction sites are kept for classifiable fields and resolve only synthetic bindings") {
    val result = extract(Seq(
      "Sites.scala" ->
        """case class Name(value: String)
          |case class Money(amount: Int)
          |object Money { def apply(text: String): Money = new Money(text.toInt) }
          |object Render { def render(text: String, width: Int): String = text.take(width) }
          |object Validators {
          |  def trivial(size: Int): Int = { require(true); size }
          |  def positive(size: Int): Int = { require(size > 0); size }
          |}
          |object Main {
          |  val names = List(Name("a"), Name("b"))
          |  val money = List(Money(1), Money("2"))
          |  def draw(): List[String] = { val width = 80; List(Render.render("a", width), Render.render("b", width)) }
          |}""".stripMargin
    ))
    val sites = result.algebra.sites.view.mapValues(_.map(_.arguments).toSet).toMap
    assertEquals(sites.get("Name"), None)
    assertEquals(sites.get("Money"), Some(Set(Vector(Algebra.Argument.Number(1)), Vector(Algebra.Argument.Unknown))))
    assertEquals(
      sites.get("Render.render(Ljava/lang/String;I)Ljava/lang/String;"),
      Some(Set(
        Vector(Algebra.Argument.Word(Algebra.Wording.Text, "a"), Algebra.Argument.Unknown),
        Vector(Algebra.Argument.Word(Algebra.Wording.Text, "b"), Algebra.Argument.Unknown)
      ))
    )
    assertEquals(result.algebra.preconditions.values.map(_.name).toVector, Vector("Validators.positive(size)"))
  }

  test("type arguments of a later type clause are counted for that clause's parameters") {
    val result = extract(Seq(
      "Main.scala" ->
        "object Main { def main(args: Array[String]): Unit = println((1.tagged(\"a\"), 2.tagged(\"b\"))) }",
      "Ext.scala" -> "extension [A](value: A) def tagged[B](tag: B): (A, B) = (value, tag)"
    ))
    assertEquals(
      result.smells.instantiations.collect {
        case use if use.owner.endsWith("tagged") => use.parameter -> use.arguments
      }
        .sortBy(_._1),
      Vector("A" -> Map("Int" -> 2), "B" -> Map("String" -> 2))
    )
  }

  test("an output directory with no TASTy is an empty build rather than a failure") {
    Scratch.using("bonsai-") { directory =>
      val result = Extraction.read(Manifest(
        Vector(Output("m", Output.Scope.Main, directory)),
        classpaths,
        Set.empty,
        jdk = targetJdk
      ))
      assertEquals(result.map(_.graph.nodes.isEmpty), Right(true))
      assertEquals(result.map(_.problems), Right(Vector.empty[String]))
    }
  }

  test("an output holding a file that is not TASTy fails the read rather than throwing") {
    Scratch.using("bonsai-") { directory =>
      Files.writeString(directory.resolve("Junk.tasty"), "not tasty"): Unit
      assert(Extraction.read(Manifest(
        Vector(Output("m", Output.Scope.Main, directory)),
        classpaths,
        Set.empty,
        jdk = targetJdk
      )).isLeft)
    }
  }

  test("a failed extraction is one problem line") {
    Scratch.using("bonsai-") { directory =>
      val classes = compile(directory, Seq("Main.scala" -> "object Main"), Vector.empty)
      Files.write(
        classes.resolve("Junk.class"),
        Array[Byte](0xca.toByte, 0xfe.toByte, 0xba.toByte, 0xbe.toByte, 0, 0, 0, 61, 0, 2, 99)
      ): Unit
      val problems =
        Extraction.read(Manifest(
          Vector(Output("m", Output.Scope.Main, classes)),
          classpaths,
          Set.empty,
          jdk = targetJdk
        ))
          .left.getOrElse(Vector.empty)
      assertEquals(problems.map(_.linesIterator.size), Vector(1))
      assert(problems.head.startsWith("extraction failed: java.lang.IllegalArgumentException"), problems.toString)
    }
  }
}
