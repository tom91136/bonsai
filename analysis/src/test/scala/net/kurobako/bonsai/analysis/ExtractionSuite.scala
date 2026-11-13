package net.kurobako.bonsai.analysis

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.java.*
import net.kurobako.bonsai.testing.*

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

  private val scalaJs = Path.of(sys.props("bonsai.analysis.scalajs"))

  private def compiled(directory: Path, sources: Seq[(String, String)], options: Array[String]): Path = {
    val classes = Files.createDirectories(directory.resolve("classes"))
    write(directory, sources)
    val paths    = sources.map((name, _) => directory.resolve(name).toString)
    val reporter = Driver().process(options ++ Array("-d", classes.toString) ++ paths, StoreReporter())
    assert(!reporter.hasErrors, reporter.allErrors.map(_.message).mkString("\n"))
    classes
  }

  test("a call chain deeper than the default recursion fuel is read") {
    val chain  = (1 to 600).map(index => s".append(\"$index\")").mkString
    val result =
      extract(program("Deep", s"object Deep { val text: String = new java.lang.StringBuilder()$chain.toString }"))
    assert(result.graph.nodes.contains("Deep"), result.graph.nodes.keySet.mkString(", "))
  }

  test("a build whose only class cannot be read fails, while readable classes keep the rest of the build") {
    assertEquals(readBroken(Seq("A.scala" -> "class A"), "A"), Left(Vector("class file A could not be read")))
    assertEquals(
      readBroken(Seq("A.scala" -> "class A", "B.scala" -> "class B"), "A"),
      Right(Vector("B", "class file A could not be read"))
    )
  }

  test("a Scala.js library call with a js.| parameter reads without undefined notes") {
    Scratch.using("bonsai-") { directory =>
      val source =
        """import scala.scalajs.js
          |class Caller { def render(value: js.Any): String = js.JSON.stringify(value, null.asInstanceOf[js.Array[js.Any]], 2) }""".stripMargin
      val classes = compiled(
        directory,
        Seq("Caller.scala" -> source),
        Array("-scalajs", "-classpath", (scalaJs +: classpath).mkString(File.pathSeparator))
      )
      val result = Extraction.read(Manifest(
        Vector(Output("m", Output.Scope.Main, classes)),
        scoped(Map("m" -> (scalaJs +: classpath))),
        Set.empty,
        jdk = targetJdk
      )).fold(problems => fail(problems.mkString("\n")), identity)
      assert(!result.notes.exists(_.contains("undefined:")), result.notes.mkString("\n"))
    }
  }

  test("a class whose parent is missing fails alone, while the other classes keep their findings") {
    Scratch.using("bonsai-") { directory =>
      val classes = compiled(
        directory,
        Seq(
          "Parent.scala" -> "class Parent[A]",
          "Child.scala"  -> "class Child extends Parent[Int]",
          "Other.scala"  -> "class Other { def value: Int = 1 }"
        ),
        Array("-classpath", classpath.mkString(File.pathSeparator))
      )
      Files.delete(classes.resolve("Parent.tasty"))
      Files.delete(classes.resolve("Parent.class"))
      val result = Extraction.read(Manifest(
        Vector(Output("m", Output.Scope.Main, classes)),
        classpaths,
        Set.empty,
        jdk = targetJdk
      )).fold(errors => fail(errors.mkString("\n")), identity)
      assert(result.graph.nodes.contains("Other"), result.graph.nodes.keySet.toString)
      assertEquals(result.problems.size, 1)
      assert(result.problems.head.contains("Child.tasty could not be analysed"), result.problems.head)
    }
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

  test("a product the compile recorded that is missing from the disk fails rather than reads as a clean build") {
    Scratch.using("bonsai-") { directory =>
      val classes = Files.createDirectories(directory.resolve("classes"))
      val source  = Files.writeString(directory.resolve("A.scala"), "class A")
      val product = classes.resolve("A.class")
      def read(sources: Vector[Output], products: Vector[Output]) = Extraction.read(
        Manifest(
          Vector(Output("m", Output.Scope.Main, classes)),
          classpaths,
          Set.empty,
          sources = sources,
          products = products,
          jdk = targetJdk
        )
      )
      val sourced = Vector(Output("m", Output.Scope.Main, source))
      val missing = read(sourced, Vector(Output("m", Output.Scope.Main, product)))
      assert(missing.left.exists(_.exists(error =>
        error.startsWith("m has main classes") && error.contains("A.class")
      )))
      assert(read(sourced, Vector.empty).left.forall(!_.exists(_.contains("recorded"))))
      assert(read(Vector.empty, Vector.empty).left.forall(!_.exists(_.contains("recorded"))))
      Files.writeString(product, ""): Unit
      assert(read(
        sourced,
        Vector(Output("m", Output.Scope.Main, product))
      ).left.forall(!_.exists(_.contains("recorded"))))
    }
  }

  test("a product the build deletes after its compile fails the read only where the classes left refer to it") {
    Seq("class Kept { def size: Int = 1 }" -> true, "class Kept { def made: Any = new Gone }" -> false).foreach {
      (kept, read) =>
        Scratch.using("bonsai-") { directory =>
          val classes  = compile(directory, Seq("Kept.scala" -> kept, "Gone.scala" -> "class Gone"), Vector.empty)
          val products = Outputs.files(classes).map(Output("m", Output.Scope.Main, _))
          Vector("Gone.class", "Gone.tasty").foreach(name => Files.delete(classes.resolve(name)))
          val result = Extraction.read(Manifest(
            Vector(Output("m", Output.Scope.Main, classes)),
            classpaths,
            Set.empty,
            products = products,
            jdk = targetJdk
          ))
          assertEquals(result.isRight, read, result.toString)
          if (read) assert(result.exists(_.graph.nodes.contains("Kept")), result.toString)
          else assert(result.left.exists(_.exists(_.contains("Gone.class"))), result.toString)
        }
    }
  }

  private def divided(directory: Path, padded: String): Manifest = {
    def padding(module: String) =
      Option.when(module == padded)("Padding.scala" -> s"package padding.$module\nclass One\nclass Two\nclass Three")
    val base = compile(
      directory.resolve("base"),
      Seq(
        "Base.scala" ->
          "package base\ntrait Greeter { def greet: String }\ncase class Mode(fast: Boolean, level: Int)\ntrait Sink { def put(slot: Int, width: Int): Int }\nobject Shared { def common: Int = 1; def fromPackaged: Int = 3 }"
      ),
      Vector.empty
    )
    val objects = compile(
      directory.resolve("objects"),
      Seq(
        "B.scala" ->
          "package a\nobject b extends base.Greeter with base.Sink {\n  def greet: String = \"b\"\n  def put(slot: Int, width: Int): Int = slot + width\n  def one: Int = base.Shared.common }"
      ) ++ padding("objects"),
      Vector(base)
    )
    val packaged = compile(
      directory.resolve("packaged"),
      Seq(
        "Shapes.scala" ->
          "package a.b\nsealed trait Shape\ncase class Circle(radius: Int) extends Shape\ncase class Square(side: Int) extends Shape",
        "Util.scala" ->
          "package a.b\ntrait Source { def next: Int }\nobject Util extends base.Greeter with base.Sink {\n  def put(slot: Int, width: Int): Int = slot + width\n  def greet: String = \"u\"\n  def two: Int = base.Shared.fromPackaged + 1\n}"
      ) ++ padding("packaged"),
      Vector(base)
    )
    val usingObjects = compile(
      directory.resolve("usingObjects"),
      Seq(
        "Main.scala" ->
          "object UsingObjects {\n  def twice(sink: base.Sink, n: Int): Int = sink.put(n, 5) + sink.put(n + 1, 5)\n  def modes(n: Int): Int = Seq(base.Mode(true, n), base.Mode(true, n + 1)).map(m => if (m.fast) m.level else 0).sum\n  def main(args: Array[String]): Unit = println(modes(1) + a.b.one + a.b.greet + (a.b: base.Greeter).greet + twice(a.b, args.length))\n}"
      ),
      Vector(base, objects)
    )
    val usingPackaged = compile(
      directory.resolve("usingPackaged"),
      Seq(
        "Main.scala" ->
          """object UsingPackaged {
            |  def area(shape: a.b.Shape): Int = shape match {
            |    case a.b.Circle(radius) => radius
            |    case a.b.Square(side)   => side
            |  }
            |  def count(source: a.b.Source): Int = source.next
            |  def mode(n: Int): Int = { val m = base.Mode(false, n); if (m.fast) m.level else 0 }
            |  def twice(sink: base.Sink, n: Int): Int = sink.put(n, 5) + sink.put(n + 1, 5)
            |  def main(args: Array[String]): Unit = {
            |    val greeter: base.Greeter = a.b.Util
            |    println(mode(1) + a.b.Util.two + count(new Counter) + area(a.b.Circle(1)) + greeter.greet + twice(a.b.Util, args.length))
            |  }
            |}
            |class Counter extends a.b.Source { override def next: Int = 1 }""".stripMargin
      ),
      Vector(base, packaged)
    )
    Manifest(
      Vector(
        Output("base", Output.Scope.Main, base),
        Output("objects", Output.Scope.Main, objects),
        Output("packaged", Output.Scope.Main, packaged),
        Output("usingObjects", Output.Scope.Main, usingObjects),
        Output("usingPackaged", Output.Scope.Main, usingPackaged)
      ),
      scoped(Map(
        "base"          -> classpath,
        "objects"       -> (classpath :+ base),
        "packaged"      -> (classpath :+ base),
        "usingObjects"  -> (classpath ++ Vector(base, objects)),
        "usingPackaged" -> (classpath ++ Vector(base, packaged))
      )),
      Set.empty,
      jdk = targetJdk
    )
  }

  private def readDivided(manifest: Manifest) = {
    val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
    assertEquals(result.problems, Vector.empty)
    assertEquals(result.notes, Vector.empty)
    result
  }

  private def eachSide(check: Extraction.Result => Unit): Unit = Seq("objects", "packaged").foreach { padded =>
    Scratch.using("bonsai-")(directory => check(readDivided(divided(directory, padded))))
  }

  test("an object in one module and a package of its name in an unrelated module read without problems") {
    eachSide { result =>
      val nodes = result.graph.nodes.keySet
      Seq("a.b", "a.b.Shape", "a.b.Circle", "a.b.Util", "UsingObjects", "UsingPackaged", "base.Shared").foreach(name =>
        assert(nodes.contains(name), s"$name not in ${nodes.toVector.sorted}")
      )
    }
  }

  test("calls into either side of a class and package clash resolve from the modules that depend on them") {
    eachSide { result =>
      val found = Members.findings(result.members, result.graph.nodes, result.reach)
      assertEquals(found.filter(_.rule == Finding.Rule.UnusedMember).map(_.detail), Vector.empty)
      Seq("a.b", "a.b.Util", "base.Shared", "base.Greeter", "a.b.Source").foreach(name =>
        assert(result.reach.live(name), s"$name is dead")
      )
    }
  }

  test("a sealed hierarchy and an override across modules of the separately read side stay known") {
    eachSide { result =>
      val sum = result.algebra.sums.find(_.node == "a.b.Shape")
      assertEquals(sum.map(_.cases.map(_.name).sorted), Some(Vector("a.b.Circle", "a.b.Square")))
      val members = result.members.members.map(member => member.name -> member).toMap
      assert(members("Counter.next").overriding, members.keySet.toString)
      assertEquals(members("Counter.next").bases, Vector("a.b.Source.next"))
      assert(members("a.b.greet").overriding, members.keySet.toString)
      assertEquals(members("a.b.Util.greet").bases, Vector("base.Greeter.greet"))
    }
  }

  test("an abstract method implemented on both sides of a class and package clash stays shared") {
    eachSide { result =>
      val found = Smells.findings(result.smells, result.algebra.products, result.graph.nodes)
      assertEquals(found.filter(_.rule == Finding.Rule.ConstantArgument).map(_.detail), Vector.empty)
    }
  }

  test("a module that needs both sides of a class and package clash fails naming it") {
    Scratch.using("bonsai-") { directory =>
      val whole    = divided(directory, "objects")
      val needing  = whole.outputs.filter(_.module == "usingObjects").map(_.copy(module = "both"))
      val packaged = whole.outputs.filter(_.module == "packaged").map(_.path)
      val manifest = whole.copy(
        outputs = whole.outputs ++ needing,
        classpath = whole.classpath +
          (("both" -> Output.Scope.Main) -> (whole.classpath("usingObjects" -> Output.Scope.Main) ++ packaged))
      )
      val errors = Extraction.read(manifest).fold(identity, result => fail(s"read: ${result.notes}"))
      assertEquals(errors.size, 1)
      assert(errors.head.startsWith("a.b is a class in module objects and a package in module packaged"), errors.head)
      assert(errors.head.contains("both"), errors.head)
    }
  }

  test("a module whose dependency holds the package of its class fails naming it whichever side is smaller") {
    Seq("app", "other").foreach { padded =>
      Scratch.using("bonsai-") { directory =>
        def padding(module: String) =
          Option.when(module == padded)("Padding.scala" ->
            s"package padding.$module\nclass One\nclass Two\nclass Three")
        val app = compile(
          directory.resolve("app"),
          Seq("B.scala" -> "package a\nobject b { def one: Int = 1 }") ++ padding("app"),
          Vector.empty
        )
        val zlib  = compile(directory.resolve("zlib"), Seq("L.scala" -> "package a.b\nclass L"), Vector.empty)
        val other = compile(
          directory.resolve("other"),
          Seq("O.scala" -> "package a.b\nclass O") ++ padding("other"),
          Vector.empty
        )
        val manifest = Manifest(
          Vector(
            Output("app", Output.Scope.Main, app),
            Output("zlib", Output.Scope.Main, zlib),
            Output("other", Output.Scope.Main, other)
          ),
          scoped(Map("app" -> (classpath :+ zlib), "zlib" -> classpath, "other" -> classpath)),
          Set.empty,
          jdk = targetJdk
        )
        val errors = Extraction.read(manifest).fold(identity, result => fail(s"read: ${result.notes}"))
        assertEquals(errors.size, 1)
        assert(errors.head.startsWith("a.b is a class in module app and a package in module"), errors.head)
        assert(errors.head.endsWith("app need both"), errors.head)
      }
    }
  }

  test("a class and a package that a library jar holds on the classpath of its module fail naming it") {
    Scratch.using("bonsai-") { directory =>
      val whole   = divided(directory, "objects")
      val library = archive(
        compile(directory.resolve("lib"), Seq("C.scala" -> "package a.b\nclass Held"), Vector.empty),
        directory.resolve("lib.jar"),
        "lib",
        "1.0"
      )
      val manifest =
        whole.copy(classpath =
          whole.classpath.updated(
            "usingObjects" -> Output.Scope.Main,
            whole.classpath("usingObjects" -> Output.Scope.Main) :+ library
          )
        )
      val errors = Extraction.read(manifest).fold(identity, result => fail(s"read: ${result.notes}"))
      assertEquals(errors.size, 1)
      assert(errors.head.contains("lib.jar"), errors.head)
    }
  }

  test("top-level definitions of a file named as a package read beside that package") {
    Scratch.using("bonsai-") { directory =>
      val first =
        compile(directory.resolve("a"), Seq("b.scala" -> "package a\ndef one: Int = 1"), Vector.empty)
      val second =
        compile(directory.resolve("b"), Seq("C.scala" -> "package a.b\nclass C { def two: Int = 2 }"), Vector.empty)
      val manifest = Manifest(
        Vector(Output("first", Output.Scope.Main, first), Output("second", Output.Scope.Main, second)),
        scoped(Map("first" -> classpath, "second" -> classpath)),
        Set.empty,
        jdk = targetJdk
      )
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      assertEquals(result.notes, Vector.empty)
      assert(result.graph.nodes.keySet.exists(_.contains("C")), result.graph.nodes.keySet.toString)
    }
  }

  test("modules whose classes and packages never share a name read together") {
    Scratch.using("bonsai-") { directory =>
      val first =
        compile(directory.resolve("a"), Seq("B.scala" -> "package a\nobject b { def one: Int = 1 }"), Vector.empty)
      val second =
        compile(directory.resolve("b"), Seq("C.scala" -> "package a.c\nclass C { def two: Int = 2 }"), Vector.empty)
      val manifest = Manifest(
        Vector(Output("first", Output.Scope.Main, first), Output("second", Output.Scope.Main, second)),
        scoped(Map("first" -> classpath, "second" -> classpath)),
        Set.empty,
        jdk = targetJdk
      )
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      assertEquals(result.notes, Vector.empty)
      assert(result.graph.nodes.keySet.exists(_.contains("C")), result.graph.nodes.keySet.toString)
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
      val manifest =
        Manifest(outputs, scoped(libraries.view.mapValues(_ +: classpath).toMap), Set.empty, jdk = targetJdk)
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
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
          scoped(Map("first" -> (older +: classpath), "second" -> (newer +: classpath))),
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
          scoped(Map("Config" -> (config +: classpath), "Format" -> (format +: classpath))),
          Set.empty,
          jdk = targetJdk
        )
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      assertEquals(result.graph.nodes.keySet.count(node => node.contains("UseConfig") || node.contains("UseFormat")), 2)
    }
  }

  test("a library body that refers to a package the classpath lacks is one note, not a failed extraction") {
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
      val manifest = Manifest(Vector(output), scoped(Map("m" -> (helper +: classpath))), Set.empty, jdk = targetJdk)
      val result   = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assert(result.graph.nodes.keySet.exists(_.contains("Use")), result.graph.nodes.keySet.toString)
      assertEquals(
        result.notes.filter(_.startsWith("could not read library bodies")),
        Vector(s"could not read library bodies of helper.Helper in $helper")
      )
      assertEquals(result.problems, Vector.empty)
      assert(result.notes.exists(_.contains("Use.tasty could not be analysed: dotty.tools.dotc.core.MissingType")))
    }
  }

  test("two versions of a package object leave the affected method a problem, not a failed run") {
    Scratch.using("bonsai-") { directory =>
      val old = compile(
        directory.resolve("old"),
        Seq(
          "Lib.scala" ->
            """package lib
              |trait Base[A]
              |trait Sub[A] extends Alias[A]""".stripMargin,
          "package.scala" -> "package object lib { type Alias[A] = Base[A] }"
        ),
        Vector.empty
      )
      val newer = compile(
        directory.resolve("new"),
        Seq("package.scala" -> "package object lib { val answer: Int = 1 }"),
        Vector.empty
      )
      val wrapper = compile(
        directory.resolve("wrapper"),
        Seq("Thing.scala" ->
          "package wrapper\nclass Thing extends lib.Sub[Int]\nobject Maker { val thing: Thing = new Thing }"),
        Vector(old)
      )
      val sources = Seq(
        "Use.scala" ->
          """object Use {
            |  def run(): Unit = { identity(wrapper.Maker.thing); () }
            |}""".stripMargin,
        "Fine.scala" -> "object Fine { def run(xs: List[Int]): Unit = { xs.map(_ + 1); () } }"
      )
      val output   = Output("m", Output.Scope.Main, compile(directory.resolve("use"), sources, Vector(old, wrapper)))
      val manifest =
        Manifest(Vector(output), scoped(Map("m" -> (wrapper +: newer +: old +: classpath))), Set.empty, jdk = targetJdk)
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assert(result.problems.exists(_.contains("Use.run could not be inferred")), result.problems.toString)
      assertEquals(
        Purity.findings(result.purity, result.graph.nodes).filter(_.rule == Finding.Rule.PureDiscard).map(_.node.name),
        Vector("Fine")
      )
    }
  }

  test("shadowing keeps the first version of an artifact") {
    val older  = Path.of("/cache/org/example/tool/1.9.0/tool-1.9.0.jar")
    val other  = Path.of("/cache/org/example/other/2.0.0/other-2.0.0.jar")
    val newest = Path.of("/cache/org/example/tool/1.10.0/tool-1.10.0.jar")
    val output = Path.of("/work/classes")
    assertEquals(Classpath.shadowed(Vector(older, other, newest, output)), Vector(older, other, output))
  }

  test("the read of a compile holds behind its classpath the libraries of the compiles it requires") {
    val older  = Path.of("/cache/org/example/tool/1.9.0/tool-1.9.0.jar")
    val newest = Path.of("/cache/org/example/tool/1.10.0/tool-1.10.0.jar")
    val shaded = Path.of("/cache/org/example/queues/3.3.0/queues-3.3.0.jar")
    val main   = Output.Scope.Main
    val core   = Output("core", main, Path.of("/work/core"))
    val app    = Output("app", Output.Scope.Test, Path.of("/work/app-test"))
    val whole  = Manifest(
      Vector(core, Output("app", main, Path.of("/work/app")), app),
      Map(
        ("core", main)             -> Vector(older, shaded),
        ("app", main)              -> Vector(newest),
        ("app", Output.Scope.Test) -> Vector.empty
      ),
      Set.empty,
      dependencies = Map("app" -> Set("core"))
    )
    assertEquals(Readings(whole).of(Set(app.part)), Vector(newest, shaded))
  }

  test("shadowing identifies a jar outside a repository layout by its manifest and unversioned name") {
    Scratch.using("bonsai-") { directory =>
      def jar(path: String, version: String) = {
        val file = directory.resolve(path)
        Files.createDirectories(file.getParent)
        archive(Files.createDirectories(directory.resolve(s"empty-$version")), file, "scala-library", version)
      }
      val boot  = jar("boot/scala-3.8.4/lib/scala-library.jar", "3.8.4")
      val rules = jar("cache/org/scala-lang/scala-library/3.9.0/scala-library-3.9.0.jar", "3.9.0")
      val rest  = jar("cache/org/scala-lang/scala-library/3.10.0/scala-library-3.10.0.jar", "3.10.0")
      for (order, first) <- Seq(
          Vector(boot, rules, rest) -> boot,
          Vector(rest, boot, rules) -> rest,
          Vector(rules, rest, boot) -> rules
        )
      do assertEquals(Classpath.shadowed(order), Vector(first))
    }
  }

  test("megamorphic sites list receivers by simple name unless two collide, count vals, and skip universals") {
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
          |  def main(args: Array[String]): Unit = {
          |    val figures = List[Figure](Circle(), Square(), Triangle())
          |    val named   = List[Named](A.Impl(), B.Impl(), Other())
          |    println((limits(figures), areas(figures), sides(figures), shown(figures), names(named), Plot().area))
          |  }
          |}""".stripMargin
      ),
      hot = Vector("**")
    )
    assertEquals(
      result.megamorphic.map(found => (found.site.caller, found.site.callee, found.names)).toSet,
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
        """case class Name(value: Vector[Int])
          |case class Money(amount: Int)
          |object Money { def apply(text: String): Money = new Money(text.toInt) }
          |object Render { def render(text: String, width: Int): String = text.take(width) }
          |object Validators {
          |  def trivial(size: Int): Int = { require(true); size }
          |  def positive(size: Int): Int = { require(size > 0); size }
          |}
          |object Main {
          |  val names = List(Name(Vector(1)), Name(Vector(2)))
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

  test("a generated source the manifest does not list reads under its absolute path") {
    Scratch.using("bonsai-") { directory =>
      val managed = directory.resolve("main").resolve("src_managed")
      val classes = compile(
        directory.resolve("main"),
        Seq("src_managed/Gen.scala" -> "package gen\nclass Gen { def value: Int = 1 }"),
        Vector.empty,
        Some(directory)
      )
      val result = Extraction.read(Manifest(
        Vector(Output("m", Output.Scope.Main, classes)),
        classpaths,
        Set.empty,
        root = Some(directory),
        generated = Vector(managed),
        jdk = targetJdk
      ))
      assertEquals(
        result.map(_.graph.nodes("gen.Gen").source),
        Right(managed.resolve("Gen.scala").toAbsolutePath.normalize.toString)
      )
    }
  }
}
