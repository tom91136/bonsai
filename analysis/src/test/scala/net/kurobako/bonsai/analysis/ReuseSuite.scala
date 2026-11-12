package net.kurobako.bonsai.analysis

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption.APPEND
import java.nio.file.attribute.FileTime
import net.kurobako.bonsai.testing.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

class ReuseSuite extends AnalysisSuite {
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(2, "min")

  private val Marker = "bonsai: inputs unchanged, report reused"

  private case class Ran(status: Int, out: String, err: String) {
    def reused: Boolean = err.linesIterator.contains(Marker)

    def unmarked: String = err.replace(s"$Marker\n", "")
  }

  private def invoke(args: Seq[String]): Ran = {
    val out    = ByteArrayOutputStream()
    val err    = ByteArrayOutputStream()
    val status = Console.withOut(out)(Console.withErr(err)(CLI.run(args.toList)))
    Ran(status, out.toString(UTF_8), err.toString(UTF_8))
  }

  private val sources = Seq(
    "Main.scala"  -> "object Main { def main(args: Array[String]): Unit = println(Tools.used()) }",
    "Tools.scala" -> "object Tools { def used(): Int = 1\n  def unused(): Int = 2 }"
  )

  private val compiled: Path = compile(Scratch.directory("bonsai-reuse-"), sources, Vector.empty)

  private def copy(from: Path, to: Path): Unit =
    Using.resource(Files.walk(from))(_.toList.asScala.foreach { path =>
      val target = to.resolve(from.relativize(path).toString)
      if (Files.isDirectory(path)) Files.createDirectories(target) else Files.copy(path, target): Unit
    })

  private class Fixture(val texts: Seq[String], home: Path) {
    val dir: Path      = Scratch.directory("bonsai-reuse-run-")
    val classes: Path  = Files.createDirectories(dir.resolve("classes"))
    val source: Path   = dir.resolve("src/Tools.scala")
    val library: Path  = dir.resolve("lib/scala3-library.jar")
    val manifest: Path = Files.createDirectories(dir.resolve("target/bonsai")).resolve("manifest.tsv")
    val store: Path    = manifest.resolveSibling("report.reuse")

    copy(compiled, classes)
    Files.createDirectories(source.getParent)
    Files.writeString(source, sources(1)._2)
    Files.createDirectories(library.getParent)
    private val jars = classpath.filter(Files.isRegularFile(_))
    Files.copy(jars.find(_.getFileName.toString.startsWith("scala3-library")).get, library)
    Files.writeString(
      manifest,
      (Seq(
        s"output\tm\tmain\t$classes",
        s"source\tm\tmain\t$source",
        s"jdk\t$home",
        s"root\t$dir"
      ) ++ jars.map(jar =>
        s"classpath\tm\tmain\t${if (jar.getFileName.toString.startsWith("scala3-library")) library else jar}"
      ) ++ texts).mkString("", "\n", "\n")
    )

    def args(extra: String*): Seq[String] = Seq("--manifest", manifest.toString, "--parallelism", "4") ++ extra

    def run(extra: String*): Ran = invoke(args(extra*))
  }

  private def fixture(texts: Seq[String] = Nil, home: Path = targetJdk.get): Fixture = Fixture(texts, home)

  /** Runs `before` twice, checks the second reuses the first, applies `change`, and checks the next run extracts again
    * and the one after reuses that.
    */
  private def assertInvalidated(f: Fixture, before: Seq[String] = Nil, after: Option[Seq[String]] = None)(
      change: => Unit
  ): Unit = {
    assert(!f.run(before*).reused)
    assert(f.run(before*).reused)
    change
    val next = after.getOrElse(before)
    assert(!f.run(next*).reused)
    assert(f.run(next*).reused)
  }

  test("an unchanged second run reuses the report, and prints the same output and status") {
    val f      = fixture()
    val first  = f.run("--purity")
    val second = f.run("--purity")
    assert(first.out.nonEmpty, first.toString)
    assert(first.err.contains("purity:"), first.err)
    assert(!first.reused)
    assert(second.reused, second.err)
    assertEquals(second.out, first.out)
    assertEquals(second.status, first.status)
    assertEquals(second.unmarked, first.unmarked)
    assert(Files.isRegularFile(f.store))
  }

  test("a failing report is reused with its status") {
    val f      = fixture()
    val first  = f.run("--error", "*")
    val second = f.run("--error", "*")
    assertEquals(first.status, 1)
    assert(second.reused, second.err)
    assertEquals(second.status, 1)
    assertEquals(second.out, first.out)
  }

  test("a changed output class file invalidates") {
    val f = fixture()
    assertInvalidated(f) {
      val file = Files.walk(f.classes).toList.asScala.find(_.toString.endsWith("Tools$.class")).get
      Files.write(file, Files.readAllBytes(file) :+ 0.toByte): Unit
    }
  }

  test("a file added to an output directory invalidates") {
    val f = fixture()
    assertInvalidated(f)(Files.writeString(f.classes.resolve("extra.txt"), "x"): Unit)
  }

  test("a changed resource file of the manifest invalidates, and an unlisted file beside it does not") {
    val f        = fixture()
    val services = Files.createDirectories(f.dir.resolve("resources/META-INF/services"))
    val listed   = services.resolve("a.Spi")
    val stray    = services.resolve("a.Spi.orig")
    Files.writeString(listed, "p.A\n")
    Files.writeString(stray, "p.B\n")
    Files.writeString(f.manifest, s"resource\tMETA-INF/services/a.Spi\t$listed\n", APPEND)
    assertInvalidated(f)(Files.writeString(listed, "p.C\n"): Unit)
    Files.writeString(stray, "p.D\n")
    Files.writeString(services.resolve(".a.Spi.swp"), "p.E\n")
    assert(f.run().reused)
  }

  test("a changed source file invalidates") {
    val f = fixture()
    assertInvalidated(f)(Files.writeString(f.source, sources(1)._2 + "\n// edited\n"): Unit)
  }

  test("a changed baseline invalidates") {
    val f        = fixture()
    val baseline = f.dir.resolve("baseline.tsv")
    Files.writeString(baseline, "")
    assertInvalidated(f, Seq("--baseline", baseline.toString))(Files.writeString(baseline, "unused\tTools\n"): Unit)
  }

  test("a changed purity lock invalidates") {
    val f    = fixture()
    val lock = f.dir.resolve("purity.tsv")
    Files.writeString(lock, "")
    assertInvalidated(f, Seq("--purity-lock", lock.toString))(Files.writeString(lock, "Tools.used\n"): Unit)
  }

  test("a changed argument invalidates") {
    val f = fixture()
    assertInvalidated(f, Nil, Some(Seq("--info")))(())
  }

  test("a changed jar on the classpath invalidates") {
    val f = fixture()
    assertInvalidated(f)(Files.setLastModifiedTime(f.library, FileTime.fromMillis(1000L)): Unit)
  }

  test("a changed release file of the manifest jdk invalidates") {
    val home    = Scratch.directory("bonsai-reuse-jdk-")
    val release = home.resolve("release")
    Files.createSymbolicLink(home.resolve("lib"), targetJdk.get.resolve("lib")): Unit
    Files.copy(targetJdk.get.resolve("release"), release): Unit
    val f = fixture(home = home)
    assertInvalidated(f)(Files.writeString(release, Files.readString(release) + "IMPLEMENTOR=\"other\"\n"): Unit)
  }

  test("a changed file a text rule reads invalidates") {
    val f     = fixture(Seq("text\t0\tkind\tterms", "text\t0\twords\tplan", "text\t0\tfiles\t**/*.txt"))
    val notes = f.dir.resolve("notes.txt")
    Files.writeString(notes, "nothing\n")
    val init = ProcessBuilder("git", "init", "-q").directory(f.dir.toFile).start()
    assert(init.waitFor(1, java.util.concurrent.TimeUnit.MINUTES) && init.exitValue() == 0)
    assertInvalidated(f)(Files.writeString(notes, "a plan\n"): Unit)
    assert(f.run().out.contains("forbidden-term"))
  }

  test("runs that write a baseline or a purity lock never reuse and leave no store") {
    for (flag <- Seq("--write-baseline", "--write-purity-lock")) {
      val f      = fixture()
      val target = f.dir.resolve("written.tsv").toString
      val first  = f.run(flag, target)
      val second = f.run(flag, target)
      assert(!first.reused && !second.reused, flag)
      assertEquals(second.out, first.out)
      assert(!Files.exists(f.store), flag)
    }
  }

  test("a baseline write keeps the entries of modules the run does not cover") {
    val f        = fixture()
    val baseline = f.dir.resolve("written.tsv")
    assert(!f.run("--write-baseline", baseline.toString).reused)
    val current = Files.readString(baseline)
    assert(current.linesIterator.exists(_.startsWith("unused-member\tTools\t")), current)
    Files.writeString(baseline, current + "unused-member\tTools\tlong gone\nunused-member\tOther\tkept\n")
    assert(!f.run("--write-baseline", baseline.toString).reused)
    val written = Files.readString(baseline)
    assert(written.linesIterator.contains("unused-member\tOther\tkept"), written)
    assert(!written.contains("long gone"), written)
    assertEquals(Report.keys(written) - "unused-member\tOther\tkept", Report.keys(current))
  }

  test("a corrupt store is a miss, and the run rewrites it") {
    val f     = fixture()
    val first = f.run()
    val key   = Files.readAllLines(f.store).get(0)
    for (corrupt <- Seq("", "garbage", s"$key\nwhat is this\n", s"$key\nout\tmaybe\tline\n")) {
      Files.writeString(f.store, corrupt)
      val next = f.run()
      assert(!next.reused, corrupt)
      assertEquals(next.out, first.out)
      assert(f.run().reused, corrupt)
    }
  }

  test("every manifest field is either hashed through its paths or read by the key by name") {
    val fields = classOf[Manifest].getDeclaredFields.map(_.getName).toSet
    val paths  = Set("outputs", "classpath", "sources", "models", "resources")
    // options, platforms, libraries, recorded, resourced, dependencies, products, texts, textExclusions and generated
    // are in the manifest bytes; root is read by the scan, and jdk names the release file that is hashed with the paths
    val named =
      Set(
        "libraries",
        "jdk",
        "options",
        "root",
        "platforms",
        "texts",
        "textExclusions",
        "generated",
        "recorded",
        "resourced",
        "dependencies",
        "products"
      )
    assertEquals(fields, paths ++ named)
    val manifest = Manifest(
      Vector(Output("a", Output.Scope.Main, Path.of("o"))),
      scoped(Map("a" -> Vector(Path.of("c")))),
      Set.empty,
      sources = Vector(Output("a", Output.Scope.Main, Path.of("s"))),
      models = Vector(Output("a", Output.Scope.Main, Path.of("m"))),
      resources = Vector(Resource("r", Path.of("r")))
    )
    assertEquals(Reuse.paths(manifest).map(_.toString).toSet, Set("o", "c", "s", "m", "r"))
    assertEquals(
      Reuse.paths(manifest.copy(jdk = Some(Path.of("home")))).map(_.toString).toSet,
      Set("o", "c", "s", "m", "r", "home/release")
    )
  }
}
