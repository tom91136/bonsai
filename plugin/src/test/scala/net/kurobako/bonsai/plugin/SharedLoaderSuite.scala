package net.kurobako.bonsai.plugin

import java.lang.ref.WeakReference
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.attribute.FileTime
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.zip.ZipFile
import munit.FunSuite
import net.kurobako.bonsai.testing.*
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Using

class SharedLoaderSuite extends FunSuite {
  private val sources = Seq(
    "Lints.scala" ->
      """object Lints {
        |  def check(left: Array[Int], right: Array[Int]) = left == right
        |  def sum(xs: List[Int]): Int = {
        |    var i   = 0
        |    var sum = 0
        |    while (i < xs.size) { sum += xs(i); i += 1 }
        |    sum
        |  }
        |}""".stripMargin
  )

  private def compile(isolated: Isolated) = isolated.compile(Isolated.jar, sources)

  @tailrec
  private def collected(references: Seq[(String, WeakReference[?])], attempts: Int = 0): List[String] = {
    val alive = references.filter((_, reference) => Option(reference.get).isDefined).map(_._1).toList
    if (alive.isEmpty || attempts >= 100) alive
    else {
      sys.runtime.gc()
      Thread.sleep(50)
      collected(references, attempts + 1)
    }
  }

  test("the plugin reports as it does when its classes load afresh for each compilation") {
    val diagnostics = compile(Isolated()).diagnostics
    assert(diagnostics.contains("[bonsai:array-equality]"), diagnostics)
    assert(diagnostics.contains("[bonsai:linear-in-loop]"), diagnostics)
  }

  test("every compilation of a compiler runs the plugin from one loader") {
    val isolated = Isolated()
    val runs     = List.fill(4)(compile(isolated))
    assertEquals(runs.map(_.loaders.size), List.fill(4)(1))
    assertEquals(runs.flatMap(_.loaders).distinct.size, 1)
    assertEquals(runs.map(_.diagnostics).distinct.size, 1)
  }

  test("finished compilations are collected while their compiler lives on") {
    val isolated = Isolated()
    val bases    = List.fill(4)(compile(isolated).base)
    assertEquals(collected(bases.map("compilation" -> _)), Nil)
    assertEquals(compile(isolated).diagnostics.isEmpty, false)
  }

  test("a discarded compiler is collected with the loader of the plugin it ran") {
    def discarded() = {
      val isolated = Isolated()
      val runs     = List.fill(3)(compile(isolated))
      Seq[(String, WeakReference[?])](
        "compiler loader" -> WeakReference(isolated.loader),
        "plugin loader"   -> WeakReference(runs.head.loaders.head)
      ) ++ runs.map("compilation" -> _.base)
    }
    assertEquals(collected(discarded()), Nil)
  }

  test("concurrent compilations report what serial ones do") {
    val isolated = Isolated()
    val pool     = Executors.newFixedThreadPool(8)
    try {
      val concurrent = (1 to 16).map(_ => pool.submit(() => compile(isolated))).map(_.get(5, TimeUnit.MINUTES))
      val serial     = compile(isolated)
      concurrent.foreach(run => assertEquals(run.diagnostics, serial.diagnostics))
      assertEquals((serial +: concurrent).flatMap(_.loaders).distinct.size, 1)
    } finally pool.shutdownNow(): Unit
  }

  test("a jar rewritten under the same path gets a loader of its own") {
    Scratch.using("bonsai-jar-") { directory =>
      val jar      = Files.copy(Isolated.jar, directory.resolve("bonsai-plugin.jar"))
      val isolated = Isolated()
      val first    = isolated.compile(jar, sources)
      val again    = isolated.compile(jar, sources)
      assert(first.loaders.head eq again.loaders.head)
      Files.setLastModifiedTime(jar, FileTime.from(Files.getLastModifiedTime(jar).toInstant.plusSeconds(60)))
      val rebuilt = isolated.compile(jar, sources)
      assert(rebuilt.loaders.head ne first.loaders.head)
      assertEquals(rebuilt.diagnostics, first.diagnostics)
    }
  }

  test("a plugin in a directory is loaded afresh by each compilation") {
    Scratch.using("bonsai-exploded-") { directory =>
      Using.resource(ZipFile(Isolated.jar.toFile)) { archive =>
        archive.entries.asScala.filterNot(_.isDirectory).foreach { entry =>
          val target = directory.resolve(entry.getName)
          Files.createDirectories(target.getParent)
          Using.resource(archive.getInputStream(entry))(Files.copy(_, target)): Unit
        }
      }
      val isolated = Isolated()
      val first    = isolated.compile(directory, sources)
      val second   = isolated.compile(directory, sources)
      assert(first.loaders.head ne second.loaders.head)
      assertEquals(first.diagnostics, second.diagnostics)
      assert(first.diagnostics.contains("[bonsai:array-equality]"), first.diagnostics)
    }
  }

  test("the loader of a jar goes with the compiler it links to") {
    def discarded() = {
      val compiler = URLClassLoader(Array(Isolated.jar.toUri.toURL), ClassLoader.getPlatformClassLoader)
      val anchor   = compiler.loadClass("dotty.tools.dotc.plugins.BonsaiLoaders$")
      val shared   = Loaders.shared(anchor, Isolated.jar)
      assert(shared eq Loaders.shared(anchor, Isolated.jar))
      assert(shared.getParent eq compiler)
      assert(shared ne Loaders.shared(classOf[BonsaiEntry], Isolated.jar))
      Seq[(String, WeakReference[?])](
        "compiler loader" -> WeakReference(compiler),
        "plugin loader"   -> WeakReference(shared)
      )
    }
    assertEquals(collected(discarded()), Nil)
  }
}
