package net.kurobako.bonsai.plugin

import java.nio.file.Files
import java.nio.file.Path
import munit.FunSuite
import net.kurobako.bonsai.collections.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The chains of collection calls the laws improve: each fixture is a source whose `// assert: <id>` comments name the
  * lints it must report, and the source `-rewrite` makes of it.
  */
class ChainsSuite extends FunSuite {
  private val Marker  = """// assert: ([a-z-]+)\s*$""".r.unanchored
  private val Setting = """// option: ([a-z=-]+)\s*$""".r.unanchored

  private val directory = Path.of(sys.props("bonsai.chains"))

  private val fixtures: List[String] = Using.resource(Files.list(directory))(
    _.toList.asScala.map(_.getFileName.toString).collect {
      case name if name.endsWith(".input.scala") =>
        name.stripSuffix(".input.scala")
    }.toList.sorted
  )

  private def read(name: String): String = Files.readString(directory.resolve(name))

  private def run(source: String, options: String*): (List[String], List[(String, Int)], String) =
    Compilations.run(Seq("Chains.scala" -> source), options) { (result, directory) =>
      (
        result.allErrors.map(_.message).toList,
        result.allWarnings.flatMap(warning => law(warning.message).map(_ -> (warning.pos.line + 1))).toList,
        Files.readString(directory.resolve("Chains.scala"))
      )
    }

  private val Ids = Laws.ids.toSet

  private def law(message: String): Option[String] = {
    val id = """\[bonsai:([a-z-]+)\]""".r.findFirstMatchIn(message).map(_.group(1))
    id.filter(Ids)
  }

  private def stripped(source: String): (String, List[(String, Int)]) = {
    val lines = source.split("\n", -1).toList
    val found = lines.zipWithIndex.collect { case (Marker(id), at) => id -> (at + 1) }
    (lines.map(line => Marker.replaceFirstIn(line, "")).mkString("\n"), found)
  }

  fixtures.foreach { name =>
    test(s"$name reports the laws it asserts and rewrites to its output") {
      val (source, expected) = stripped(read(s"$name.input.scala"))
      val options            = source.split("\n", -1).toList.collect { case Setting(option) => option }
      val (errors, found, _) = run(source, options*)
      assertEquals(errors, Nil)
      assertEquals(found.sorted.mkString("\n"), expected.sorted.mkString("\n"))
      val (rewriteErrors, _, rewritten) = run(source, ("-rewrite" :: options)*)
      assertEquals(rewriteErrors, Nil)
      assertEquals(rewritten, stripped(read(s"$name.output.scala"))._1)
    }
  }

  test("a chain in a hot class is a collect or a nest whatever the functions are") {
    val body = """  case class Node(children: List[Node])
      |  val nodes: List[Node] = Nil
      |  val numbers = List("1", "")
      |  val parsed = numbers.filter(_.nonEmpty).map(_.toLong)
      |  val path = nodes.flatMap(_.children).flatMap(_.children)
      |}""".stripMargin
    val source             = s"package demo\n\nobject Hot {\n$body\n\nobject Cold {\n$body\n"
    val (errors, found, _) = run(source, "hot=demo.Hot")
    assertEquals(errors, Nil)
    assertEquals(
      found.filter((id, _) => id == "filter-map-to-collect" || id == "flat-map-nesting").sorted,
      List("filter-map-to-collect" -> 7, "flat-map-nesting" -> 8)
    )
  }

  test("a law is a check that can be disabled") {
    val (source, expected) = stripped(read("CollectionChains.input.scala"))
    assert(expected.nonEmpty)
    val (errors, found, _) = run(source, "disable=filter-to-count")
    assertEquals(errors, Nil)
    assert(!found.exists(_._1 == "filter-to-count"))
    assert(found.exists(_._1 == "find-to-exists"))
  }

  test("a law is a check that can be an error") {
    val (source, _)        = stripped(read("CollectionChains.input.scala"))
    val (errors, found, _) = run(source, "error=filter-to-count")
    assert(errors.exists(_.contains("[bonsai:filter-to-count]")), errors.mkString("\n"))
    assert(!found.exists(_._1 == "filter-to-count"))
  }

  test("the default of an option is spelled with fold unless the option says otherwise") {
    val (source, _) = stripped(
      """object Spelling {
        |  val option: Option[Int] = Some(1)
        |  val mapped = option.map(_ + 1).getOrElse(0)// assert: map-get-or-else-to-fold
        |  val folded = option.fold(0)(_ + 1)// assert: fold-to-map-get-or-else
        |}""".stripMargin
    )
    def laws(options: String*) = run(source, options*)._2.map(_._1)
    assertEquals(laws(), List("map-get-or-else-to-fold"))
    assertEquals(laws("option-fold=fold"), List("map-get-or-else-to-fold"))
    assertEquals(laws("option-fold=map-get-or-else"), List("fold-to-map-get-or-else"))
  }

  test("every law is a known check") {
    val (errors, _, _) = run("object Empty", Laws.ids.map(id => s"disable=$id")*)
    assertEquals(errors, Nil)
  }
}
