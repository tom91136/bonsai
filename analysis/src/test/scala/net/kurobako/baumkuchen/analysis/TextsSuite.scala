package net.kurobako.baumkuchen.analysis

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import munit.FunSuite
import scala.jdk.CollectionConverters.*
import scala.util.Using

class TextsSuite extends FunSuite {
  private def rule(fields: (String, String)*): Texts.Rule =
    Texts.rules(Map(0 -> fields.groupMap(_._1)(_._2).view.mapValues(_.toVector).toMap)).fold(fail(_), _.head)

  private def check(text: String, rules: Texts.Rule*): Vector[(String, Int, String)] =
    Texts.check("src/a.cc", text.getBytes(StandardCharsets.UTF_8), rules.toVector).map(finding =>
      (finding.rule.id, finding.node.line, finding.detail)
    )

  test("a forbidden term matches as a word, a plural, and inside identifiers capitalised or upper case") {
    val terms = rule("kind" -> "terms", "words" -> "plan", "words" -> "contract")
    assertEquals(
      check(
        "a plan\nplans\nExecutionPlan x\nPLAN_SIZE\nplanet Planet PLANE explanation\ncontractor\nTheContracts here",
        terms
      ),
      Vector(
        ("forbidden-term", 1, "plan is a forbidden term"),
        ("forbidden-term", 2, "plan is a forbidden term"),
        ("forbidden-term", 3, "plan is a forbidden term"),
        ("forbidden-term", 4, "plan is a forbidden term"),
        ("forbidden-term", 7, "contract is a forbidden term")
      )
    )
  }

  test("a forbidden pattern reports each line it matches, and an ignore marker exempts its line and the next") {
    val literal = rule(
      "kind"    -> "patterns",
      "pattern" -> """\bU?INT(?:8|16|32|64|MAX|PTR)_C\b""",
      "message" -> "write uint64_t{n}"
    )
    assertEquals(
      check(
        "auto a = UINT64_C(1);\n// baumkuchen:ignore forbidden-pattern\nauto b = INT8_C(2);\nauto c = INT32_C(3);\nUINT64_CX",
        literal
      ),
      Vector(("forbidden-pattern", 1, "write uint64_t{n}"), ("forbidden-pattern", 4, "write uint64_t{n}"))
    )
  }

  test("a binary file is reported once, and line rules skip it") {
    val binary = rule("kind" -> "binary")
    val terms  = rule("kind" -> "terms", "words" -> "plan")
    assertEquals(
      Texts.check("blob.bin", Array[Byte](112, 108, 97, 110, 0, 1), Vector(binary, terms)).map(_.detail),
      Vector("is binary; embed it as text with the commands that produce it")
    )
    assertEquals(check("plain text", binary), Vector.empty)
  }

  test("a line can need a blank line before it, and a block of lines a blank line after it") {
    val tests = rule(
      "kind"    -> "preceded",
      "pattern" -> """^[A-Z_]*TEST_CASE[A-Z_]*\s*\(""",
      "message" -> "put a blank line before a test case"
    )
    val includes = rule(
      "kind"      -> "followed",
      "pattern"   -> """^\s*#\s*include\b""",
      "continues" -> """^\s*#\s*(?:else|elif)\b""",
      "neutral"   -> """^\s*#""",
      "message"   -> "put a blank line after the includes"
    )
    val source =
      """TEST_CASE("first") {}
        |int x;
        |TEST_CASE("second") {}
        |
        |TEMPLATE_TEST_CASE("third") {}
        |#include <a>
        |#ifdef B
        |#include <b>
        |#else
        |#include <c>
        |#endif
        |int y;
        |#include <d>
        |
        |int z;""".stripMargin
    assertEquals(
      check(source, tests, includes),
      Vector(
        ("blank-line-before", 3, "put a blank line before a test case"),
        ("blank-line-after", 12, "put a blank line after the includes")
      )
    )
  }

  test("rules are read from the manifest, and a rule missing what it needs is rejected") {
    val file = Files.createTempFile("baumkuchen-", ".tsv")
    try {
      Files.writeString(
        file,
        "text-exclude\t**/vendor/**\ntext\t0\tkind\tterms\ntext\t0\twords\tplan\ntext\t0\tlevel\twarning\n" +
          "text\t1\tkind\tpatterns\ntext\t1\tpattern\tx\n"
      )
      val manifest = Manifest.read(file).fold(fail(_), identity)
      assertEquals(manifest.textExclusions, Vector("**/vendor/**"))
      assertEquals(Texts.rules(manifest.texts), Left("text rule 1: patterns take a message"))
      val terms = Texts.rules(manifest.texts.removed(1)).fold(fail(_), identity)
      assertEquals(
        terms.map(rule => (rule.kind, rule.level, rule.words)),
        Vector((Texts.Kind.Terms, Finding.Level.Warning, Vector("plan")))
      )
    } finally Files.delete(file)
  }

  test("the files git sees are checked against the globs of each rule, and errors fail unless kept as warnings") {
    val root = Files.createTempDirectory("baumkuchen-texts-")
    try {
      def write(path: String, text: String) = {
        val file = root.resolve(path)
        Files.createDirectories(file.getParent)
        Files.writeString(file, text): Unit
      }
      write("main.cc", "a plan\n")
      write("notes.md", "a plan\n")
      write("vendor/lib.cc", "a plan\n")
      write("ignored.cc", "a plan\n")
      write(".gitignore", "ignored.cc\n")
      val init = ProcessBuilder("git", "init", "-q").directory(root.toFile).start()
      assert(init.waitFor(1, java.util.concurrent.TimeUnit.MINUTES) && init.exitValue() == 0)
      val terms = rule("kind" -> "terms", "words" -> "plan", "files" -> "**/*.cc")
      val found = Texts.findings(root, Texts.matchers(Vector("**/vendor/**")), Vector(terms)).fold(fail(_), identity)
      assertEquals(found.map(finding => s"${finding.node.source}:${finding.node.line}"), Vector("main.cc:1"))
      val options = Report.Options(Set.empty, Set.empty, Set.empty, info = false)
      assertEquals(Report(found, Map.empty, options).map(_.level), Vector("error"))
      assertEquals(
        Report(found, Map.empty, options.copy(lenient = Set("forbidden-term"))).map(_.level),
        Vector("warning")
      )
      assertEquals(
        Report(found, Map.empty, options.copy(baseline = found.map(_.key).toSet)).map(_.level),
        Vector("warning")
      )
    } finally Using.resource(Files.walk(root))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }
}
