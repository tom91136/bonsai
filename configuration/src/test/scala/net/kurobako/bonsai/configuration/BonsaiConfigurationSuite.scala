package net.kurobako.bonsai.configuration

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.configuration.BonsaiConfiguration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

class BonsaiConfigurationSuite extends munit.FunSuite {

  private val directory = FunFixture[Path](
    _ => Files.createTempDirectory("bonsai-configuration-"),
    path => Using.resource(Files.walk(path))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  )

  private def read(path: Path, conf: String): Configuration = {
    val file = path.resolve(".bonsai.conf")
    Files.writeString(file, conf)
    BonsaiConfiguration.read(file.toFile)
  }

  private def failure(path: Path, conf: String): String =
    try fail(s"read ${read(path, conf)}")
    catch { case error: ConfigurationError => error.getMessage }

  directory.test("a project without its own flag takes the build-level library") { path =>
    assertEquals(read(path, "library = true\nprojects.api.wildcards = 1\n").library("api"), Some(true))
  }

  directory.test("a project flag overrides the build-level library") { path =>
    assertEquals(read(path, "library = true\nprojects.api.library = false\n").library("api"), Some(false))
  }

  directory.test("no library flag at either level gives no decision") { path =>
    assertEquals(read(path, "projects.api.wildcards = 1\n").library("api"), None)
  }

  directory.test("a file that does not exist is an empty configuration") { path =>
    val configured = BonsaiConfiguration.read(path.resolve("absent.conf").toFile)
    assertEquals(configured.build, BonsaiConfiguration.Settings(None, None, None, None, None))
    assertEquals(configured.text, Vector.empty)
    assertEquals(configured.projects, Map.empty)
  }

  directory.test("the build-level settings and lists are read") { path =>
    val configured = read(
      path,
      """disabled = [acyclic, local-throw]
        |errors = [broad-catch]
        |warnings = [identity-token]
        |wildcards = 3
        |baseline = "findings.tsv"
        |purity = "pure.tsv"
        |effects { "demo.io" = [input, output] }
        |concurrent = [demo.WorkerPool.run]
        |generated = ["**/target/generated/**"]
        |hot = ["demo.kernel.**"]
        |reflective = ["demo.rendered.*"]
        |intended = ["demo.A=demo.B"]
        |checked = [demo.where]
        |""".stripMargin
    )
    assertEquals(configured.build.disabled, Some(Set("acyclic", "local-throw")))
    assertEquals(configured.build.errors, Some(Set("broad-catch")))
    assertEquals(configured.build.warnings, Some(Set("identity-token")))
    assertEquals(configured.build.wildcards, Some(3))
    assertEquals(configured.baseline, Some("findings.tsv"))
    assertEquals(configured.purity, Some("pure.tsv"))
    assertEquals(configured.effects, Some(Map("demo.io" -> Set("input", "output"))))
    assertEquals(configured.concurrent, Some(Set("demo.WorkerPool.run")))
    assertEquals(configured.generated, Some(Set("**/target/generated/**")))
    assertEquals(configured.hot, Some(Set("demo.kernel.**")))
    assertEquals(configured.reflective, Some(Set("demo.rendered.*")))
    assertEquals(configured.intended, Some(Set("demo.A=demo.B")))
    assertEquals(configured.checked, Some(Set("demo.where")))
  }

  directory.test("the rules disabled over generated code are read by id, or all of them with *") { path =>
    assertEquals(
      read(path, "generated-disabled = [duplicate-code, \"*\"]\n").generatedDisabled,
      Some(Set("duplicate-code", "*"))
    )
    assertEquals(read(path, "generated = [\"gen/**\"]\n").generatedDisabled, None)
  }

  directory.test("bounded members keep the order they are named in") { path =>
    val configured = read(
      path,
      """bounded = [
        |  { member = "demo.Encoding.operands", max = 4, reason = "at most four operands" },
        |  { member = "demo.Encoding.modifiers", max = 2, reason = "at most two modifiers" }
        |]
        |""".stripMargin
    )
    assertEquals(
      configured.bounded,
      Vector(
        ("demo.Encoding.operands", 4, "at most four operands"),
        ("demo.Encoding.modifiers", 2, "at most two modifiers")
      )
    )
  }

  directory.test("nesting limits are read and others rejected") { path =>
    assertEquals(read(path, "nesting { depth = 4, size = 30 }\n").nesting, Map("depth" -> 4, "size" -> 30))
    assert(failure(path, "nesting { height = 4 }\n").contains("nesting.height"))
  }

  directory.test("a project resolves its own settings over the build's") { path =>
    val configured = read(
      path,
      """disabled = [acyclic]
        |errors = [broad-catch]
        |wildcards = 3
        |projects.demo { disabled = [local-throw], library = true }
        |""".stripMargin
    )
    assertEquals(
      configured.resolve("demo"),
      BonsaiConfiguration.Settings(Some(Set("local-throw")), Some(Set("broad-catch")), None, Some(3), Some(true))
    )
    assertEquals(configured.resolve("other"), configured.build)
  }

  directory.test("unknown keys fail the read, naming the keys there are") { path =>
    assert(failure(path, "unknown = 1\n").contains("unknown keys unknown"))
    assert(failure(path, "projects.demo.unknown = 1\n").contains("projects.demo.unknown"))
  }

  directory.test("a configuration that does not parse fails the read with the file") { path =>
    val message = failure(path, "disabled = [\n")
    assert(message.startsWith("bonsai: "), message)
    assert(message.contains(".bonsai.conf"), message)
  }

  directory.test("text rules become manifest lines by their index") { path =>
    val configured = read(
      path,
      """text {
        |  exclude = ["docs/**"]
        |  terms = [{ words = [alpha, beta], message = "no", files = ["*.md"] }]
        |  licences { message = "no licences", level = warn }
        |}
        |""".stripMargin
    )
    assertEquals(
      configured.text,
      Vector(
        "text-exclude\tdocs/**",
        "text\t0\tkind\tterms",
        "text\t0\tfiles\t*.md",
        "text\t0\tmessage\tno",
        "text\t0\twords\talpha",
        "text\t0\twords\tbeta",
        "text\t1\tkind\tlicences",
        "text\t1\tlevel\twarn",
        "text\t1\tmessage\tno licences"
      )
    )
  }

  directory.test("text rules reject fields their kind does not take") { path =>
    assert(failure(path, "text.binary { message = x, pattern = y }\n").contains("text.binary.pattern"))
    assert(failure(path, "text.nothing = 1\n").contains("text.nothing"))
    assert(failure(path, "text.terms = [{ words = [\"a\\tb\"], message = x }]\n").contains("tabs or newlines"))
  }

  test("the options name only the findings the analysis knows") {
    val analysis = Analysis(
      Set("duplicate-code", "acyclic"),
      Set("unused-member"),
      Set("huge-method"),
      Map("demo.io" -> Set("output", "input")),
      Set("demo.run"),
      Set("gen/**"),
      Set("duplicate-code", "*"),
      Set("demo.rendered.*"),
      Set("demo.kernel.**"),
      Set("demo.A=demo.B"),
      Vector(("demo.operands", 4, "four operands")),
      Map("size" -> 30, "depth" -> 4)
    )
    val options = analysis.options(
      Set("duplicate-code", "unused-member", "huge-method"),
      File("/b/base.tsv"),
      File("/b/pure.tsv")
    )
    assertEquals(
      options,
      Seq(
        "--disable",
        "duplicate-code",
        "--error",
        "unused-member",
        "--warn",
        "huge-method",
        "--baseline",
        "/b/base.tsv",
        "--purity-lock",
        "/b/pure.tsv",
        "--effects",
        "demo.io=input,output",
        "--concurrent",
        "demo.run",
        "--generated",
        "gen/**",
        "--generated-disable",
        "*",
        "--generated-disable",
        "duplicate-code",
        "--reflective",
        "demo.rendered.*",
        "--hot",
        "demo.kernel.**",
        "--intended",
        "demo.A=demo.B",
        "--bounded",
        "demo.operands",
        "4",
        "four operands",
        "--nesting",
        "--nesting-depth",
        "4",
        "--nesting-size",
        "30"
      )
    )
  }
}
