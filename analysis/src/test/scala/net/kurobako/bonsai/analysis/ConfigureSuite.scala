package net.kurobako.bonsai.analysis

import java.io.ByteArrayOutputStream
import java.nio.file.Files
import munit.FunSuite
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.configuration.*
import net.kurobako.bonsai.testing.*

class ConfigureSuite extends FunSuite {
  private def lines(conf: String, module: String = "demo"): Either[String, Vector[String]] =
    Scratch.using("configure") { directory =>
      Files.writeString(directory.resolve(".bonsai.conf"), conf)
      Configure.lines(directory.resolve(".bonsai.conf"), module).map(_.map(_.replace(directory.toString, "<root>")))
    }

  test("a missing file names only the default checks skipped over generated code") {
    val missing = Scratch.using("configure")(directory => Configure.lines(directory.resolve("absent.conf"), "demo"))
    assertEquals(missing.map(_.filterNot(_.startsWith("option\t"))), Right(Vector("generated-disable\t*")))
  }

  test("the generated globs and the checks skipped over generated code are printed") {
    assertEquals(
      lines("generated = [\"**/gen/**\"]\ngenerated-disabled = [identity-token]\n")
        .map(_.filter(_.startsWith("generated"))),
      Right(Vector("generated\t**/gen/**", "generated-disable\tidentity-token"))
    )
    assertEquals(
      lines("generated-disabled = []\n").map(_.filter(_.startsWith("generated-disable"))),
      Right(Vector.empty[String])
    )
  }

  test("the enabled rules are written after the disabled ones") {
    val printed = lines("enabled = [ExplicitResultTypes]\ndisabled = [acyclic]\n")
    assertEquals(
      printed.map(_.filter(line => line.startsWith("disabled\t") || line.startsWith("enabled\t"))),
      Right(Vector("disabled\tacyclic", "enabled\tExplicitResultTypes"))
    )
  }

  test("a module's settings override the build's and the analysis options name known findings only") {
    val printed = lines(
      """disabled = [acyclic, duplicate-code]
        |errors = [unused-member]
        |wildcards = 3
        |hot = ["demo.kernel.**"]
        |checked = [demo.where]
        |bounded = [{ member = "demo.Encoding.operands", max = 4, reason = "at most four operands" }]
        |intended = ["demo.A=demo.B"]
        |generated-disabled = ["*"]
        |projects.demo { warnings = [huge-method], library = true }
        |""".stripMargin
    )
    assertEquals(
      printed,
      Right(Vector(
        "disabled\tacyclic",
        "disabled\tduplicate-code",
        "errors\tunused-member",
        "warnings\thuge-method",
        "wildcards\t3",
        "library\ttrue",
        "hot\tdemo.kernel.**",
        "bounded\tdemo.Encoding.operands:4",
        "checked\tdemo.where",
        "generated-disable\t*",
        "option\t--disable",
        "option\tduplicate-code",
        "option\t--error",
        "option\tunused-member",
        "option\t--warn",
        "option\thuge-method",
        "option\t--baseline",
        "option\t<root>/bonsai-baseline.tsv",
        "option\t--purity-lock",
        "option\t<root>/bonsai-purity.tsv",
        "option\t--generated-disable",
        "option\t*",
        "option\t--hot",
        "option\tdemo.kernel.**",
        "option\t--intended",
        "option\tdemo.A=demo.B",
        "option\t--bounded",
        "option\tdemo.Encoding.operands",
        "option\t4",
        "option\tat most four operands"
      ))
    )
  }

  test("the option fold is printed for the module that resolves it") {
    val conf = "option-fold = map-get-or-else\nprojects.demo { option-fold = fold }\n"
    assertEquals(lines(conf).map(_.filter(_.startsWith("option-fold"))), Right(Vector("option-fold\tfold")))
    assertEquals(
      lines(conf, "other").map(_.filter(_.startsWith("option-fold"))),
      Right(Vector("option-fold\tmap-get-or-else"))
    )
    assertEquals(lines("wildcards = 2\n").map(_.filter(_.startsWith("option-fold"))), Right(Vector.empty[String]))
  }

  test("the modules left out are printed whichever module is asked for") {
    val conf = "excluded-modules = [docs, broken]\n"
    val left = Vector("excluded\tbroken", "excluded\tdocs")
    assertEquals(lines(conf).map(_.filter(_.startsWith("excluded\t"))), Right(left))
    assertEquals(lines(conf, "other").map(_.filter(_.startsWith("excluded\t"))), Right(left))
  }

  test("the spellings the configuration accepts are those the laws know") {
    assertEquals(BonsaiConfiguration.OptionFolds, OptionFold.values.toSeq.map(_.key))
    assertEquals(OptionFold.Default.key, BonsaiConfiguration.OptionFolds.head)
  }

  test("the text rules are printed as manifest lines") {
    val printed = lines("text { exclude = [\"docs/**\"], binary { message = no } }\n")
    assertEquals(
      printed.map(_.filter(line => line.startsWith("text"))),
      Right(Vector("text-exclude\tdocs/**", "text\t0\tkind\tbinary", "text\t0\tmessage\tno"))
    )
  }

  test("a configuration that does not read is an error") {
    assert(lines("nothing = 1\n").left.exists(_.contains("unknown keys nothing")))
  }

  test("the entries are written to the given file and not printed") {
    Scratch.using("configure") { directory =>
      val conf = directory.resolve(".bonsai.conf")
      val out  = directory.resolve("entries.tsv")
      Files.writeString(conf, "disabled = [acyclic]\n")
      val arguments = Array("--config", conf.toString, "--module", "demo", "--out", out.toString)
      val printed   = ByteArrayOutputStream()
      Console.withOut(printed)(Configure.main(arguments))
      assertEquals(printed.toString, "")
      assertEquals(
        Files.readString(out).linesIterator.filter(_.startsWith("disabled\t")).toVector,
        Vector("disabled\tacyclic")
      )
    }
  }
}
