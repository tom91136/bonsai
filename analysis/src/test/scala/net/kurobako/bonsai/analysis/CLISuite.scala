package net.kurobako.bonsai.analysis

import java.nio.file.Path
import munit.FunSuite
import net.kurobako.bonsai.effects.Kind
import net.kurobako.bonsai.java.JavaLint

class CLISuite extends FunSuite {
  private def thrown(work: () => Int): Throwable =
    try
      fail(s"returned ${work()}")
    catch { case error: Throwable => error }

  test("background work returns its result, and rethrows whatever it threw, fatal errors included") {
    assertEquals(CLI.background(() => 7)(), 7)
    assertEquals(thrown(CLI.background(() => throw IllegalArgumentException("bad"))).getMessage, "bad")
    assert(thrown(CLI.background(() => throw StackOverflowError())).isInstanceOf[StackOverflowError])
    assert(thrown(CLI.background(() => throw OutOfMemoryError("heap"))).isInstanceOf[OutOfMemoryError])
  }

  test("options select, escalate and baseline rules by id") {
    val parsed = CLI.parse(List(
      "--manifest",
      "build.tsv",
      "--disable",
      "weaker-access",
      "--error",
      "unused",
      "--error",
      "duplicate-code",
      "--baseline",
      "baseline.tsv",
      "--write-baseline",
      "next.tsv",
      "--warn",
      "huge-method",
      "--mode",
      "ci",
      "--info"
    ))
    assertEquals(
      parsed,
      Right(CLI.Options(
        manifest = Some(Path.of("build.tsv")),
        generated = Vector.empty,
        info = true,
        disabled = Set("weaker-access"),
        errors = Set("unused", "duplicate-code"),
        warnings = Set("huge-method"),
        mode = CLI.Mode.CI,
        baseline = Some(Path.of("baseline.tsv")),
        record = Some(Path.of("next.tsv"))
      ))
    )
  }

  test("the ci mode escalates the strict rules on top of those named") {
    val strict = Set(
      "unused",
      "test-only",
      "unused-member",
      "unread-field",
      "test-only-member",
      "duplicate-code",
      "invariant-violation",
      "redundant-default",
      "var-could-be-val",
      "huge-method",
      "method-limit",
      "intermediate-collections",
      "purity-regression",
      "effect-policy"
    ) ++ JavaLint.chains
    assertEquals(
      CLI.parse(List("--mode", "ci", "--error", "complexity")).map(_.failing),
      Right(strict + "complexity")
    )
    assertEquals(CLI.parse(List("--mode", "dev", "--error", "complexity")).map(_.failing), Right(Set("complexity")))
    assert(CLI.parse(List("--mode", "release")).left.exists(_.startsWith("unknown mode: release")))
    assert(CLI.parse(List("--warn", "bogus")).left.exists(_.startsWith("unknown rule: bogus")))
  }

  test("every rule is escalated at once with *, except those kept as warnings") {
    val every = Finding.Rule.values.map(_.id).toSet
    assertEquals(CLI.parse(List("--error", "*")).map(_.failing), Right(every))
    assertEquals(
      CLI.parse(List("--error", "*", "--warn", "unused-member")).map(_.failing),
      Right(every - "unused-member")
    )
    assertEquals(CLI.parse(List("--mode", "ci", "--warn", "unused")).map(_.failing("unused")), Right(false))
  }

  test("purity is locked, written and held to policies, and other threads named") {
    val parsed = CLI.parse(List(
      "--purity-lock",
      "purity.tsv",
      "--write-purity-lock",
      "next.tsv",
      "--effects",
      "demo.*.core=",
      "--effects",
      "demo.io=input, output",
      "--concurrent",
      "demo.Pool.run"
    ))
    assertEquals(parsed.map(_.lock), Right(Some(Path.of("purity.tsv"))))
    assertEquals(parsed.map(_.relock), Right(Some(Path.of("next.tsv"))))
    assertEquals(
      parsed.map(_.policies),
      Right(Vector(
        Purity.Policy("demo.*.core", Set.empty),
        Purity.Policy("demo.io", Set(Kind.Input, Kind.Output))
      ))
    )
    assertEquals(parsed.map(_.concurrent), Right(Set("demo.Pool.run")))
    assert(CLI.parse(List("--effects", "demo=reads,throws")).left.exists(_.startsWith("unknown effect kinds throws")))
    assert(CLI.parse(List("--effects", "demo")).left.exists(_.startsWith("an effects policy is written")))
  }

  test("generated patterns and the purity summary flag are carried through") {
    assertEquals(
      CLI.parse(List("--generated", "**/src/generated/**", "--purity")).map(options =>
        (options.generated, options.purity)
      ),
      Right((Vector("glob:**/src/generated/**"), true))
    )
  }

  test("rules are disabled over generated code by id, or all of them with *") {
    assertEquals(
      CLI.parse(List("--generated-disable", "duplicate-code", "--generated-disable", "*")).map(_.generatedDisabled),
      Right(Set("duplicate-code", "*"))
    )
    assert(CLI.parse(List("--generated-disable", "bogus")).left.exists(_.startsWith("unknown rule: bogus")))
  }

  test("intended pairs are carried through, and a malformed pair is rejected") {
    assertEquals(
      CLI.parse(List("--intended", "a.Execute*=a.Render*", "--intended", "b.**=b.Other")).map(_.intended),
      Right(Vector("a.Execute*" -> "a.Render*", "b.**" -> "b.Other"))
    )
    assert(CLI.parse(List("--intended", "a.Execute")).left.exists(_.startsWith("an intended pair is written")))
    assert(CLI.parse(List("--intended", "=a.Render")).left.exists(_.startsWith("an intended pair is written")))
    assert(CLI.parse(List("--intended", "a=b=c")).left.exists(_.startsWith("an intended pair is written")))
  }

  test("bounded members are carried through with a positive max and a reason, and malformed entries are rejected") {
    assertEquals(
      CLI.parse(List("--bounded", "a.Encoding.operands", "4", "at most four")).map(_.bounded),
      Right(Vector(Complexity.Bounded("a.Encoding.operands", 4, "at most four")))
    )
    assert(CLI.parse(List("--bounded", "a.Encoding.operands", "4", " ")).left.exists(_.startsWith("a bounded entry")))
    assert(
      CLI.parse(List("--bounded", "a.Encoding.operands", "0", "at most none")).left.exists(
        _.startsWith("a bounded max is a positive integer")
      )
    )
  }

  test("nesting limits are carried through, and a limit below its least is rejected") {
    assertEquals(CLI.parse(Nil).map(_.nesting), Right(Nesting.Config()))
    assertEquals(
      CLI.parse(List(
        "--nesting",
        "--nesting-depth",
        "3",
        "--nesting-damping",
        "0",
        "--nesting-size",
        "120"
      )).map(_.nesting),
      Right(Nesting.Config(depth = 3, damping = 0, size = 120, enabled = true))
    )
    assert(CLI.parse(List("--nesting-depth", "0")).left.exists(_.startsWith("a nesting depth is an integer")))
    assert(CLI.parse(List("--nesting-damping", "-1")).left.exists(_.startsWith("a nesting damping is an integer")))
    assert(CLI.parse(List("--nesting-size", "many")).left.exists(_.startsWith("a nesting size is an integer")))
  }

  test("unknown rule ids are rejected") {
    assert(CLI.parse(List("--error", "bogus")).left.exists(_.startsWith("unknown rule: bogus")))
    assert(CLI.parse(List("--disable", "bogus")).left.exists(_.startsWith("unknown rule: bogus")))
  }
}
