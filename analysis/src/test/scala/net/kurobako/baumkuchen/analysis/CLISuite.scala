package net.kurobako.baumkuchen.analysis

import java.nio.file.Path
import munit.FunSuite

class CLISuite extends FunSuite {
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
      "test-only-member",
      "duplicate-code",
      "invariant-violation",
      "redundant-default",
      "var-could-be-val",
      "huge-method",
      "method-limit",
      "purity-regression",
      "effect-policy"
    )
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
        Purity.Policy("demo.io", Set(Purity.Kind.Input, Purity.Kind.Output))
      ))
    )
    assertEquals(parsed.map(_.concurrent), Right(Set("demo.Pool.run")))
    assert(CLI.parse(List("--effects", "demo=reads,throws")).left.exists(_.startsWith("unknown effect kinds throws")))
    assert(CLI.parse(List("--effects", "demo")).left.exists(_.startsWith("an effects policy is written")))
  }

  test("unknown rule ids are rejected") {
    assert(CLI.parse(List("--error", "bogus")).left.exists(_.startsWith("unknown rule: bogus")))
    assert(CLI.parse(List("--disable", "bogus")).left.exists(_.startsWith("unknown rule: bogus")))
  }
}
