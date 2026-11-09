package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import munit.FunSuite
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.testing.*
import scala.collection.mutable

class CompiledSuite extends FunSuite {
  import Recorded.Level.*

  private class Build(val root: Path) {
    val classes: Path                                            = Files.createDirectories(root.resolve("app/classes"))
    val sources: mutable.ReusableBuilder[Output, Vector[Output]] = Vector.newBuilder[Output]

    def source(name: String, content: String, scope: Output.Scope = Output.Scope.Main): Path = {
      val file = root.resolve(s"app/src/$name")
      Files.createDirectories(file.getParent)
      Files.writeString(file, content)
      sources += Output("app", scope, file)
      file
    }

    def record(file: Path, entries: Recorded.Entry*): Unit = recordIn(classes, file, entries*)

    def recordIn(output: Path, file: Path, entries: Recorded.Entry*): Unit =
      Recorded.write(
        output,
        Recorded.Record(Recorded.key(root, file), Recorded.digest(Files.readAllBytes(file)), entries.toList)
      )

    def manifest(recorded: Set[String] = Set("app"), generated: Vector[Path] = Vector.empty): Manifest =
      Manifest(
        Vector(Output("app", Output.Scope.Main, classes)),
        Map.empty,
        Set.empty,
        sources = sources.result(),
        root = Some(root),
        recorded = recorded,
        generated = generated
      )
  }

  private def build(name: String)(body: Build => Unit): Unit =
    test(name)(Scratch.using("bonsai-compiled-")(root => body(Build(root))))

  private def shown(found: Either[Vector[String], Vector[Finding]]): Vector[String] =
    found.toOption.get.map(finding =>
      s"${finding.level.id} ${finding.rule.id} ${finding.node.source}:${finding.node.line}"
    )

  build("each record becomes a finding of the rule it names, at the level the compiler plugin judged") { build =>
    val main = build.source("Main.scala", "object Main")
    build.record(
      main,
      Recorded.Entry("identical-branches", Error, 4, "both branches are the same"),
      Recorded.Entry("long-arguments", Warning, 9, "too many"),
      Recorded.Entry("inferred-refined-type", Info, 2, "refined")
    )
    val found = Compiled.findings(build.manifest())
    assertEquals(
      shown(found).sorted,
      Vector(
        "error identical-branches app/src/Main.scala:4",
        "info inferred-refined-type app/src/Main.scala:2",
        "warning long-arguments app/src/Main.scala:9"
      )
    )
    val finding = found.toOption.get.find(_.rule.id == "identical-branches").get
    assertEquals(finding.detail, "both branches are the same")
    assertEquals(finding.node.module, "app")
    assertEquals(finding.node.scope, Output.Scope.Main)
  }

  build("the report fails on a recorded error, whatever the rules of the analysis fail on") { build =>
    val main = build.source("Main.scala", "object Main")
    build.record(main, Recorded.Entry("a-check", Error, 1, "fails"), Recorded.Entry("b-check", Warning, 2, "passes"))
    val findings = Compiled.findings(build.manifest()).toOption.get
    val options  = Report.Options(Set.empty, Set.empty, Set.empty, info = false)
    val lines    = Report(findings, Map.empty, options)
    assertEquals(
      lines.map(line => line.level -> line.finding.rule.id).toSet,
      Set("error" -> "a-check", "warning" -> "b-check")
    )
    val nonfatal = Report(findings, Map.empty, options.copy(nonfatal = Set("a-check"), failing = Set("b-check")))
    assertEquals(
      nonfatal.map(line => line.level -> line.finding.rule.id).toSet,
      Set("error" -> "a-check", "warning" -> "b-check")
    )
  }

  build("a baseline holds back no compiler finding, and a baseline written leaves them out") { build =>
    val main = build.source("Main.scala", "object Main")
    build.record(main, Recorded.Entry("a-check", Error, 1, "fails"))
    val findings = Compiled.findings(build.manifest()).toOption.get
    val baseline = Report.Options(Set.empty, Set.empty, findings.map(_.key).toSet, info = false)
    assertEquals(Report(findings, Map.empty, baseline).map(_.level), Vector("error"))
    assertEquals(Report.merge("", Report(findings, Map.empty, baseline), Map.empty, Set.empty), "\n")
  }

  build("information shows only when asked for, as for the rules of the analysis") { build =>
    val main = build.source("Main.scala", "object Main")
    build.record(main, Recorded.Entry("a-check", Info, 1, "fyi"))
    val findings = Compiled.findings(build.manifest()).toOption.get
    assertEquals(
      Report(findings, Map.empty, Report.Options(Set.empty, Set.empty, Set.empty, info = false)),
      Vector.empty
    )
    assertEquals(
      Report(findings, Map.empty, Report.Options(Set.empty, Set.empty, Set.empty, info = true)).map(_.level),
      Vector("info")
    )
  }

  build("a source with no record is not a partial report but a failure that says what to do") { build =>
    val seen    = build.source("Seen.scala", "object Seen")
    val missing = build.source("Missing.scala", "object Missing")
    build.record(seen)
    val found = Compiled.findings(build.manifest())
    assert(found.left.exists(_.exists(_.contains("app/src/Missing.scala"))), found.toString)
    assert(found.left.exists(_.exists(_.contains("clean"))), found.toString)
    assert(found.left.exists(!_.exists(_.contains("Seen.scala"))), found.toString)
    build.record(missing)
    assertEquals(shown(Compiled.findings(build.manifest())), Vector.empty)
  }

  build("a record made of other contents than the source holds is stale and fails") { build =>
    val main = build.source("Main.scala", "object Main")
    build.record(main, Recorded.Entry("a-check", Error, 1, "old"))
    Files.writeString(main, "object Main { def changed = 1 }")
    val found = Compiled.findings(build.manifest())
    assert(found.left.exists(_.exists(_.contains("app/src/Main.scala"))), found.toString)
  }

  build("a record of a source that is gone, or that the module no longer compiles, is not reported") { build =>
    val main    = build.source("Main.scala", "object Main")
    val gone    = build.root.resolve("app/src/Gone.scala")
    val dropped = build.root.resolve("app/src/Dropped.scala")
    Files.writeString(gone, "object Gone")
    Files.writeString(dropped, "object Dropped")
    build.record(main)
    build.record(gone, Recorded.Entry("a-check", Error, 1, "stale"))
    build.record(dropped, Recorded.Entry("a-check", Error, 1, "stale"))
    Files.delete(gone)
    assertEquals(shown(Compiled.findings(build.manifest())), Vector.empty)
  }

  build("a generated source the build compiles is read though the manifest lists no source for it") { build =>
    val main      = build.source("Main.scala", "object Main")
    val generated = build.root.resolve("app/target/gen/Gen.scala")
    Files.createDirectories(generated.getParent)
    Files.writeString(generated, "object Gen")
    build.record(main)
    build.record(generated, Recorded.Entry("a-check", Warning, 3, "in generated code"))
    val found = Compiled.findings(build.manifest(generated = Vector(generated.getParent)))
    assertEquals(shown(found), Vector("warning a-check app/target/gen/Gen.scala:3"))
  }

  build("only sources of Scala are expected to have a record") { build =>
    val main = build.source("Main.scala", "object Main")
    val _    = build.source("Native.java", "class Native {}")
    build.record(main)
    assertEquals(shown(Compiled.findings(build.manifest())), Vector.empty)
  }

  build("a module the compiler plugin did not record is not checked") { build =>
    val _ = build.source("Main.scala", "object Main")
    assertEquals(shown(Compiled.findings(build.manifest(recorded = Set.empty))), Vector.empty)
  }

  build("a record in any output of the module and scope serves, and the same finding in two is one") { build =>
    val other = Files.createDirectories(build.root.resolve("app/other-classes"))
    val main  = build.source("Main.scala", "object Main")
    val entry = Recorded.Entry("a-check", Error, 1, "twice")
    build.record(main, entry)
    build.recordIn(other, main, entry)
    val manifest = build.manifest()
    val both     = manifest.copy(outputs = manifest.outputs :+ Output("app", Output.Scope.Main, other))
    assertEquals(shown(Compiled.findings(both)), Vector("error a-check app/src/Main.scala:1"))
    val empty = Files.createDirectories(build.root.resolve("app/empty"))
    val third = manifest.copy(outputs = Vector(Output("app", Output.Scope.Main, empty), manifest.outputs.head))
    assertEquals(shown(Compiled.findings(third)), Vector("error a-check app/src/Main.scala:1"))
  }
}
