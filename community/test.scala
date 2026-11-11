#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
//> using dep com.lihaoyi::ujson:4.4.3
//> using file projects.scala
//> using file packing.scala
//> using file health.scala
//> using file summary.scala
// Checks the community programs' logic: health classification, comparison with the previous run, shard packing.
// usage: test.scala

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path

val banner = "bonsai: relaxed mode, 176 of 177 analysis rules active"

def output(
    status: String = "ok",
    log: Vector[String] = Vector.empty,
    err: Vector[String] = Vector.empty,
    crash: Vector[String] = Vector.empty,
    runs: Vector[(String, Int)] = Vector.empty,
    findings: Vector[String] = Vector.empty,
    name: String = "p",
    failing: Set[String] = Set.empty
): Output = Output(name, status, log, err, crash, runs, failing, findings)

def kinds(o: Output): Vector[String] = Health.assess(o).map(_.kind)

@main def test(): Unit = {
  var failed                                           = Vector.empty[String]
  def check(name: String)(condition: => Boolean): Unit =
    if (
      !(try condition
      catch { case e: Throwable => println(s"$name: $e"); false })
    ) failed :+= name

  check("a clean ok build is healthy")(kinds(output(log =
    Vector("[info] compiling", banner, "[success] Total time")
  )).isEmpty)
  check("an sbt note from the plugin is not a finding")(
    kinds(output(log = Vector("[info] note: bonsai analyses Scala 3 only and left out src (Scala 2.12.21)"))).isEmpty
  )
  val undefined  = "note: undefined: new io.netty.X # -1: TermRef(TypeRef(NoPrefix,X),<init>) at readTasty"
  val unanalysed =
    "note: /work/A.tasty could not be analysed: dotty.tools.dotc.core.MissingType: Cannot resolve reference to type a.B."
  check("a bare note line is a problem")(kinds(output(log = Vector(banner, "note: something else"))) ==
    Vector("problem"))
  check("a note on an undefined reference is not a failure")(kinds(output(log = Vector(banner, undefined))).isEmpty)
  check("a note on a missing type is not a failure")(kinds(output(err = Vector(unanalysed))).isEmpty)
  check("such notes are listed")(Health.missing(output(log = Vector(banner, undefined, unanalysed, undefined))) ==
    Vector(undefined, unanalysed))
  check("an undefined note from another phase is a problem")(
    kinds(output(log = Vector("note: undefined: x # 3: T at typer"))) == Vector("problem")
  )
  check("a note that fails on another error is a problem")(
    kinds(output(log = Vector("note: /work/A.tasty could not be analysed: java.lang.AssertionError: boom"))) ==
      Vector("problem")
  )
  check("a problem line naming a missing type is a problem")(
    kinds(output(log = Vector("problem: /work/A.tasty could not be analysed: dotty.tools.dotc.core.MissingType: x"))) ==
      Vector("problem")
  )
  check("the read-in-runs line is not a clash")(
    kinds(output(err =
      Vector(
        "read in 2 groups: Foo is a class in a.jar and a package in b.jar, which the compiler cannot read together"
      )
    )).isEmpty
  )
  check("the read-in-runs line is not a clash with a prefix")(
    kinds(output(log =
      Vector("[info] read in 3 groups: Foo is a class in a.jar and a package in b.jar")
    )).isEmpty
  )
  check("a clash that fails the analysis is red")(
    kinds(
      output(
        status = "failed",
        log = Vector(
          "Foo is a class in a.jar and a package in b.jar, which the compiler cannot read together, and m, n need both",
          "[error] bonsai analysis failed"
        )
      )
    ) == Vector("analysis-failed", "clash")
  )
  check("an unresolved clash is red")(
    kinds(output(log =
      Vector("Foo is a class and a package on the classpath, which the compiler cannot read together")
    )) == Vector("clash")
  )
  check("a problem line is a problem")(
    kinds(output(err = Vector("problem: class file a.B could not be read: boom"))) == Vector("problem")
  )
  check("the unreadable phrases are problems")(
    Vector("x could not be analysed", "y cannot be unpickled", "z could not be read").forall(l =>
      kinds(output(log = Vector(l))) == Vector("problem")
    )
  )
  check("a failed analysis is reported")(
    kinds(output(status = "failed", log = Vector(banner, "[error] bonsai analysis failed"))) ==
      Vector("analysis-failed")
  )
  check("a build that fails on its own is not bonsai's")(
    kinds(output(
      status = "failed",
      log = Vector(
        "[error] Foo.scala:3:4: Not found: bar",
        "java.lang.StackOverflowError",
        "\tat dotty.tools.dotc.typer.Typer.typed"
      )
    )).isEmpty
  )
  check("a clash is reported")(
    kinds(output(log =
      Vector("Foo is a class in a and a package in b, which the compiler cannot read together, and a, b need both")
    )) == Vector("clash")
  )
  check("a bonsai frame is a crash")(
    kinds(output(log =
      Vector(
        "java.lang.AssertionError: boom",
        "\tat scala.runtime.X.f(X.scala:1)",
        "\tat net.kurobako.bonsai.analysis.Extraction$.read(Extraction.scala:1)"
      )
    )) == Vector("crash")
  )
  check("the crash evidence names the exception")(
    Health.assess(output(log =
      Vector("java.lang.AssertionError: boom", "\tat net.kurobako.bonsai.analysis.X.f(X.scala:1)")
    )).head.evidence.head ==
      "java.lang.AssertionError: boom"
  )
  check("a nonzero analysis exit is a crash")(kinds(output(runs = Vector("m.tsv" -> 1))) == Vector("crash"))
  check("a zero analysis exit is not")(kinds(output(runs = Vector("m.tsv" -> 0))).isEmpty)
  check("an analysis that fails on its findings is healthy")(
    kinds(output(
      status = "failed",
      log = Vector(banner, "error\tcomplexity\thandwritten\tm\tA\tA.scala:1\tcubic\tk", "[error] bonsai analysis failed"),
      runs = Vector("m.tsv" -> 1),
      failing = Set("m.tsv")
    )).isEmpty
  )
  check("a failing finding of another run explains no exit")(
    kinds(output(
      status = "failed",
      log = Vector(banner, "[error] bonsai analysis failed"),
      runs = Vector("m.tsv" -> 1, "n.tsv" -> 1),
      failing = Set("m.tsv")
    )) == Vector("analysis-failed", "crash")
  )
  check("an uncaught exception after the banner is a crash")(
    kinds(output(log = Vector(banner, "Exception in thread \"main\" java.lang.OutOfMemoryError: Java heap space"))) ==
      Vector("crash")
  )
  check("an uncaught exception before the banner is not")(
    kinds(output(status = "failed", log = Vector("Exception in thread \"main\" java.lang.Error", banner))).isEmpty
  )
  check("a JVM crash of the analysis is a crash")(
    kinds(output(status = "failed", log = Vector(banner), crash = Vector("== /tmp/hs_err_pid1.log"))) == Vector("crash")
  )
  check("a JVM crash before the analysis is not")(
    kinds(output(status = "failed", log = Vector("compiling"), crash = Vector("== /tmp/hs_err_pid1.log"))).isEmpty
  )
  check("a timeout in the analysis is reported")(
    kinds(output(status = "timeout", log = Vector("compiling", banner))) == Vector("stopped")
  )
  check("an oom in the analysis is reported")(kinds(output(status = "oom", log = Vector(banner))) == Vector("stopped"))
  check("a kill in the analysis is reported")(kinds(output(status = "killed", log = Vector(banner))) ==
    Vector("stopped"))
  check("a timeout in the build is not")(kinds(output(status = "timeout", log = Vector("compiling"))).isEmpty)
  check("an oom in the build is not")(kinds(output(status = "oom", log = Vector("compiling"))).isEmpty)
  check("the analysis banner may come from the second run")(
    kinds(output(status = "timeout", err = Vector(banner))) == Vector("stopped")
  )

  def row(now: String, before: Option[String]) =
    Row(output(status = now), Vector.empty, before.map(Before(_, Vector.empty)))
  check("ok to failed regresses")(row("failed", Some("ok")).regressed)
  check("ok to not-run regresses")(row("not-run", Some("ok")).regressed)
  check("ok to missing regresses")(row("missing", Some("ok")).regressed)
  check("failed to failed does not")(!row("failed", Some("failed")).regressed)
  check("failed to ok does not")(!row("ok", Some("failed")).regressed)
  check("a new project does not")(!row("failed", None).regressed)
  check("ok to ok is not red")(!row("ok", Some("ok")).red)
  check("a failure makes a row red")(Row(output(), Vector(Failure("problem", Vector("x"))), None).red)

  val long =
    rows(Iterator(output(status = "failed", log = Vector("problem: x") ++ Vector.fill(50)("tail"))), Map.empty).head
  check("rows judge the whole log and keep its tail")(long.failures.map(_.kind) == Vector("problem") &&
    long.output.log.size == 20)

  check("the summary lists missing classes by project")(
    render(Vector(Row(output(name = "m"), Vector.empty, None, Vector(undefined))), Vector.empty, None, Int.MaxValue)
      .contains("### m\n\n```text\n" + undefined)
  )

  check("changes see added and removed lines")(
    changes(Vector("a", "b", "c"), Vector("b", "c", "d")) == (Vector("a"), Vector("d"))
  )
  check("changes count duplicates")(changes(Vector("a", "a"), Vector("a")) == (Vector("a"), Vector.empty))
  check("equal findings change nothing")(changes(Vector("a"), Vector("a")) == (Vector.empty, Vector.empty))

  val artifacts =
    """{"artifacts":[
      |{"name":"community-baseline","expired":false,"created_at":"2026-11-10T10:00:00Z","workflow_run":{"id":1,"head_branch":"main"}},
      |{"name":"community-baseline","expired":false,"created_at":"2026-11-11T10:00:00Z","workflow_run":{"id":3,"head_branch":"main"}},
      |{"name":"community-baseline","expired":false,"created_at":"2026-11-12T10:00:00Z","workflow_run":{"id":4,"head_branch":"topic"}},
      |{"name":"community-baseline","expired":true,"created_at":"2026-11-13T10:00:00Z","workflow_run":{"id":5,"head_branch":"main"}},
      |{"name":"community-baseline","expired":false,"created_at":"2026-11-14T10:00:00Z","workflow_run":{"id":6,"head_branch":"main"}}
      |]}""".stripMargin
  check("the newest unexpired main baseline is chosen")(latestBaseline(artifacts, "main", "") == Some("6"))
  check("the running run is skipped")(latestBaseline(artifacts, "main", "6") == Some("3"))
  check("a branch with no baseline has none")(latestBaseline(artifacts, "other", "").isEmpty)
  check("no artifacts, no baseline")(latestBaseline("""{"artifacts":[]}""", "main", "").isEmpty)

  val root                                  = Files.createTempDirectory("community-test")
  def put(path: String, text: String): Unit = {
    val file = root.resolve(path)
    Files.createDirectories(file.getParent)
    Files.writeString(file, text, UTF_8)
  }
  put("out/a/status", "ok\n")
  put("out/a/build.log", s"\u001B[0m$banner\nnote: undefined: x\n")
  put("out/a/timings.tsv", "clone\t1.5\nbuild\t10.0\n")
  put("out/a/info/runs.tsv", "m.tsv\t0\t3.2\tpeak_kb=1\n")
  put("out/a/info/m.tsv.err", "problem: y could not be read\n")
  put("out/a/info/m.manifest.tsv.out", "warning\tb\nwarning\ta\n")
  put("out/a/info/runs.tsv", "m.tsv\t0\t3.2\tpeak_kb=1\nn.manifest.tsv\t1\t1.0\tpeak_kb=1\n")
  put("out/a/info/n.manifest.tsv.out", "warning\tc\nerror\td\n")
  put("out/b/status", "runner-error:125\n")
  val a = Output.read(root.resolve("out/a"))
  check("a status is read")(a.status == "ok" && Output.read(root.resolve("out/b")).status == "runner-error")
  check("an unreadable directory has no status")(Output.read(root.resolve("out/none")).status == "missing")
  check("ansi codes are dropped")(a.log.head == banner)
  check("the analysis exit is read")(a.runs == Vector("m.tsv" -> 0, "n.manifest.tsv" -> 1))
  check("a run that printed a failing finding is read")(a.failing == Set("n.manifest.tsv"))
  check("findings are read sorted")(a.findings == Vector("error\td", "warning\ta", "warning\tb", "warning\tc"))
  check("both logs are searched")(Health.assess(a).head.evidence.size == 2)

  writeBaseline(Vector(a), root.resolve("out"), root.resolve("base"))
  val read = readPrevious(root.resolve("base"))
  check("a baseline reads back")(read.get("a").contains(Before("ok", a.findings)))
  check("timings travel with it")(seconds(Files.readString(root.resolve("base/a/timings.tsv"))) == 11.5)
  check("a missing baseline is empty")(readPrevious(root.resolve("nowhere")).isEmpty)

  val good = Row(output(name = "g"), Vector.empty, Some(Before("ok", Vector("x"))))
  val bad  = Row(
    output(name = "b", status = "failed", log = Vector("compile error")),
    Vector.empty,
    Some(Before("ok", Vector.empty))
  )
  val sick =
    Row(output(name = "s", findings = Vector("x", "y")), Vector(Failure("problem", Vector("problem: z"))), None)
  val diffs = findingChanges(Vector(good.copy(output = good.output.copy(findings = Vector("x", "w"))), bad, sick))
  check("changes list only the changed projects")(diffs.map(_.name) == Vector("g"))
  val text = render(Vector(good, bad, sick), diffs, Some("7"), Int.MaxValue)
  check("a red run says so")(text.contains("Red: 1 projects with a bonsai health failure and 1 regressions"))
  check("a regression is listed with its log")(text.contains("### b") && text.contains("compile error"))
  check("a failure is listed with its evidence")(text.contains("problem: z"))
  check("the summary is ASCII")(text.forall(_ < 128))
  check("a green run says so")(render(
    Vector(good),
    Vector.empty,
    Some("7"),
    Int.MaxValue
  ).startsWith("# Community build\n\nGreen"))
  check("no previous run is said")(render(
    Vector(sick),
    Vector.empty,
    None,
    Int.MaxValue
  ).contains("No previous run to compare with"))
  check("the summary is cut to its budget")(render(Vector(good, bad, sick), diffs, Some("7"), 400).length < 600)

  check("names derive from urls")(derivedName("https://github.com/scala/scala3.git") == "scala__scala3")
  check("a list reads as an array or as lines")(
    parseProjects("""[{"url":"a"},{"url":"b"}]""") == parseProjects("{\"url\":\"a\"}\n{\"url\":\"b\"}\n")
  )
  val project = """{"name":"n","url":"https://x/y/z","ref":"r","java":17,"sbt":[]}"""
  check("a good project passes")(projectErrors(parseProjects(project)).isEmpty)
  check("a bad java is caught")(projectErrors(parseProjects(project.replace("17", "9"))).nonEmpty)
  check("a missing url is caught")(projectErrors(parseProjects("""{"ref":"r","java":17}""")).nonEmpty)
  check("a name used twice is caught")(
    projectErrors(parseProjects(s"$project\n$project")).exists(_.contains("2 times"))
  )

  check("without timings the shards go by index")(spread(Vector("a", "b", "c", "d", "e"), 2, Map.empty) ==
    Vector(0, 1, 0, 1, 0))
  check("timings balance the shards")(
    packed(Vector(100.0, 90.0, 80.0, 10.0, 10.0, 10.0), 2) == Vector(0, 1, 1, 0, 0, 0)
  )
  check("known timings spread a project the previous run lacked")(
    spread(Vector("a", "b", "c"), 2, Map("a" -> 1000.0, "b" -> 10.0)).distinct.size == 2
  )
  check("timings add up")(seconds("clone\t1.0\nbuild\t2.5\n") == 3.5)

  if (failed.nonEmpty) {
    failed.foreach(name => System.err.println(s"FAILED: $name"))
    sys.exit(1)
  }
  println("community tests passed")
}
