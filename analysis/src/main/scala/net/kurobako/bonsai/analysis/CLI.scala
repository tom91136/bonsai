package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.ExecutionException
import java.util.concurrent.FutureTask
import java.util.concurrent.TimeUnit
import scala.util.Using

object CLI {
  private val usage =
    "usage: bonsai-analysis --manifest <file> [--generated <glob>]... [--info] [--purity] " +
      "[--mode relaxed|strict|audit] " +
      "[--disable <rule>]... [--generated-disable <rule>|*]... [--error <rule>]... [--warn <rule>]... " +
      "[--baseline <file>] [--write-baseline <file>] " +
      "[--purity-lock <file>] [--write-purity-lock <file>] [--effects <prefix>=<kind>,...]... [--concurrent <method>]... " +
      "[--reflective <glob>]... [--hot <glob>]... [--intended <glob>=<glob>]... " +
      "[--bounded <member> <max> <reason>]... " +
      "[--nesting-depth <levels>] [--nesting-damping <users>] " +
      "[--nesting] [--nesting-size <lines>] [--parallelism <threads>]"

  /** The strict mode fails on the warnings of fatal rules. The audit mode judges as the strict mode does, and adds the
    * findings the compiler plugin recorded, which the compiler did not fail on.
    */
  enum Mode(val id: String) {
    case Relaxed extends Mode("relaxed")
    case Strict  extends Mode("strict")
    case Audit   extends Mode("audit")

    def strict: Boolean = this != Relaxed
  }

  // The only constructions are the defaults and a test setting every field, which reads as the flags moving together.
  /** `record` names the file `--write-baseline` writes the reported warnings to, and `relock` the file
    * `--write-purity-lock` writes the pure methods to. `parallelism` is the number of threads the stages that touch no
    * compiler state may use, which is the parallelism of the build that runs the analysis.
    */
  @SuppressWarnings(Array("bonsai:redundant-flag"))
  case class Options(
      manifest: Option[Path] = None,
      generated: Vector[String] = Vector.empty,
      info: Boolean = false,
      disabled: Set[Report.Disable] = Set.empty,
      errors: Set[String] = Set.empty,
      nonfatal: Set[String] = Set.empty,
      mode: Mode = Mode.Relaxed,
      baseline: Option[Path] = None,
      record: Option[Path] = None,
      purity: Boolean = false,
      lock: Option[Path] = None,
      relock: Option[Path] = None,
      policies: Vector[Purity.Policy] = Vector.empty,
      concurrent: Set[String] = Set.empty,
      reflective: Vector[String] = Vector.empty,
      hot: Vector[String] = Vector.empty,
      intended: Vector[(String, String)] = Vector.empty,
      bounded: Vector[Complexity.Bounded] = Vector.empty,
      nesting: Nesting.Config = Nesting.Config(),
      parallelism: Int = 1
  ) {

    /** The rules whose warnings fail: those named, every rule with `*`, and the fatal rules in the strict mode, except
      * those that are nonfatal.
      */
    def failing: Set[String] = {
      val fatal = if (mode.strict) Finding.Rule.values.view.collect { case rule if rule.fatal => rule.id }.toSet
      else Set.empty
      (if (errors("*")) rules else errors) ++ fatal -- nonfatal
    }
  }

  private val rules         = Finding.Rule.values.map(_.id).toSet
  private val ruleFlags     = Set("--disable", "--generated-disable", "--warn", "--error")
  private val wildcardFlags = Set("--generated-disable", "--error")

  private def disable(rule: String, scope: Report.Scope): Report.Disable =
    Report.Disable(if (rule == "*") Report.Selector.All else Report.Selector.Named(rule), scope)

  private def banner(options: Options): String = {
    val active = Finding.Rule.values.count { rule =>
      (rule != Finding.Rule.Nesting || options.nesting.enabled) &&
      !options.disabled.exists(disable => disable.scope == Report.Scope.Everywhere && disable.selector.covers(rule.id))
    }
    s"bonsai: ${options.mode.id} mode, $active of ${Finding.Rule.values.size} analysis rules active"
  }

  def main(args: Array[String]): Unit = {
    val status = run(args.toList)
    if (status != 0) sys.exit(status)
  }

  /** Runs the analysis, printing the report to the console, and returns the exit status. */
  def run(args: List[String]): Int = {
    val result = parse(args).flatMap { options =>
      Console.err.println(banner(options))
      Using.resource(Parallel(options.parallelism))(parallel => analyse(args, options, parallel)).map(options.mode -> _)
    }
    result match {
      case Left(error) =>
        Console.err.println(error)
        1
      case Right((mode, outcome)) =>
        outcome.record.diagnostics.foreach(Console.err.println(_))
        outcome.scanProblem.foreach(problem => Console.err.println(s"${Reuse.Problem}$problem"))
        outcome.record.report.foreach(shown => println(shown.text))
        if (outcome.reused) Console.err.println(Reuse.Marker)
        if (outcome.record.report.exists(_.failing) || mode.strict && outcome.problematic) 1 else 0
    }
  }

  private case class Outcome(record: Reuse.Record, scanProblem: Option[String], reused: Boolean) {
    def problematic: Boolean = scanProblem.isDefined || record.problems.nonEmpty
  }

  private type Scan = () => (Either[String, Vector[Finding]], Vector[Finding])

  private def analyse(args: List[String], options: Options, parallel: Parallel): Either[String, Outcome] =
    for {
      path     <- options.manifest.toRight(usage)
      manifest <- Manifest.read(path)
      scanned = scanning(manifest)
      keyed   = Option.when(options.record.isEmpty && options.relock.isEmpty)(
        Reuse.store(path) -> Reuse.key(
          args,
          path,
          manifest,
          (options.baseline ++ options.lock).toVector,
          parallel,
          () => Reuse.rendered(scanned()._1, scanned()._2)
        )
      )
      kept = keyed.flatMap(Reuse.load(_, _))
      record <- kept.toRight(()).left.flatMap(_ => extract(manifest, options, parallel, scanned))
    } yield {
      if (kept.isEmpty) keyed.foreach(Reuse.save(_, _, record))
      Outcome(record, scanned()._1.left.toOption, kept.isDefined)
    }

  private def extract(
      manifest: Manifest,
      options: Options,
      parallel: Parallel,
      scanned: Scan
  ): Either[String, Reuse.Record] =
    Extraction.read(
      manifest,
      options.generated,
      options.concurrent,
      options.reflective,
      options.hot,
      options.bounded,
      parallel
    ).left.map(_.mkString("\n")).flatMap(result => recorded(manifest, options).map(result -> _)).map {
      (result, compiled) =>
        val locked = options.lock.filter(Files.exists(_)).fold(Set.empty[String])(file =>
          Report.keys(Files.readString(file))
        )
        val (scannedTexts, excluded) = scanned()
        val texts                    = scannedTexts.getOrElse(Vector.empty)
        val findings                 =
          (Report.findings(result, Intended(options.intended), options.nesting, parallel) ++
            compiled ++
            Build.duplicates(manifest) ++
            Build.unused(result.unusedBounds) ++ texts ++
            excluded ++
            (if (options.mode.strict) Build.fatal(manifest) else Vector.empty) ++
            Purity.regressions(result.purity, result.graph.nodes, locked) ++
            Purity.violations(result.purity, result.graph.nodes, options.policies)).map(relocated(manifest, _))
        options.relock.foreach(file => Files.writeString(file, Purity.lock(result.purity, result.graph.nodes)): Unit)
        val baseline = options.baseline.filter(Files.exists(_)).fold(Set.empty[String])(file =>
          Report.keys(Files.readString(file))
        )
        val report = Report.Options(options.disabled, options.failing, baseline, options.info, options.nonfatal)
        options.record.foreach { file =>
          val previous = if (Files.exists(file)) Files.readString(file) else ""
          val nodes    = findings.map(finding => finding.node.name -> finding.node).toMap ++ result.graph.nodes
          val lines    = Report(findings, result.graph.nodes, report.copy(info = false))
          Files.writeString(file, Report.merge(previous, lines, nodes, manifest.outputs.map(_.module).toSet)): Unit
        }
        Reuse.Record(
          result.status ++
            result.problems.map(problem => s"${Reuse.Problem}$problem") ++
            result.notes.map(note => s"${Reuse.Note}$note") ++
            (if (options.purity) Purity.summary(result.purity) else Vector.empty),
          Report(findings, result.graph.nodes, report).map(line => Reuse.Shown(line.failing, line.show))
        )
    }

  private def relocated(manifest: Manifest, finding: Finding): Finding =
    finding.copy(node = finding.node.copy(source = manifest.relative(Path.of(finding.node.source)).toString))

  private def recorded(manifest: Manifest, options: Options): Either[String, Vector[Finding]] =
    if (options.mode == Mode.Audit) Compiled.findings(manifest).left.map(_.mkString("\n")) else Right(Vector.empty)

  // The sources are read apart from the build's classes, so the scan runs while those are hashed or extracted.
  private def scanning(manifest: Manifest): Scan =
    background(() => scan(manifest))

  /** Runs `work` on a thread of its own, and returns what joins it: the result, or whatever `work` threw. */
  def background[A](work: () => A): () => A = {
    val task   = FutureTask(() => work())
    val thread = Thread(task, "bonsai-background")
    thread.setDaemon(true)
    thread.start()
    () => {
      thread.join()
      // the thread has finished, so the task is complete and the wait cannot time out
      try task.get(0, TimeUnit.NANOSECONDS)
      catch { case failed: ExecutionException => throw failed.getCause }
    }
  }

  private def scan(manifest: Manifest): (Either[String, Vector[Finding]], Vector[Finding]) = (
    Texts.rules(manifest.texts).flatMap(rules =>
      manifest.root.toRight("the manifest names no root for the text rules").flatMap(root =>
        Texts.findings(root, Texts.matchers(manifest.textExclusions), rules)
      )
    ),
    manifest.root.fold(Vector.empty)(
      Texts.excludedSources(
        _,
        manifest.textExclusions ++ manifest.texts.values.flatMap(_.getOrElse("exclude", Vector.empty))
      )
    )
  )

  def parse(args: List[String]): Either[String, Options] = parse(args, Options())

  private def positive(name: String, text: String, least: Int): Either[String, Int] =
    text.toIntOption.filter(_ >= least).toRight(s"a $name is an integer of at least $least, not $text\n$usage")

  /** A pair as written, `<glob>=<glob>`. */
  private def intended(text: String): Either[String, (String, String)] = text.split("=", -1) match {
    case Array(left, right) if left.nonEmpty && right.nonEmpty => Right(left -> right)
    case _ => Left(s"an intended pair is written <glob>=<glob>, not $text")
  }

  private def bounded(options: Options, member: String, max: String, reason: String): Either[String, Options] =
    max.toIntOption.filter(_ > 0) match {
      case None                           => Left(s"a bounded max is a positive integer, not $max\n$usage")
      case Some(_) if reason.trim.isEmpty => Left(s"a bounded entry for $member needs a reason\n$usage")
      case Some(limit) => Right(options.copy(bounded = options.bounded :+ Complexity.Bounded(member, limit, reason)))
    }

  private type Step = Either[String, (List[String], Options)]

  private def inputArgs(options: Options): PartialFunction[List[String], Step] = {
    case "--manifest" :: path :: rest          => Right(rest -> options.copy(manifest = Some(Path.of(path))))
    case "--info" :: rest                      => Right(rest -> options.copy(info = true))
    case "--purity" :: rest                    => Right(rest -> options.copy(purity = true))
    case "--baseline" :: path :: rest          => Right(rest -> options.copy(baseline = Some(Path.of(path))))
    case "--write-baseline" :: path :: rest    => Right(rest -> options.copy(record = Some(Path.of(path))))
    case "--purity-lock" :: path :: rest       => Right(rest -> options.copy(lock = Some(Path.of(path))))
    case "--write-purity-lock" :: path :: rest => Right(rest -> options.copy(relock = Some(Path.of(path))))
  }

  private def analysisArgs(options: Options): PartialFunction[List[String], Step] = {
    case "--concurrent" :: method :: rest => Right(rest -> options.copy(concurrent = options.concurrent + method))
    case "--reflective" :: glob :: rest   => Right(rest -> options.copy(reflective = options.reflective :+ glob))
    case "--hot" :: glob :: rest          => Right(rest -> options.copy(hot = options.hot :+ glob))
    case "--generated" :: pattern :: rest => Right(rest -> options.copy(generated = options.generated :+ pattern))
    case "--bounded" :: member :: max :: reason :: rest =>
      bounded(options, member, max, reason) match {
        case Left(error) => Left(error)
        case Right(next) => Right(rest -> next)
      }
    case "--intended" :: pair :: rest =>
      intended(pair) match {
        case Left(error)   => Left(s"$error\n$usage")
        case Right(parsed) => Right(rest -> options.copy(intended = options.intended :+ parsed))
      }
    case "--effects" :: policy :: rest =>
      Purity.Policy.parse(policy) match {
        case Left(error)   => Left(s"$error\n$usage")
        case Right(parsed) => Right(rest -> options.copy(policies = options.policies :+ parsed))
      }
  }

  private def tuningArgs(options: Options): PartialFunction[List[String], Step] = {
    case "--nesting" :: rest => Right(rest -> options.copy(nesting = options.nesting.copy(enabled = true)))
    case "--nesting-depth" :: levels :: rest =>
      positive("nesting depth", levels, 1) match {
        case Left(error)  => Left(error)
        case Right(depth) => Right(rest -> options.copy(nesting = options.nesting.copy(depth = depth)))
      }
    case "--nesting-damping" :: users :: rest =>
      positive("nesting damping", users, 0) match {
        case Left(error)    => Left(error)
        case Right(damping) => Right(rest -> options.copy(nesting = options.nesting.copy(damping = damping)))
      }
    case "--nesting-size" :: lines :: rest =>
      positive("nesting size", lines, 1) match {
        case Left(error) => Left(error)
        case Right(size) => Right(rest -> options.copy(nesting = options.nesting.copy(size = size)))
      }
    case "--parallelism" :: threads :: rest =>
      positive("parallelism", threads, 1) match {
        case Left(error)  => Left(error)
        case Right(count) => Right(rest -> options.copy(parallelism = count))
      }
  }

  private def ruleArgs(options: Options): PartialFunction[List[String], Step] = {
    case "--mode" :: mode :: _ if !Mode.values.exists(_.id == mode) => Left(s"unknown mode: $mode\n$usage")
    case "--mode" :: mode :: rest => Right(rest -> options.copy(mode = Mode.values.find(_.id == mode).get))
    case flag :: rule :: _ if ruleFlags(flag) && !(wildcardFlags(flag) && rule == "*") && !rules(rule) =>
      Left(s"unknown rule: $rule\n$usage")
    case "--disable" :: rule :: rest =>
      Right(rest -> options.copy(disabled = options.disabled + disable(rule, Report.Scope.Everywhere)))
    case "--generated-disable" :: rule :: rest =>
      Right(rest -> options.copy(disabled = options.disabled + disable(rule, Report.Scope.Generated)))
    case "--warn" :: rule :: rest  => Right(rest -> options.copy(nonfatal = options.nonfatal + rule))
    case "--error" :: rule :: rest => Right(rest -> options.copy(errors = options.errors + rule))
  }

  @scala.annotation.tailrec
  private def parse(args: List[String], options: Options): Either[String, Options] = args match {
    case Nil        => Right(options)
    case other :: _ =>
      val step = inputArgs(options)
        .orElse(analysisArgs(options))
        .orElse(tuningArgs(options))
        .orElse(ruleArgs(options))
        .lift(args)
        .getOrElse(Left(s"unknown argument: $other\n$usage"))
      step match {
        case Left(error)         => Left(error)
        case Right((rest, next)) => parse(rest, next)
      }
  }
}
