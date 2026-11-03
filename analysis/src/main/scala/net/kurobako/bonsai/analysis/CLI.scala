package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.ExecutionException
import java.util.concurrent.FutureTask
import java.util.concurrent.TimeUnit
import scala.util.Using

object CLI {
  private val usage =
    "usage: bonsai-analysis --manifest <file> [--generated <glob>]... [--info] [--purity] [--mode dev|ci] " +
      "[--disable <rule>]... [--generated-disable <rule>|*]... [--error <rule>]... [--warn <rule>]... " +
      "[--baseline <file>] [--write-baseline <file>] " +
      "[--purity-lock <file>] [--write-purity-lock <file>] [--effects <prefix>=<kind>,...]... [--concurrent <method>]... " +
      "[--reflective <glob>]... [--hot <glob>]... [--intended <glob>=<glob>]... " +
      "[--bounded <member> <max> <reason>]... " +
      "[--nesting-depth <levels>] [--nesting-damping <users>] " +
      "[--nesting] [--nesting-size <lines>] [--parallelism <threads>]"

  /** The ci mode fails on the warnings of strict rules. */
  enum Mode(val id: String) {
    case Dev extends Mode("dev")
    case CI  extends Mode("ci")
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
      warnings: Set[String] = Set.empty,
      mode: Mode = Mode.Dev,
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

    /** The rules whose warnings fail: those named, every rule with `*`, and the strict rules in the ci mode, except
      * those kept as warnings.
      */
    def failing: Set[String] = {
      val strict = if (mode == Mode.CI) Finding.Rule.values.view.collect { case rule if rule.strict => rule.id }.toSet
      else Set.empty
      (if (errors("*")) rules else errors) ++ strict -- warnings
    }
  }

  private val rules         = Finding.Rule.values.map(_.id).toSet
  private val ruleFlags     = Set("--disable", "--generated-disable", "--warn", "--error")
  private val wildcardFlags = Set("--generated-disable", "--error")

  private def disable(rule: String, scope: Report.Scope): Report.Disable =
    Report.Disable(if (rule == "*") Report.Selector.All else Report.Selector.Named(rule), scope)

  def main(args: Array[String]): Unit = {
    val status = run(args.toList)
    if (status != 0) sys.exit(status)
  }

  /** Runs the analysis, printing the report to the console, and returns the exit status. */
  def run(args: List[String]): Int = {
    val result = parse(args).flatMap(options =>
      Using.resource(Parallel(options.parallelism))(parallel => analyse(args, options, parallel))
    )
    result match {
      case Left(error) =>
        Console.err.println(error)
        1
      case Right(outcome) =>
        outcome.record.diagnostics.foreach(Console.err.println(_))
        outcome.scanProblem.foreach(problem => Console.err.println(s"problem: $problem"))
        outcome.record.report.foreach(shown => println(shown.text))
        if (outcome.reused) Console.err.println(Reuse.Marker)
        if (outcome.record.report.exists(_.failing)) 1 else 0
    }
  }

  private case class Outcome(record: Reuse.Record, scanProblem: Option[String], reused: Boolean)

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
    ).left.map(_.mkString("\n")).map { result =>
      val locked = options.lock.filter(Files.exists(_)).fold(Set.empty[String])(file =>
        Report.keys(Files.readString(file))
      )
      val (scannedTexts, excluded) = scanned()
      val texts                    = scannedTexts.getOrElse(Vector.empty)
      val findings                 = Report.findings(result, Intended(options.intended), options.nesting, parallel) ++
        Build.duplicates(manifest) ++
        Build.unused(result.unusedBounds) ++ texts ++
        excluded ++
        (if (options.mode == Mode.CI) Build.fatal(manifest) else Vector.empty) ++
        Purity.regressions(result.purity, result.graph.nodes, locked) ++
        Purity.violations(result.purity, result.graph.nodes, options.policies)
      options.relock.foreach(file => Files.writeString(file, Purity.lock(result.purity, result.graph.nodes)): Unit)
      val baseline = options.baseline.filter(Files.exists(_)).fold(Set.empty[String])(file =>
        Report.keys(Files.readString(file))
      )
      val report = Report.Options(options.disabled, options.failing, baseline, options.info, options.warnings)
      options.record.foreach(file =>
        Files.writeString(file, Report.write(Report(findings, result.graph.nodes, report.copy(info = false)))): Unit
      )
      Reuse.Record(
        result.problems.map(problem => s"problem: $problem") ++
          (if (options.purity) Purity.summary(result.purity) else Vector.empty),
        Report(findings, result.graph.nodes, report).map(line => Reuse.Shown(line.failing, line.show))
      )
    }

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

  @scala.annotation.tailrec
  private def parse(args: List[String], options: Options): Either[String, Options] = args match {
    case Nil                                   => Right(options)
    case "--manifest" :: path :: rest          => parse(rest, options.copy(manifest = Some(Path.of(path))))
    case "--info" :: rest                      => parse(rest, options.copy(info = true))
    case "--purity" :: rest                    => parse(rest, options.copy(purity = true))
    case "--baseline" :: path :: rest          => parse(rest, options.copy(baseline = Some(Path.of(path))))
    case "--write-baseline" :: path :: rest    => parse(rest, options.copy(record = Some(Path.of(path))))
    case "--purity-lock" :: path :: rest       => parse(rest, options.copy(lock = Some(Path.of(path))))
    case "--write-purity-lock" :: path :: rest => parse(rest, options.copy(relock = Some(Path.of(path))))
    case "--concurrent" :: method :: rest      => parse(rest, options.copy(concurrent = options.concurrent + method))
    case "--reflective" :: glob :: rest        => parse(rest, options.copy(reflective = options.reflective :+ glob))
    case "--hot" :: glob :: rest               => parse(rest, options.copy(hot = options.hot :+ glob))
    case "--bounded" :: member :: max :: reason :: rest =>
      bounded(options, member, max, reason) match {
        case Left(error) => Left(error)
        case Right(next) => parse(rest, next)
      }
    case "--nesting" :: rest => parse(rest, options.copy(nesting = options.nesting.copy(enabled = true)))
    case "--nesting-depth" :: levels :: rest =>
      positive("nesting depth", levels, 1) match {
        case Left(error)  => Left(error)
        case Right(depth) => parse(rest, options.copy(nesting = options.nesting.copy(depth = depth)))
      }
    case "--nesting-damping" :: users :: rest =>
      positive("nesting damping", users, 0) match {
        case Left(error)    => Left(error)
        case Right(damping) => parse(rest, options.copy(nesting = options.nesting.copy(damping = damping)))
      }
    case "--parallelism" :: threads :: rest =>
      positive("parallelism", threads, 1) match {
        case Left(error)  => Left(error)
        case Right(count) => parse(rest, options.copy(parallelism = count))
      }
    case "--nesting-size" :: lines :: rest =>
      positive("nesting size", lines, 1) match {
        case Left(error) => Left(error)
        case Right(size) => parse(rest, options.copy(nesting = options.nesting.copy(size = size)))
      }
    case "--intended" :: pair :: rest =>
      intended(pair) match {
        case Left(error)   => Left(s"$error\n$usage")
        case Right(parsed) => parse(rest, options.copy(intended = options.intended :+ parsed))
      }
    case "--effects" :: policy :: rest =>
      Purity.Policy.parse(policy) match {
        case Left(error)   => Left(s"$error\n$usage")
        case Right(parsed) => parse(rest, options.copy(policies = options.policies :+ parsed))
      }
    case "--generated" :: pattern :: rest =>
      parse(rest, options.copy(generated = options.generated :+ s"glob:$pattern"))
    case "--mode" :: mode :: _ if !Mode.values.exists(_.id == mode) => Left(s"unknown mode: $mode\n$usage")
    case "--mode" :: mode :: rest => parse(rest, options.copy(mode = Mode.values.find(_.id == mode).get))
    case flag :: rule :: _ if ruleFlags(flag) && !(wildcardFlags(flag) && rule == "*") && !rules(rule) =>
      Left(s"unknown rule: $rule\n$usage")
    case "--disable" :: rule :: rest =>
      parse(rest, options.copy(disabled = options.disabled + disable(rule, Report.Scope.Everywhere)))
    case "--generated-disable" :: rule :: rest =>
      parse(rest, options.copy(disabled = options.disabled + disable(rule, Report.Scope.Generated)))
    case "--warn" :: rule :: rest  => parse(rest, options.copy(warnings = options.warnings + rule))
    case "--error" :: rule :: rest => parse(rest, options.copy(errors = options.errors + rule))
    case other :: _                => Left(s"unknown argument: $other\n$usage")
  }
}
