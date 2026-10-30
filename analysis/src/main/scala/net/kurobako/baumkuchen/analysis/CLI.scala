package net.kurobako.baumkuchen.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.ExecutionException
import java.util.concurrent.FutureTask
import java.util.concurrent.TimeUnit

object CLI {
  private val usage =
    "usage: baumkuchen-analysis --manifest <file> [--generated <glob>]... [--info] [--purity] [--mode dev|ci] " +
      "[--disable <rule>]... [--error <rule>]... [--warn <rule>]... [--baseline <file>] [--write-baseline <file>] " +
      "[--purity-lock <file>] [--write-purity-lock <file>] [--effects <prefix>=<kind>,...]... [--concurrent <method>]... " +
      "[--reflective <glob>]... [--hot <glob>]... [--intended <glob>=<glob>]... " +
      "[--bounded <member> <max> <reason>]... [--nesting-depth <levels>] [--nesting-damping <users>] " +
      "[--nesting] [--nesting-size <lines>]"

  /** The ci mode fails on the warnings of strict rules. */
  enum Mode(val id: String) {
    case Dev extends Mode("dev")
    case CI  extends Mode("ci")
  }

  // The only constructions are the defaults and a test setting every field, which reads as the flags moving together.
  /** `record` names the file `--write-baseline` writes the reported warnings to, and `relock` the file
    * `--write-purity-lock` writes the pure methods to.
    */
  @SuppressWarnings(Array("baumkuchen:redundant-flag"))
  case class Options(
      manifest: Option[Path] = None,
      generated: Vector[String] = Vector.empty,
      info: Boolean = false,
      disabled: Set[String] = Set.empty,
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
      nesting: Nesting.Config = Nesting.Config()
  ) {

    /** The rules whose warnings fail: those named, every rule with `*`, and the strict rules in the ci mode, except
      * those kept as warnings.
      */
    def failing: Set[String] = {
      val strict = if (mode == Mode.CI) Finding.Rule.values.filter(_.strict).map(_.id).toSet else Set.empty
      (if (errors("*")) rules else errors) ++ strict -- warnings
    }
  }

  private val rules = Finding.Rule.values.map(_.id).toSet

  def main(args: Array[String]): Unit = {
    val result = for {
      options  <- parse(args.toList)
      path     <- options.manifest.toRight(usage)
      manifest <- Manifest.read(path)
      scanned = scanning(manifest)
      result <-
        Extraction.read(
          manifest,
          options.generated,
          options.concurrent,
          options.reflective,
          options.hot,
          options.bounded
        ).left.map(_.mkString("\n"))
    } yield {
      result.problems.foreach(problem => Console.err.println(s"problem: $problem"))
      if (options.purity) Purity.summary(result.purity).foreach(Console.err.println(_))
      val locked = options.lock.filter(Files.exists(_)).fold(Set.empty[String])(file =>
        Purity.locked(Files.readString(file))
      )
      val (scannedTexts, excluded) = scanned()
      val texts                    = scannedTexts.fold(
        problem => {
          Console.err.println(s"problem: $problem")
          Vector.empty
        },
        identity
      )
      val findings = Report.findings(result, Intended(options.intended), options.nesting) ++ Build.duplicates(manifest) ++
        Build.unused(result.unusedBounds) ++ texts ++
        excluded ++
        (if (options.mode == Mode.CI) Build.fatal(manifest) else Vector.empty) ++
        Purity.regressions(result.purity, result.graph.nodes, locked) ++
        Purity.violations(result.purity, result.graph.nodes, options.policies)
      options.relock.foreach(file => Files.writeString(file, Purity.lock(result.purity, result.graph.nodes)): Unit)
      val baseline = options.baseline.filter(Files.exists(_)).fold(Set.empty[String])(file =>
        Report.baseline(Files.readString(file))
      )
      val report = Report.Options(options.disabled, options.failing, baseline, options.info, options.warnings)
      options.record.foreach(file =>
        Files.writeString(file, Report.write(Report(findings, result.graph.nodes, report.copy(info = false)))): Unit
      )
      Report(findings, result.graph.nodes, report)
    }
    result match {
      case Left(error) =>
        Console.err.println(error)
        sys.exit(1)
      case Right(lines) =>
        lines.foreach(line => println(line.show))
        if (lines.exists(_.failing)) sys.exit(1)
    }
  }

  // The sources are read apart from the build's classes, so the scan runs while those are extracted.
  private def scanning(manifest: Manifest): () => (Either[String, Vector[Finding]], Vector[Finding]) =
    background(() => scan(manifest))

  /** Runs `work` on a thread of its own, and returns what joins it: the result, or whatever `work` threw. */
  def background[A](work: () => A): () => A = {
    val task   = FutureTask(() => work())
    val thread = Thread(task, "baumkuchen-background")
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
      max.toIntOption.filter(_ > 0) match {
        case None                           => Left(s"a bounded max is a positive integer, not $max\n$usage")
        case Some(_) if reason.trim.isEmpty => Left(s"a bounded entry for $member needs a reason\n$usage")
        case Some(limit)                    =>
          parse(rest, options.copy(bounded = options.bounded :+ Complexity.Bounded(member, limit, reason)))
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
    case "--nesting-size" :: lines :: rest =>
      positive("nesting size", lines, 1) match {
        case Left(error)  => Left(error)
        case Right(size) => parse(rest, options.copy(nesting = options.nesting.copy(size = size)))
      }
    case "--intended" :: pair :: rest =>
      Intended.parse(pair) match {
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
    case "--disable" :: rule :: _ if !rules(rule)              => Left(s"unknown rule: $rule\n$usage")
    case "--warn" :: rule :: _ if !rules(rule)                 => Left(s"unknown rule: $rule\n$usage")
    case "--error" :: rule :: _ if rule != "*" && !rules(rule) => Left(s"unknown rule: $rule\n$usage")
    case "--disable" :: rule :: rest => parse(rest, options.copy(disabled = options.disabled + rule))
    case "--warn" :: rule :: rest    => parse(rest, options.copy(warnings = options.warnings + rule))
    case "--error" :: rule :: rest   => parse(rest, options.copy(errors = options.errors + rule))
    case other :: _                  => Left(s"unknown argument: $other\n$usage")
  }
}
