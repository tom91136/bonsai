package net.kurobako.baumkuchen.analysis

import java.nio.file.Files
import java.nio.file.Path

object CLI {
  private val usage =
    "usage: baumkuchen-analysis --manifest <file> [--generated <glob>]... [--info] [--purity] [--mode dev|ci] " +
      "[--disable <rule>]... [--error <rule>]... [--warn <rule>]... [--baseline <file>] [--write-baseline <file>] " +
      "[--purity-lock <file>] [--write-purity-lock <file>] [--effects <prefix>=<kind>,...]... [--concurrent <method>]... " +
      "[--reflective <glob>]... [--hot <glob>]..."

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
      hot: Vector[String] = Vector.empty
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
      result   <-
        Extraction.read(
          manifest,
          options.generated,
          options.concurrent,
          options.reflective,
          options.hot
        ).left.map(_.mkString("\n"))
    } yield {
      result.problems.foreach(problem => Console.err.println(s"problem: $problem"))
      if (options.purity) Purity.summary(result.purity).foreach(Console.err.println(_))
      val locked = options.lock.filter(Files.exists(_)).fold(Set.empty[String])(file =>
        Purity.locked(Files.readString(file))
      )
      val texts = Texts.rules(manifest.texts).flatMap(rules =>
        manifest.root.toRight("the manifest names no root for the text rules").flatMap(root =>
          Texts.findings(root, Texts.matchers(manifest.textExclusions), rules)
        )
      ).fold(
        problem => {
          Console.err.println(s"problem: $problem")
          Vector.empty
        },
        identity
      )
      val findings = Report.findings(result) ++ Build.duplicates(manifest) ++ texts ++
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

  def parse(args: List[String]): Either[String, Options] = parse(args, Options())

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
    case "--effects" :: policy :: rest         =>
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
