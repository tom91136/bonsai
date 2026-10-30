package net.kurobako.baumkuchen.sbt

import com.typesafe.config.Config
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigUtil
import sbt.*
import sbt.Keys.*
import sbt.internal.util.MessageOnlyException
import sbt.librarymanagement.DependencyResolution
import scala.jdk.CollectionConverters.*
import scalafix.sbt.ScalafixPlugin
import scalafix.sbt.ScalafixPlugin.autoImport.*

/** Wires baumkuchen's scalafix rules, compiler plugin checks and whole-build analysis into every project, with defaults
  * that warn while developing and fail in CI.
  *
  * `.baumkuchen.conf` at the root of the build sets the defaults of the keys, for the build at its top level and for a
  * project under `projects.<id>`, so a build configured there alone still loads without the plugin:
  * {{{
  * disabled   = [PackageNaming]
  * errors     = [local-throw]
  * warnings   = []
  * wildcards  = 2
  * baseline   = baumkuchen-baseline.tsv
  * purity     = baumkuchen-purity.tsv
  * effects    { "demo.*.core" = [], "demo.io" = [input, output] }
  * concurrent = [demo.WorkerPool.parallelFor]
  * hot        = [demo.kernel.**, "!**.*Suite"]
  * reflective = [demo.rendered.*]
  * intended   = ["demo.*.Execute*=demo.*.Render*"]
  * checked    = [demo.constraint.where]
  * bounded    = [{ member = "demo.Encoding.operands", max = 4, reason = "an instruction has at most four operands" }]
  * nesting    { depth = 2, damping = 1, size = 200 }
  * projects.api { library = true }
  * }}}
  *
  * `bounded` lists members, by qualified name glob as for `hot`, whose collections hold at most `max` elements, so the
  * complexity rules count their loops as constant; a `reason` is required, and an entry that matches no collection is
  * reported as `unused-bound`.
  *
  * `nesting` tunes the `nesting` rule, which suggests the type or helper only one other type uses be nested in it, and
  * a nested type that uses nothing of its parent be moved out: `depth` is the deepest nesting a move may create, one
  * level less when it crosses files, `damping` the number of declarations of the new owner that must use what moves,
  * and `size` the most lines a moved type, or a group of helpers moving together, may have. The rule runs only when the
  * file has this block, and a key left out takes the analysis' default.
  *
  * `intended` lists pairs of qualified name globs, written `<glob>=<glob>` and matched as for `hot`, whose copies are
  * meant, such as the execute and render instances of one operation or unrelated bit layouts that look alike.
  * `duplicate-code` and `near-duplicate-code` drop a group of copies, and `duplicate-shape` a group of types, when
  * every two of its members, by the type holding the copy, are an intended pair in either order.
  *
  * `text` configures rules over every file git sees, whatever its language, which `baumkuchenAnalyse` checks. Globs
  * match the path from the build root with a leading slash, and `**` crosses directories; every rule takes `files`,
  * `exclude` and a `level` of `error`, the default, `warning` or `info`; and a line containing
  * `baumkuchen:ignore <rule id>` exempts itself and the line after it:
  * {{{
  * text {
  *   exclude  = ["**.min.js"]
  *   terms    = [{ words = [plan], files = ["**.{scala,md}"] }]
  *   patterns = [{ pattern = "\\bUINT64_C\\b", message = "write uint64_t{n}", files = ["**.cpp"] }]
  *   binary   = { level = warning }
  *   licences = { files = ["**.scala"] }
  *   preceded = [{ pattern = "^TEST_CASE\\(", message = "put a blank line before a test case" }]
  *   followed = [{ pattern = "^#include\\b", neutral = "^#", message = "put a blank line after the includes" }]
  * }
  * }}}
  * Their rule ids are `forbidden-term`, `forbidden-pattern`, `binary-file`, `licence-notice`, `blank-line-before` and
  * `blank-line-after`; `followed` blocks go on through lines its `pattern`, `continues` or `neutral` matches.
  * `licences` reports a licence notice written out in a source file, by default of any usual language, where the
  * licence belongs in a resource beside the sources; the terms rule leaves licence notices out of its search. A source
  * file that a text `exclude` names whole is reported as `excluded-source`.
  *
  * `@SuppressWarnings(Array("baumkuchen:<id>"))` on an enclosing definition suppresses a check or analysis rule, and
  * `"baumkuchen"` all of them. An expression takes it ascribed, as `expr: @SuppressWarnings(Array("baumkuchen:<id>"))`.
  * `@nowarn("msg=baumkuchen:<id>")` suppresses the compiler plugin's checks too, but the compiler sees no warning of an
  * analysis rule for it to suppress, and a build without baumkuchen sees none at all, so it reports the `@nowarn` under
  * `-Wunused:nowarn`. The `nowarn-suppression` check flags it.
  */
object BaumkuchenPlugin extends AutoPlugin {
  override def requires: Plugins      = ScalafixPlugin
  override def trigger: PluginTrigger = allRequirements

  object autoImport {
    enum BaumkuchenMode {
      case Dev, CI
    }

    val baumkuchenMode = settingKey[BaumkuchenMode](
      "Dev warns; CI fails on the strict checks and rules. Defaults to BAUMKUCHEN_MODE, else CI when CI is set"
    )
    val baumkuchenDisabled =
      settingKey[Set[String]]("Compiler plugin checks, analysis rules and scalafix rules left out, by id")
    val baumkuchenErrors =
      settingKey[Set[String]]("Compiler plugin checks and analysis rules that fail in every mode, by id")
    val baumkuchenWarnings =
      settingKey[Set[String]]("Compiler plugin checks, analysis rules and scalafix rules that never fail, by id")
    val baumkuchenWildcards = settingKey[Int]("Positional wildcards a case class pattern may carry")
    val baumkuchenBaseline  = settingKey[File]("Analysis findings recorded here report without failing")
    val baumkuchenLibrary   = taskKey[Boolean](
      "Whether other builds use this project's public API, so the analysis counts it as used. " +
        "Defaults to whether the project is published and has no main class"
    )
    val baumkuchenPurity  = settingKey[File]("Methods recorded here as pure fail the analysis once they are not")
    val baumkuchenEffects = settingKey[Map[String, Set[String]]](
      "Kinds of effects (reads, writes, input, output, random, unknown) the methods of a package or class may have, by " +
        "its qualified name, with * for any one name; the most specific entry decides"
    )
    val baumkuchenGenerated = settingKey[Set[String]](
      "Globs of sources the build generates, such as **/src/generated/**, which the analysis labels as generated; " +
        "every source is checked by default"
    )
    val baumkuchenConcurrent =
      settingKey[Set[String]]("Methods, by qualified name, that run the functions passed to them on other threads")
    val baumkuchenHot = settingKey[Set[String]](
      "Globs of the classes and objects whose code runs hot, by qualified name, with * for one name segment and ** " +
        "for any number, and ! before a glob leaving out what it matches; boxing-fold, megamorphic-call, and " +
        "invariant-in-loop on matches whose arms vary, report only in them, while unspecialised-function and " +
        "lambda-trait leave out classes a ! glob matches and hot ones respectively"
    )
    val baumkuchenReflective = settingKey[Set[String]](
      "Globs of the classes and objects, by qualified name as for baumkuchenHot, that code outside the sources names, " +
        "as text compiled later or reflection does; they and their public members are used"
    )
    val baumkuchenIntended = settingKey[Set[String]](
      "Pairs of globs written <glob>=<glob>, matched as for baumkuchenHot, of types whose copies or likeness are meant; " +
        "duplicate-code, near-duplicate-code and duplicate-shape leave out groups whose members are all such pairs"
    )
    val baumkuchenChecked = settingKey[Set[String]](
      "Refinement types, by qualified name, whose checked extensions and overloads a conversion can bypass, and whose " +
        "inferred refinements the compiler plugin reports"
    )
    val baumkuchenNesting = settingKey[Map[String, Int]](
      "Limits of the nesting rule by name: depth, damping and size; a limit left out takes the analysis' default"
    )
    val baumkuchenBounded = settingKey[Vector[(String, Int, String)]](
      "Members, as (qualified name glob matched as for baumkuchenHot, largest number of elements, reason), whose " +
        "collections hold at most that many elements, which the complexity rules count as constants"
    )
    val baumkuchenAnalysisHeap =
      settingKey[String]("The largest heap the forked analysis may take, as -Xmx takes it, such as 4g")
    val baumkuchenAnalyse       = taskKey[Unit]("Analyse this project and the projects it aggregates as one build")
    val baumkuchenBaselineWrite = taskKey[File]("Record the current analysis warnings in baumkuchenBaseline")
    val baumkuchenPurityWrite   = taskKey[File]("Record the methods of main code that are pure in baumkuchenPurity")
  }

  import autoImport.*

  private val Organization = BaumkuchenArtifacts.organization
  private val Version      = BaumkuchenArtifacts.version

  /** Every check the compiler plugin knows. */
  val Checks: Set[String] = BaumkuchenArtifacts.checks

  /** Every scalafix rule, in the order they run, with the built-in ones last. */
  val Rules: Seq[String] = BaumkuchenArtifacts.rules ++ Seq("DisableSyntax", "RedundantSyntax")

  /** The DisableSyntax bans on by default, each disabled by its own id. */
  val Bans: Seq[String] =
    Seq("noVars", "noReturns", "noNulls", "noAsInstanceOf", "noIsInstanceOf", "noXml", "noFinalize")
      .map("DisableSyntax." + _)

  /** Every analysis rule. */
  val Findings: Set[String] = BaumkuchenArtifacts.findings

  private val baumkuchenScalafixConfig =
    taskKey[File]("Write the scalafix configuration of the selected rules").withRank(KeyRanks.Invisible)
  private val baumkuchenValidated =
    settingKey[Unit]("Reject ids no tool knows when the build loads").withRank(KeyRanks.Invisible)
  private val baumkuchenModeId =
    settingKey[String]("The baumkuchenMode option value the tools read").withRank(KeyRanks.Invisible)
  private val baumkuchenEnabled =
    settingKey[Boolean]("Whether baumkuchen analyses this project").withRank(KeyRanks.Invisible)
  private val baumkuchenConfiguration =
    settingKey[Configuration]("The settings .baumkuchen.conf gives").withRank(KeyRanks.Invisible)

  private case class Settings(
      disabled: Option[Set[String]],
      errors: Option[Set[String]],
      warnings: Option[Set[String]],
      wildcards: Option[Int],
      library: Option[Boolean]
  )

  private case class Configuration(
      build: Settings,
      baseline: Option[String],
      purity: Option[String],
      effects: Option[Map[String, Set[String]]],
      concurrent: Option[Set[String]],
      generated: Option[Set[String]],
      hot: Option[Set[String]],
      reflective: Option[Set[String]],
      intended: Option[Set[String]],
      checked: Option[Set[String]],
      bounded: Vector[(String, Int, String)],
      nesting: Map[String, Int],
      text: Vector[String],
      projects: Map[String, Settings]
  )

  /** The kinds of effects a policy may allow. */
  private val Effects = Set("reads", "writes", "input", "output", "random", "unknown")

  override def globalSettings: Seq[Setting[?]] = Seq(
    baumkuchenMode :=
      (sys.env.get("BAUMKUCHEN_MODE") match {
        case Some(mode) =>
          BaumkuchenMode.values.find(_.toString.equalsIgnoreCase(mode)).getOrElse(
            sys.error(s"BAUMKUCHEN_MODE must be one of ${BaumkuchenMode.values.mkString(", ")}, not $mode")
          )
        case None => if (sys.env.contains("CI")) BaumkuchenMode.CI else BaumkuchenMode.Dev
      })
  ) ++ addCommandAlias("baumkuchenCheck", "; Test/compile ; scalafixAll --check ; baumkuchenAnalyse")

  // Defaults live in ThisBuild so one assignment there configures every project, and a project can still override.
  override def buildSettings: Seq[Setting[?]] = Seq(
    baumkuchenConfiguration := configured(baseDirectory.value / ".baumkuchen.conf"),
    baumkuchenDisabled      := baumkuchenConfiguration.value.build.disabled.getOrElse(Set.empty),
    baumkuchenErrors        := baumkuchenConfiguration.value.build.errors.getOrElse(Set.empty),
    baumkuchenWarnings      := baumkuchenConfiguration.value.build.warnings.getOrElse(Set.empty),
    baumkuchenWildcards     := baumkuchenConfiguration.value.build.wildcards.getOrElse(2),
    baumkuchenBaseline      :=
      IO.resolve(
        baseDirectory.value,
        file(baumkuchenConfiguration.value.baseline.getOrElse("baumkuchen-baseline.tsv"))
      ),
    baumkuchenPurity :=
      IO.resolve(baseDirectory.value, file(baumkuchenConfiguration.value.purity.getOrElse("baumkuchen-purity.tsv"))),
    baumkuchenEffects    := baumkuchenConfiguration.value.effects.getOrElse(Map.empty),
    baumkuchenConcurrent := baumkuchenConfiguration.value.concurrent.getOrElse(Set.empty),
    baumkuchenGenerated  := baumkuchenConfiguration.value.generated.getOrElse(Set.empty),
    baumkuchenHot        := baumkuchenConfiguration.value.hot.getOrElse(Set.empty),
    baumkuchenReflective := baumkuchenConfiguration.value.reflective.getOrElse(Set.empty),
    baumkuchenIntended   := baumkuchenConfiguration.value.intended.getOrElse(Set.empty),
    baumkuchenChecked    := baumkuchenConfiguration.value.checked.getOrElse(Set.empty),
    baumkuchenBounded    := baumkuchenConfiguration.value.bounded,
    baumkuchenNesting    := baumkuchenConfiguration.value.nesting,
    // The JVM would otherwise take a quarter of the host's memory, which a shared machine runs out of.
    baumkuchenAnalysisHeap := "4g"
  )

  override def projectSettings: Seq[Setting[?]] = Seq(
    baumkuchenEnabled  := true,
    baumkuchenDisabled := own(baumkuchenConfiguration.value, thisProject.value.id).flatMap(_.disabled)
      .getOrElse((ThisBuild / baumkuchenDisabled).value),
    baumkuchenErrors := own(baumkuchenConfiguration.value, thisProject.value.id).flatMap(_.errors)
      .getOrElse((ThisBuild / baumkuchenErrors).value),
    baumkuchenWarnings := own(baumkuchenConfiguration.value, thisProject.value.id).flatMap(_.warnings)
      .getOrElse((ThisBuild / baumkuchenWarnings).value),
    baumkuchenWildcards := own(baumkuchenConfiguration.value, thisProject.value.id).flatMap(_.wildcards)
      .getOrElse((ThisBuild / baumkuchenWildcards).value),
    baumkuchenValidated := {
      val unknown = (baumkuchenDisabled.value ++ baumkuchenErrors.value ++ baumkuchenWarnings.value) --
        (Checks ++ Findings ++ Rules ++ Bans)
      if (unknown.nonEmpty)
        throw MessageOnlyException(
          s"baumkuchen: unknown ids ${unknown.toSeq.sorted.mkString(", ")}; the ids are the compiler plugin checks " +
            s"${Checks.toSeq.sorted.mkString(", ")}; the analysis rules ${Findings.toSeq.sorted.mkString(", ")}; " +
            s"and the scalafix rules ${(Rules ++ Bans).mkString(", ")}"
        )
      val kinds = baumkuchenEffects.value.values.flatten.toSet -- Effects
      if (kinds.nonEmpty)
        throw MessageOnlyException(
          s"baumkuchen: unknown effect kinds ${kinds.toSeq.sorted.mkString(", ")}; the kinds are " +
            Effects.toSeq.sorted.mkString(", ")
        )
      val strangers = baumkuchenConfiguration.value.projects.keySet --
        loadedBuild.value.allProjectRefs.map(_._1.project)
      if (strangers.nonEmpty)
        throw MessageOnlyException(
          s"baumkuchen: .baumkuchen.conf configures projects ${strangers.toSeq.sorted.mkString(", ")}, which the " +
            "build does not have"
        )
    },
    baumkuchenModeId  := baumkuchenMode.value.toString.toLowerCase(java.util.Locale.ROOT),
    baumkuchenLibrary := Def.uncached(own(baumkuchenConfiguration.value, thisProject.value.id).flatMap(_.library)
      .getOrElse(!(publish / skip).value && (Compile / discoveredMainClasses).value.isEmpty)),
    libraryDependencies ++= {
      if (supported(scalaVersion.value))
        Seq(compilerPlugin((Organization % "baumkuchen-plugin" % Version).cross(CrossVersion.full)))
      else {
        sLog.value.warn(
          s"baumkuchen: no compiler plugin for Scala ${scalaVersion.value} in ${name.value}; " +
            s"its checks are skipped (built for ${BaumkuchenArtifacts.scalaVersions.mkString(", ")})"
        )
        Nil
      }
    },
    scalacOptions ++= {
      def option(name: String, ids: Set[String]) = (ids & Checks).toSeq.sorted.map(id => s"-P:baumkuchen:$name=$id")
      if (!supported(scalaVersion.value)) Nil
      else
        (s"-P:baumkuchen:mode=${baumkuchenModeId.value}" +:
          (option("disable", baumkuchenDisabled.value) ++ option("error", baumkuchenErrors.value) ++
            option("warn", baumkuchenWarnings.value) :+ s"-P:baumkuchen:wildcards=${baumkuchenWildcards.value}")) ++
          baumkuchenHot.value.toSeq.sorted.map(glob => s"-P:baumkuchen:hot=$glob") ++
          baumkuchenChecked.value.toSeq.sorted.map(tpe => s"-P:baumkuchen:checked=$tpe")
    },
    semanticdbEnabled := true,
    // The rules are a Scala 3 artifact, while sbt-scalafix resolves %% rules against Scala 2.13.
    scalafixDependencies += Organization % "baumkuchen-scalafix_3" % Version,
    // scalafixConfig is a setting, so the file it names is written by a task each scalafix run depends on.
    scalafixConfig           := Some(target.value / "baumkuchen" / "scalafix.conf"),
    baumkuchenScalafixConfig := Def.uncached(scalafixConfiguration(
      target.value / "baumkuchen" / "scalafix.conf",
      (ThisBuild / baseDirectory).value / ".scalafix.conf",
      Rules.filterNot(baumkuchenDisabled.value),
      Rules.filter(baumkuchenWarnings.value),
      Bans.filter(_ => !baumkuchenDisabled.value("DisableSyntax")).partition(!baumkuchenDisabled.value(_))
    )),
    Compile / scalafix := (Compile / scalafix).dependsOn(baumkuchenScalafixConfig).evaluated,
    Test / scalafix    := (Test / scalafix).dependsOn(baumkuchenScalafixConfig).evaluated,
    scalafixAll        := scalafixAll.dependsOn(baumkuchenScalafixConfig).evaluated,
    baumkuchenAnalyse  := Def.uncached {
      val code = analyse((_, _) => Nil).value
      if (code != 0) throw MessageOnlyException("baumkuchen analysis failed")
    },
    baumkuchenAnalyse / aggregate := false,
    baumkuchenBaselineWrite       := Def.uncached {
      val _ = analyse((baseline, _) => Seq("--write-baseline", baseline.getAbsolutePath)).value
      baumkuchenBaseline.value
    },
    baumkuchenBaselineWrite / aggregate := false,
    baumkuchenPurityWrite               := Def.uncached {
      val _ = analyse((_, purity) => Seq("--write-purity-lock", purity.getAbsolutePath)).value
      baumkuchenPurity.value
    },
    baumkuchenPurityWrite / aggregate := false
  )

  private def own(configured: Configuration, project: String): Option[Settings] =
    configured.projects.get(project)

  private def configured(file: File): Configuration =
    if (!file.exists)
      Configuration(
        Settings(None, None, None, None, None),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        Vector.empty,
        Map.empty,
        Vector.empty,
        Map.empty
      )
    else
      try {
        val config   = ConfigFactory.parseFile(file).resolve()
        val projects = Option.when(config.hasPath("projects"))(config.getConfig("projects")).fold(Map.empty) {
          projects =>
            projects.root.keySet.asScala.toSeq.map(id =>
              id -> settings(projects.getConfig(ConfigUtil.joinPath(id)), s"projects.$id", Set.empty)
            ).toMap
        }
        def path(key: String) = Option.when(config.hasPath(key))(config.getString(key))
        Configuration(
          settings(
            config,
            "",
            Set(
              "baseline",
              "purity",
              "effects",
              "concurrent",
              "generated",
              "hot",
              "reflective",
              "intended",
              "checked",
              "bounded",
              "nesting",
              "text",
              "projects"
            )
          ),
          path("baseline"),
          path("purity"),
          Option.when(config.hasPath("effects"))(config.getConfig("effects")).map(effects =>
            effects.root.keySet.asScala.toSeq.map(prefix =>
              prefix -> effects.getStringList(ConfigUtil.joinPath(prefix)).asScala.toSet
            ).toMap
          ),
          Option.when(config.hasPath("concurrent"))(config.getStringList("concurrent").asScala.toSet),
          Option.when(config.hasPath("generated"))(config.getStringList("generated").asScala.toSet),
          Option.when(config.hasPath("hot"))(config.getStringList("hot").asScala.toSet),
          Option.when(config.hasPath("reflective"))(config.getStringList("reflective").asScala.toSet),
          Option.when(config.hasPath("intended"))(config.getStringList("intended").asScala.toSet),
          Option.when(config.hasPath("checked"))(config.getStringList("checked").asScala.toSet),
          Option.when(config.hasPath("bounded"))(config.getConfigList("bounded").asScala.toVector.map(entry =>
            (entry.getString("member"), entry.getInt("max"), entry.getString("reason"))
          )).getOrElse(Vector.empty),
          nesting(config),
          Option.when(config.hasPath("text"))(config.getConfig("text")).fold(Vector.empty)(texts(_)),
          projects
        )
      } catch {
        case error: ConfigException => throw MessageOnlyException(s"baumkuchen: $file: ${error.getMessage}")
      }

  private val NestingLimits = Seq("depth", "damping", "size")

  /** The limits of the nesting rule the configuration names. */
  private def nesting(config: Config): Map[String, Int] =
    if (!config.hasPath("nesting")) Map.empty
    else {
      val block   = config.getConfig("nesting")
      val unknown = block.root.keySet.asScala.toSet -- NestingLimits
      if (unknown.nonEmpty)
        throw MessageOnlyException(
          s"baumkuchen: .baumkuchen.conf has unknown keys ${unknown.toSeq.sorted.map("nesting." + _).mkString(", ")}; " +
            s"the keys are ${NestingLimits.map("nesting." + _).mkString(", ")}"
        )
      NestingLimits.filter(block.hasPath).map(limit => limit -> block.getInt(limit)).toMap
    }

  private val TextKinds  = Seq("terms", "patterns", "binary", "licences", "preceded", "followed")
  private val TextFields = Map(
    "terms"    -> Set("words", "message"),
    "patterns" -> Set("pattern", "message"),
    "binary"   -> Set("message"),
    "licences" -> Set("message"),
    "preceded" -> Set("pattern", "message"),
    "followed" -> Set("pattern", "continues", "neutral", "message")
  ).view.mapValues(_ ++ Set("files", "exclude", "level")).toMap
  private val TextLists = Set("words", "files", "exclude")

  /** The manifest lines of the text rules: the exclusions, then each rule's fields by its index. */
  private def texts(config: Config): Vector[String] = {
    def known(found: Set[String], keys: Set[String], where: String) = {
      val unknown = found -- keys
      if (unknown.nonEmpty)
        throw MessageOnlyException(
          s"baumkuchen: .baumkuchen.conf has unknown keys ${unknown.toSeq.sorted.map(where + _).mkString(", ")}; the " +
            s"keys are ${keys.toSeq.sorted.mkString(", ")}"
        )
    }
    // The manifest is tab-separated, one entry a line.
    def plain(value: String) =
      if (value.exists(character => character == '\t' || character == '\n'))
        throw MessageOnlyException(s"baumkuchen: .baumkuchen.conf text values cannot hold tabs or newlines: $value")
      else value
    known(config.root.keySet.asScala.toSet, TextKinds.toSet + "exclude", "text.")
    val exclusions = Option.when(config.hasPath("exclude"))(config.getStringList("exclude").asScala.toVector)
      .getOrElse(Vector.empty).map(glob => s"text-exclude\t${plain(glob)}")
    val rules = TextKinds.flatMap(kind =>
      if (!config.hasPath(kind)) Nil
      else if (kind == "binary" || kind == "licences") List(kind -> config.getConfig(kind))
      else config.getConfigList(kind).asScala.toList.map(kind -> _)
    )
    exclusions ++ rules.zipWithIndex.flatMap { case ((kind, rule), index) =>
      known(rule.root.keySet.asScala.toSet, TextFields(kind), s"text.$kind.")
      s"text\t$index\tkind\t$kind" +: rule.root.keySet.asScala.toVector.sorted.flatMap(field =>
        (if (TextLists(field)) rule.getStringList(field).asScala.toVector else Vector(rule.getString(field)))
          .map(value => s"text\t$index\t$field\t${plain(value)}")
      )
    }
  }

  private def settings(config: Config, where: String, others: Set[String]): Settings = {
    val keys    = Set("disabled", "errors", "warnings", "wildcards", "library")
    val unknown = config.root.keySet.asScala.toSet -- keys -- others
    if (unknown.nonEmpty)
      throw MessageOnlyException(
        s"baumkuchen: .baumkuchen.conf has unknown keys ${unknown.toSeq.sorted.map(where + _).mkString(", ")}; the " +
          s"keys are ${(keys ++ others).toSeq.sorted.mkString(", ")}"
      )
    def ids(path: String) = Option.when(config.hasPath(path))(config.getStringList(path).asScala.toSet)
    Settings(
      ids("disabled"),
      ids("errors"),
      ids("warnings"),
      Option.when(config.hasPath("wildcards"))(config.getInt("wildcards")),
      Option.when(config.hasPath("library"))(config.getBoolean("library"))
    )
  }

  private def extendsTest(configuration: sbt.librarymanagement.Configuration): Boolean =
    configuration.extendsConfigs.exists(parent => parent.name == Test.name || extendsTest(parent))

  private final case class Analysed(
      project: String,
      configuration: String,
      scope: String,
      classes: String,
      library: Boolean,
      sources: Seq[String]
  )

  // A configuration with nothing to compile, or whose classes or sources another one of its project already holds,
  // as one that shares its parent's settings does, adds nothing to analyse.
  private def redundant(entries: Seq[Analysed]): Seq[Analysed] =
    entries.groupBy(_.project).values.toSeq.flatMap { own =>
      def base(entry: Analysed) = entry.configuration == Compile.name || entry.configuration == Test.name
      val (primary, extra)      = own.partition(base)
      extra.sortBy(_.configuration).foldLeft((primary, Vector.empty[Analysed])) { case ((kept, dropped), entry) =>
        if (
          entry.sources.isEmpty ||
          kept.exists(other => other.classes == entry.classes || other.sources.toSet == entry.sources.toSet)
        ) (kept, dropped :+ entry)
        else (kept :+ entry, dropped)
      }._2
    }

  private def supported(scalaVersion: String): Boolean = BaumkuchenArtifacts.scalaVersions.contains(scalaVersion)

  // The user's own file supplies rule settings and any rules of its own, which baumkuchen's rules extend; it can turn
  // a ban off, while a disabled ban stays off.
  private def scalafixConfiguration(
      file: File,
      user: File,
      rules: Seq[String],
      warnings: Seq[String],
      bans: (Seq[String], Seq[String])
  ): File = {
    val include                                = if (user.exists) s"include file(\"${user.getAbsolutePath}\")\n" else ""
    def list(key: String, values: Seq[String]) =
      values.map(value => s"  \"$value\"").mkString(s"$key = $${?$key} [\n", "\n", "\n]\n")
    val (enabled, disabled) = bans
    IO.write(
      file,
      enabled.map(ban => s"$ban = true\n").mkString + include + disabled.map(ban => s"$ban = false\n").mkString +
        list("rules", rules) + list("lint.warning", warnings)
    )
    file
  }

  /** Runs the analysis with `extra` arguments derived from the baseline and purity files, returning its exit code. */
  private def analyse(extra: (File, File) => Seq[String]): Def.Initialize[Task[Int]] = Def.taskDyn {
    // Aggregated projects baumkuchen does not run on, such as native toolchain builds, lack the JVM settings it reads.
    val data    = buildStructure.value.data
    val enabled = thisProjectRef.all(ScopeFilter(inAggregates(ThisProject))).value
      .filter(ref => (ref / baumkuchenEnabled).get(data).contains(true))
    // Scala Native and Scala.js twins compile the JVM project's sources again, so the JVM copy stands for those, and
    // the analysis reads only the classes of their own sources.
    def twin(ref: ProjectRef) = (ref / thisProject).get(data).exists(_.autoPlugins.exists(plugin =>
      plugin.label.endsWith("ScalaNativePlugin") || plugin.label.endsWith("ScalaJSPlugin")
    ))
    val (twins, jvm) = enabled.partition(twin(_))
    val platforms    = if (jvm.nonEmpty) twins.map(_.project).toSet else Set.empty[String]
    // Configurations that extend Test, as integration tests do, hold test code too.
    val tests = enabled.flatMap(ref => (ref / thisProject).get(data).toList.flatMap(_.configurations))
      .filter(extendsTest).distinctBy(_.name)
    val testNames      = tests.map(_.name).toSet + Test.name
    val configurations = Compile +: Test +: tests
    val analysed       = ScopeFilter(inProjects(enabled*), inConfigurations(configurations*))
    // The JVM projects' libraries come first, so they decide where a platform's library has the same classes.
    val jvmLibraries      = ScopeFilter(inProjects(jvm*), inConfigurations(configurations*))
    val platformLibraries = ScopeFilter(inProjects(twins*), inConfigurations(configurations*))
    // A project that other projects depend on only from their tests, as fixtures are, is test code itself.
    val dependents = buildStructure.value.allProjectRefs.flatMap(ref =>
      (ref / thisProject).get(data).toList.flatMap(_.dependencies)
    ).groupMap(_.project.project)(_.configuration)
    val fixtures = dependents.collect {
      case (id, configurations)
          if configurations.forall(_.exists(mappings =>
            mappings.split(';').forall(_.trim.takeWhile(_ != '-') == Test.name)
          )) => id
    }.toSet
    // The projects an analysed project depends on, analysed or not, are read from the classpath.
    val external = Def.task {
      val converter = fileConverter.value
      (externalDependencyClasspath.value ++ internalDependencyClasspath.value).map(entry =>
        converter.toPath(entry.data).toString
      )
    }
    Def.task {
      val entries = Def.task {
        val _     = (compile.value, copyResources.value)
        val id    = thisProject.value.id
        val name  = configuration.value.name
        val scope = if (testNames(name) || fixtures(id)) "test" else "main"
        Analysed(
          id,
          name,
          scope,
          classDirectory.value.toString,
          scope == "main" && baumkuchenLibrary.value,
          unmanagedSources.value.map(_.toString)
        )
      }.all(analysed).value
      val kept    = entries.filterNot(redundant(entries).toSet)
      val outputs = kept.flatMap(entry =>
        s"output\t${entry.project}\t${entry.scope}\t${entry.classes}" +:
          Option.when(entry.library)(s"library\t${entry.project}").toSeq
      )
      val analysedOutputs = outputs.collect { case s"output\t$_\t$_\t$directory" => directory }.toSet
      val libraries       = (external.all(jvmLibraries).value ++ external.all(platformLibraries).value).flatten.distinct
        .filterNot(analysedOutputs)
      val log       = streams.value.log
      val classpath = (libraries ++ javalibInterface(libraries, dependencyResolution.value, target.value, log))
        .map(entry => s"classpath\t$entry")
      val platform        = platforms.toSeq.sorted.map(id => s"platform\t$id")
      val compilerOptions = Def.task {
        val id = thisProject.value.id
        if (fixtures(id)) Nil else scalacOptions.value.map(option => s"option\t$id\t$option")
      }.all(ScopeFilter(inProjects((if (jvm.nonEmpty) jvm else enabled)*), inConfigurations(Compile))).value.flatten
      // Every platform's sources, so copies between the directories of a cross-built project show.
      val sources = kept.flatMap(entry => entry.sources.map(file => s"source\t${entry.project}\t${entry.scope}\t$file"))
      val baseline = baumkuchenBaseline.value
      val purity   = baumkuchenPurity.value
      val manifest = target.value / "baumkuchen" / "manifest.tsv"
      val root     = s"root\t${(ThisBuild / baseDirectory).value}"
      val text     = baumkuchenConfiguration.value.text
      IO.write(
        manifest,
        (outputs ++ platform ++ classpath ++ compilerOptions ++ sources ++ text :+ root).mkString("", "\n", "\n")
      )
      val tool = analysisClasspath(dependencyResolution.value, scalaVersion.value, target.value, log)
      def option(name: String, ids: Set[String]) = (ids & Findings).toSeq.sorted.flatMap(id => Seq(s"--$name", id))
      val options = Seq("--mode", baumkuchenModeId.value) ++ option("disable", baumkuchenDisabled.value) ++
        option("error", baumkuchenErrors.value) ++ option("warn", baumkuchenWarnings.value) ++
        Seq("--baseline", baseline.getAbsolutePath, "--purity-lock", purity.getAbsolutePath) ++
        baumkuchenEffects.value.toSeq.sortBy(_._1).flatMap((prefix, kinds) =>
          Seq("--effects", s"$prefix=${kinds.toSeq.sorted.mkString(",")}")
        ) ++ baumkuchenConcurrent.value.toSeq.sorted.flatMap(method => Seq("--concurrent", method)) ++
        baumkuchenGenerated.value.toSeq.sorted.flatMap(glob => Seq("--generated", glob)) ++
        baumkuchenReflective.value.toSeq.sorted.flatMap(glob => Seq("--reflective", glob)) ++
        baumkuchenHot.value.toSeq.sorted.flatMap(glob => Seq("--hot", glob)) ++
        baumkuchenIntended.value.toSeq.sorted.flatMap(pair => Seq("--intended", pair)) ++
        baumkuchenBounded.value.flatMap((member, max, reason) => Seq("--bounded", member, max.toString, reason)) ++
        (if (baumkuchenNesting.value.isEmpty) Nil
         else "--nesting" +: baumkuchenNesting.value.toSeq.sortBy(_._1).flatMap((limit, value) =>
           Seq(s"--nesting-$limit", value.toString)
         )) ++
        extra(baseline, purity)
      Fork.java(
        ForkOptions().withRunJVMOptions(Vector(s"-Xmx${baumkuchenAnalysisHeap.value}")),
        Seq(
          "-cp",
          tool.mkString(java.io.File.pathSeparator),
          "net.kurobako.baumkuchen.analysis.CLI",
          "--manifest",
          manifest.getAbsolutePath
        ) ++ options
      )
    }
  }

  private val Javalib = ".*/javalib_native[^/]*/([^/]+)/.*".r

  // Scala Native's javalib ships its interface to the runtime as NIR only, which inline expansions in a twin's classes
  // reference, so the analysis reads the published class files of the same version.
  private def javalibInterface(
      libraries: Seq[String],
      resolution: DependencyResolution,
      target: File,
      log: Logger
  ): Vector[String] =
    libraries.collectFirst { case Javalib(version) => version }.toVector.flatMap { version =>
      val module = ("org.scala-native" % "javalib-intf" % version).intransitive()
      resolution.retrieve(module, None, target / "baumkuchen" / "javalib-intf", log) match {
        case Right(files)  => files.map(_.toString)
        case Left(warning) =>
          log.warn(s"baumkuchen: cannot resolve javalib-intf $version: ${warning.resolveException.getMessage}")
          Vector.empty
      }
    }

  private def analysisClasspath(
      resolution: DependencyResolution,
      scalaVersion: String,
      target: File,
      log: Logger
  ): Vector[File] = {
    if (!supported(scalaVersion))
      throw MessageOnlyException(
        s"baumkuchen: no analysis for Scala $scalaVersion (built for ${BaumkuchenArtifacts.scalaVersions.mkString(", ")})"
      )
    val module = Organization % s"baumkuchen-analysis_$scalaVersion" % Version
    resolution.retrieve(module, None, target / "baumkuchen" / "analysis", log) match {
      case Right(files)  => files.toVector
      case Left(warning) =>
        throw MessageOnlyException(s"baumkuchen: cannot resolve the analysis: ${warning.resolveException.getMessage}")
    }
  }
}
