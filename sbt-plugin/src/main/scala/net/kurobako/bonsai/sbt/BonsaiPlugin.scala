package net.kurobako.bonsai.sbt

import net.kurobako.bonsai.sbt.BonsaiConfiguration.Configuration
import net.kurobako.bonsai.sbt.BonsaiConfiguration.Settings
import net.kurobako.bonsai.sbt.ManifestLines
import sbt.*
import sbt.Keys.*
import sbt.internal.util.MessageOnlyException
import sbt.librarymanagement.DependencyResolution
import scalafix.sbt.ScalafixPlugin
import scalafix.sbt.ScalafixPlugin.autoImport.*

/** Wires bonsai's scalafix rules, compiler plugin checks and whole-build analysis into every project, with defaults
  * that warn while developing and fail in CI.
  *
  * `.bonsai.conf` at the root of the build sets the defaults of the keys, for the build at its top level and for a
  * project under `projects.<id>`, so a build configured there alone still loads without the plugin:
  * {{{
  * disabled   = [PackageNaming]
  * errors     = [local-throw]
  * warnings   = []
  * wildcards  = 2
  * baseline   = bonsai-baseline.tsv
  * purity     = bonsai-purity.tsv
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
  * `text` configures rules over every file git sees, whatever its language, which `bonsaiAnalyse` checks. Globs match
  * the path from the build root with a leading slash, and `**` crosses directories; every rule takes `files`, `exclude`
  * and a `level` of `error`, the default, `warning` or `info`; and a line containing `bonsai:ignore <rule id>` exempts
  * itself and the line after it:
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
  * `@SuppressWarnings(Array("bonsai:<id>"))` on an enclosing definition suppresses a check or analysis rule, and
  * `"bonsai"` all of them. An expression takes it ascribed, as `expr: @SuppressWarnings(Array("bonsai:<id>"))`.
  * `@nowarn("msg=bonsai:<id>")` suppresses the compiler plugin's checks too, but the compiler sees no warning of an
  * analysis rule for it to suppress, and a build without bonsai sees none at all, so it reports the `@nowarn` under
  * `-Wunused:nowarn`. The `nowarn-suppression` check flags it.
  */
object BonsaiPlugin extends AutoPlugin {
  override def requires: Plugins      = ScalafixPlugin
  override def trigger: PluginTrigger = allRequirements

  object autoImport {
    enum BonsaiMode {
      case Dev, CI
    }

    val bonsaiMode = settingKey[BonsaiMode](
      "Dev warns; CI fails on the strict checks and rules. Defaults to BONSAI_MODE, else CI when CI is set"
    )
    val bonsaiDisabled =
      settingKey[Set[String]]("Compiler plugin checks, analysis rules and scalafix rules left out, by id")
    val bonsaiErrors =
      settingKey[Set[String]]("Compiler plugin checks and analysis rules that fail in every mode, by id")
    val bonsaiWarnings =
      settingKey[Set[String]]("Compiler plugin checks, analysis rules and scalafix rules that never fail, by id")
    val bonsaiWildcards = settingKey[Int]("Positional wildcards a case class pattern may carry")
    val bonsaiBaseline  = settingKey[File]("Analysis findings recorded here report without failing")
    val bonsaiLibrary   = taskKey[Boolean](
      "Whether other builds use this project's public API, so the analysis counts it as used. " +
        "Defaults to whether the project is published and has no main class"
    )
    val bonsaiPurity  = settingKey[File]("Methods recorded here as pure fail the analysis once they are not")
    val bonsaiEffects = settingKey[Map[String, Set[String]]](
      "Kinds of effects (reads, writes, input, output, random, unknown) the methods of a package or class may have, by " +
        "its qualified name, with * for any one name; the most specific entry decides"
    )
    val bonsaiGenerated = settingKey[Set[String]](
      "Globs of sources the build generates, such as **/src/generated/**, which the analysis labels as generated; " +
        "every source is checked by default"
    )
    val bonsaiConcurrent =
      settingKey[Set[String]]("Methods, by qualified name, that run the functions passed to them on other threads")
    val bonsaiHot = settingKey[Set[String]](
      "Globs of the classes and objects whose code runs hot, by qualified name, with * for one name segment and ** " +
        "for any number, and ! before a glob leaving out what it matches; boxing-fold, megamorphic-call, and " +
        "invariant-in-loop on matches whose arms vary, report only in them, while unspecialised-function and " +
        "lambda-trait leave out classes a ! glob matches and hot ones respectively"
    )
    val bonsaiReflective = settingKey[Set[String]](
      "Globs of the classes and objects, by qualified name as for bonsaiHot, that code outside the sources names, " +
        "as text compiled later or reflection does; they and their public members are used"
    )
    val bonsaiIntended = settingKey[Set[String]](
      "Pairs of globs written <glob>=<glob>, matched as for bonsaiHot, of types whose copies or likeness are meant; " +
        "duplicate-code, near-duplicate-code and duplicate-shape leave out groups whose members are all such pairs"
    )
    val bonsaiChecked = settingKey[Set[String]](
      "Refinement types, by qualified name, whose checked extensions and overloads a conversion can bypass, and whose " +
        "inferred refinements the compiler plugin reports"
    )
    val bonsaiNesting = settingKey[Map[String, Int]](
      "Limits of the nesting rule by name: depth, damping and size; a limit left out takes the analysis' default"
    )
    val bonsaiBounded = settingKey[Vector[(String, Int, String)]](
      "Members, as (qualified name glob matched as for bonsaiHot, largest number of elements, reason), whose " +
        "collections hold at most that many elements, which the complexity rules count as constants"
    )
    val bonsaiAnalysisHeap =
      settingKey[String]("The largest heap the forked analysis may take, as -Xmx takes it, such as 4g")
    val bonsaiAnalyse       = taskKey[Unit]("Analyse this project and the projects it aggregates as one build")
    val bonsaiBaselineWrite = taskKey[File]("Record the current analysis warnings in bonsaiBaseline")
    val bonsaiPurityWrite   = taskKey[File]("Record the methods of main code that are pure in bonsaiPurity")
  }

  import autoImport.*

  private val Organization = BonsaiArtifacts.organization
  private val Version      = BonsaiArtifacts.version

  /** Every check the compiler plugin knows. */
  private val Checks: Set[String] = BonsaiArtifacts.checks

  /** Every scalafix rule, in the order they run, with the built-in ones last. */
  val Rules: Seq[String] = BonsaiArtifacts.rules ++ Seq("DisableSyntax", "RedundantSyntax")

  /** The DisableSyntax bans on by default, each disabled by its own id. */
  private val Bans: Seq[String] =
    Seq("noVars", "noReturns", "noNulls", "noAsInstanceOf", "noIsInstanceOf", "noXml", "noFinalize")
      .map("DisableSyntax." + _)

  /** Every analysis rule. */
  private val Findings: Set[String] = BonsaiArtifacts.findings

  private val bonsaiScalafixConfig =
    taskKey[File]("Write the scalafix configuration of the selected rules").withRank(KeyRanks.Invisible)
  private val bonsaiValidated =
    settingKey[Unit]("Reject ids no tool knows when the build loads").withRank(KeyRanks.Invisible)
  private val bonsaiModeId =
    settingKey[String]("The bonsaiMode option value the tools read").withRank(KeyRanks.Invisible)
  private val bonsaiEnabled =
    settingKey[Boolean]("Whether bonsai analyses this project").withRank(KeyRanks.Invisible)
  private val bonsaiConfiguration =
    settingKey[Configuration]("The settings .bonsai.conf gives").withRank(KeyRanks.Invisible)

  /** The kinds of effects a policy may allow. */
  private val Effects = Set("reads", "writes", "input", "output", "random", "unknown")

  override def globalSettings: Seq[Setting[?]] = Seq(
    bonsaiMode :=
      (sys.env.get("BONSAI_MODE") match {
        case Some(mode) =>
          BonsaiMode.values.find(_.toString.equalsIgnoreCase(mode)).getOrElse(
            sys.error(s"BONSAI_MODE must be one of ${BonsaiMode.values.mkString(", ")}, not $mode")
          )
        case None => if (sys.env.contains("CI")) BonsaiMode.CI else BonsaiMode.Dev
      })
  ) ++ addCommandAlias("bonsaiCheck", "; Test/compile ; scalafixAll --check ; bonsaiAnalyse")

  // Defaults live in ThisBuild so one assignment there configures every project, and a project can still override.
  override def buildSettings: Seq[Setting[?]] = Seq(
    bonsaiConfiguration := BonsaiConfiguration.read(baseDirectory.value / ".bonsai.conf"),
    bonsaiDisabled      := bonsaiConfiguration.value.build.disabled.getOrElse(Set.empty),
    bonsaiErrors        := bonsaiConfiguration.value.build.errors.getOrElse(Set.empty),
    bonsaiWarnings      := bonsaiConfiguration.value.build.warnings.getOrElse(Set.empty),
    bonsaiWildcards     := bonsaiConfiguration.value.build.wildcards.getOrElse(2),
    bonsaiBaseline      :=
      IO.resolve(
        baseDirectory.value,
        file(bonsaiConfiguration.value.baseline.getOrElse(BonsaiConfiguration.DefaultBaseline))
      ),
    bonsaiPurity :=
      IO.resolve(
        baseDirectory.value,
        file(bonsaiConfiguration.value.purity.getOrElse(BonsaiConfiguration.DefaultPurity))
      ),
    bonsaiEffects    := bonsaiConfiguration.value.effects.getOrElse(Map.empty),
    bonsaiConcurrent := bonsaiConfiguration.value.concurrent.getOrElse(Set.empty),
    bonsaiGenerated  := bonsaiConfiguration.value.generated.getOrElse(Set.empty),
    bonsaiHot        := bonsaiConfiguration.value.hot.getOrElse(Set.empty),
    bonsaiReflective := bonsaiConfiguration.value.reflective.getOrElse(Set.empty),
    bonsaiIntended   := bonsaiConfiguration.value.intended.getOrElse(Set.empty),
    bonsaiChecked    := bonsaiConfiguration.value.checked.getOrElse(Set.empty),
    bonsaiBounded    := bonsaiConfiguration.value.bounded,
    bonsaiNesting    := bonsaiConfiguration.value.nesting,
    // The JVM would otherwise take a quarter of the host's memory, which a shared machine runs out of.
    bonsaiAnalysisHeap := "4g"
  )

  override def projectSettings: Seq[Setting[?]] = Seq(
    bonsaiEnabled   := true,
    bonsaiDisabled  := ownOr(bonsaiDisabled)(_.disabled).value,
    bonsaiErrors    := ownOr(bonsaiErrors)(_.errors).value,
    bonsaiWarnings  := ownOr(bonsaiWarnings)(_.warnings).value,
    bonsaiWildcards := ownOr(bonsaiWildcards)(_.wildcards).value,
    bonsaiValidated := {
      val unknown = (bonsaiDisabled.value ++ bonsaiErrors.value ++ bonsaiWarnings.value) --
        (Checks ++ Findings ++ Rules ++ Bans)
      if (unknown.nonEmpty)
        throw MessageOnlyException(
          s"bonsai: unknown ids ${unknown.toSeq.sorted.mkString(", ")}; the ids are the compiler plugin checks " +
            s"${Checks.toSeq.sorted.mkString(", ")}; the analysis rules ${Findings.toSeq.sorted.mkString(", ")}; " +
            s"and the scalafix rules ${(Rules ++ Bans).mkString(", ")}"
        )
      val kinds = bonsaiEffects.value.values.flatten.toSet -- Effects
      if (kinds.nonEmpty)
        throw MessageOnlyException(
          s"bonsai: unknown effect kinds ${kinds.toSeq.sorted.mkString(", ")}; the kinds are " +
            Effects.toSeq.sorted.mkString(", ")
        )
      val strangers = bonsaiConfiguration.value.projects.keySet --
        loadedBuild.value.allProjectRefs.map(_._1.project)
      if (strangers.nonEmpty)
        throw MessageOnlyException(
          s"bonsai: .bonsai.conf configures projects ${strangers.toSeq.sorted.mkString(", ")}, which the " +
            "build does not have"
        )
    },
    bonsaiModeId  := bonsaiMode.value.toString.toLowerCase(java.util.Locale.ROOT),
    bonsaiLibrary := Def.uncached(bonsaiConfiguration.value.library(thisProject.value.id)
      .getOrElse(!(publish / skip).value && (Compile / discoveredMainClasses).value.isEmpty)),
    libraryDependencies ++= {
      ScalaVersions.plugin(scalaVersion.value) match {
        case Some(line) =>
          Seq(compilerPlugin((Organization % "bonsai-plugin" % Version).cross(CrossVersion.constant(line))))
        case None =>
          sLog.value.warn(
            s"bonsai: no compiler plugin for Scala ${scalaVersion.value} in ${name.value}; " +
              s"its checks are skipped (built for $builtLines)"
          )
          Nil
      }
    },
    scalacOptions ++= {
      def option(name: String, ids: Set[String]) = (ids & Checks).toSeq.sorted.map(id => s"-P:bonsai:$name=$id")
      if (ScalaVersions.plugin(scalaVersion.value).isEmpty) Nil
      else
        (s"-P:bonsai:mode=${bonsaiModeId.value}" +:
          (option("disable", bonsaiDisabled.value) ++ option("error", bonsaiErrors.value) ++
            option("warn", bonsaiWarnings.value) :+ s"-P:bonsai:wildcards=${bonsaiWildcards.value}")) ++
          bonsaiHot.value.toSeq.sorted.map(glob => s"-P:bonsai:hot=$glob") ++
          bonsaiChecked.value.toSeq.sorted.map(tpe => s"-P:bonsai:checked=$tpe")
    },
    semanticdbEnabled := true,
    // The rules are a Scala 3 artifact, while sbt-scalafix resolves %% rules against Scala 2.13.
    scalafixDependencies += Organization % "bonsai-scalafix_3" % Version,
    // scalafixConfig is a setting, so the file it names is written by a task each scalafix run depends on.
    scalafixConfig       := Some(target.value / "bonsai" / "scalafix.conf"),
    bonsaiScalafixConfig := Def.uncached(scalafixConfiguration(
      target.value / "bonsai" / "scalafix.conf",
      (ThisBuild / baseDirectory).value / ".scalafix.conf",
      Rules.filterNot(bonsaiDisabled.value),
      Rules.filter(bonsaiWarnings.value),
      (if (bonsaiDisabled.value("DisableSyntax")) Nil else Bans).partition(!bonsaiDisabled.value(_))
    )),
    Compile / scalafix := (Compile / scalafix).dependsOn(bonsaiScalafixConfig).evaluated,
    Test / scalafix    := (Test / scalafix).dependsOn(bonsaiScalafixConfig).evaluated,
    scalafixAll        := scalafixAll.dependsOn(bonsaiScalafixConfig).evaluated,
    bonsaiAnalyse      := Def.uncached {
      val code = analyse((_, _) => Nil).value
      if (code != 0) throw MessageOnlyException("bonsai analysis failed")
    },
    bonsaiAnalyse / aggregate := false,
    bonsaiBaselineWrite       := Def.uncached {
      val _ = analyse((baseline, _) => Seq("--write-baseline", baseline.getAbsolutePath)).value
      bonsaiBaseline.value
    },
    bonsaiBaselineWrite / aggregate := false,
    bonsaiPurityWrite               := Def.uncached {
      val _ = analyse((_, purity) => Seq("--write-purity-lock", purity.getAbsolutePath)).value
      bonsaiPurity.value
    },
    bonsaiPurityWrite / aggregate := false
  )

  // The lines the compiler plugin is built for, with the oldest patch of those it does not run in from the first.
  private def builtLines: String =
    BonsaiArtifacts.lines.map(line => BonsaiArtifacts.oldest.get(line).fold(line)(oldest => s"$line from $oldest"))
      .mkString(", ")

  private def own(configured: Configuration, project: String): Option[Settings] =
    configured.projects.get(project)

  private def ownOr[A](key: SettingKey[A])(pick: Settings => Option[A]): Def.Initialize[A] = Def.setting {
    own(bonsaiConfiguration.value, thisProject.value.id).flatMap(pick).getOrElse((ThisBuild / key).value)
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

  // The user's own file supplies rule settings and any rules of its own, which bonsai's rules extend; it can turn
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
    // Aggregated projects bonsai does not run on, such as native toolchain builds, lack the JVM settings it reads.
    val data    = buildStructure.value.data
    val enabled = thisProjectRef.all(ScopeFilter(inAggregates(ThisProject))).value
      .filter(ref => (ref / bonsaiEnabled).get(data).contains(true))
    // Scala Native and Scala.js twins compile the JVM project's sources again, so the JVM copy stands for those, and
    // the analysis reads only the classes of their own sources.
    def twin(ref: ProjectRef) = (ref / thisProject).get(data).exists(_.autoPlugins.exists(plugin =>
      plugin.label.endsWith("ScalaNativePlugin") || plugin.label.endsWith("ScalaJSPlugin")
    ))
    val (twins, jvm) = enabled.partition(twin(_))
    val platforms    = if (jvm.nonEmpty) twins.map(_.project).toSet else Set.empty[String]
    // Configurations that extend Test, as integration tests do, hold test code too.
    val tests = enabled.view.flatMap(ref => (ref / thisProject).get(data).toList.flatMap(_.configurations))
      .filter(extendsTest).toSeq.distinctBy(_.name)
    val testNames      = tests.map(_.name).toSet + Test.name
    val configurations = Compile +: Test +: tests
    val analysed       = ScopeFilter(inProjects(enabled*), inConfigurations(configurations*))
    val fixtures       = Fixtures.fixtures(buildStructure.value)
    // The projects an analysed project depends on, analysed or not, are read from the classpath.
    val external = Def.task {
      val converter = fileConverter.value
      thisProject.value.id -> (externalDependencyClasspath.value ++ internalDependencyClasspath.value).map(entry =>
        converter.toPath(entry.data).toString
      )
    }
    Def.task {
      val entries = Def.task {
        val _     = (compile.value, copyResources.value)
        val id    = thisProject.value.id
        val name  = configuration.value.name
        val scope = ManifestLines.scope(fixtures, id, testNames(name))
        Analysed(
          id,
          name,
          scope,
          classDirectory.value.toString,
          bonsaiLibrary.value,
          unmanagedSources.value.map(_.toString)
        )
      }.all(analysed).value
      val kept    = entries.filterNot(redundant(entries).toSet)
      val outputs = kept.flatMap(entry =>
        ManifestLines.outputs(entry.project, entry.scope, entry.classes, entry.library)
      )
      val analysedOutputs = kept.map(_.classes).toSet
      val libraries       = external.all(analysed).value.groupMap(_._1)(_._2).toSeq.sortBy(_._1).map {
        (id, entries) => id -> entries.flatten.distinct.filterNot(analysedOutputs)
      }
      val log        = streams.value.log
      val resolution = dependencyResolution.value
      val classpath  = libraries.flatMap((id, entries) =>
        ManifestLines.classpath(id, entries ++ javalibInterface(entries, resolution, target.value, log))
      )
      val platform        = platforms.toSeq.sorted.map(id => s"platform\t$id")
      val compilerOptions = Def.task {
        val id = thisProject.value.id
        if (fixtures(id)) Nil else ManifestLines.options(id, scalacOptions.value)
      }.all(ScopeFilter(inProjects((if (jvm.nonEmpty) jvm else enabled)*), inConfigurations(Compile))).value.flatten
      // Every platform's sources, so copies between the directories of a cross-built project show.
      val sources  = kept.flatMap(entry => ManifestLines.sources(entry.project, entry.scope, entry.sources))
      val baseline = bonsaiBaseline.value
      val purity   = bonsaiPurity.value
      val manifest = target.value / "bonsai" / "manifest.tsv"
      val text     = bonsaiConfiguration.value.text
      IO.write(
        manifest,
        ManifestLines.render(
          outputs ++ platform ++ classpath ++ compilerOptions ++ sources ++ text,
          (ThisBuild / baseDirectory).value
        )
      )
      // What the build generates is reported where its generator lives, not where it lands.
      val generated =
        Def.task(BonsaiConfiguration.generatedGlob((ThisBuild / baseDirectory).value, sourceManaged.value))
          .all(analysed).value.toSet
      val versions = Def.task(scalaVersion.value).all(analysed).value.distinct
      val tool     = analysisClasspath(dependencyResolution.value, versions, target.value, log)
      val options  = Seq("--mode", bonsaiModeId.value) ++
        BonsaiConfiguration.Analysis(
          bonsaiDisabled.value,
          bonsaiErrors.value,
          bonsaiWarnings.value,
          bonsaiEffects.value,
          bonsaiConcurrent.value,
          bonsaiGenerated.value ++ generated,
          bonsaiReflective.value,
          bonsaiHot.value,
          bonsaiIntended.value,
          bonsaiBounded.value,
          bonsaiNesting.value
        ).options(Findings, baseline, purity) ++
        extra(baseline, purity)
      Fork.java(
        ForkOptions().withRunJVMOptions(Vector(s"-Xmx${bonsaiAnalysisHeap.value}")),
        Seq(
          "-cp",
          tool.mkString(java.io.File.pathSeparator),
          "net.kurobako.bonsai.analysis.CLI",
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
      resolution.retrieve(module, None, target / "bonsai" / "javalib-intf", log) match {
        case Right(files)  => files.map(_.toString)
        case Left(warning) =>
          log.warn(s"bonsai: cannot resolve javalib-intf $version: ${warning.resolveException.getMessage}")
          Vector.empty
      }
    }

  private def analysisClasspath(
      resolution: DependencyResolution,
      scalaVersions: Seq[String],
      target: File,
      log: Logger
  ): Vector[File] = {
    scalaVersions.find(!ScalaVersions.readable(_)).foreach(scalaVersion =>
      throw MessageOnlyException(
        s"bonsai: no analysis for Scala $scalaVersion (it reads the TASTy of Scala ${BonsaiArtifacts.analysisScala} " +
          "and the minor lines before it)"
      )
    )
    val module = Organization % "bonsai-analysis_3" % Version
    resolution.retrieve(module, None, target / "bonsai" / "analysis", log) match {
      case Right(files)  => files.toVector
      case Left(warning) =>
        throw MessageOnlyException(s"bonsai: cannot resolve the analysis: ${warning.resolveException.getMessage}")
    }
  }
}
