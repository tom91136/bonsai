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
  * projects.api { library = true }
  * }}}
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
    val baumkuchenConcurrent =
      settingKey[Set[String]]("Methods, by qualified name, that run the functions passed to them on other threads")
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
    baumkuchenConcurrent := baumkuchenConfiguration.value.concurrent.getOrElse(Set.empty)
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
        s"-P:baumkuchen:mode=${baumkuchenModeId.value}" +:
          (option("disable", baumkuchenDisabled.value) ++ option("error", baumkuchenErrors.value) ++
            option("warn", baumkuchenWarnings.value) :+ s"-P:baumkuchen:wildcards=${baumkuchenWildcards.value}")
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
    if (!file.exists) Configuration(Settings(None, None, None, None, None), None, None, None, None, Map.empty)
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
          settings(config, "", Set("baseline", "purity", "effects", "concurrent", "projects")),
          path("baseline"),
          path("purity"),
          Option.when(config.hasPath("effects"))(config.getConfig("effects")).map(effects =>
            effects.root.keySet.asScala.toSeq.map(prefix =>
              prefix -> effects.getStringList(ConfigUtil.joinPath(prefix)).asScala.toSet
            ).toMap
          ),
          Option.when(config.hasPath("concurrent"))(config.getStringList("concurrent").asScala.toSet),
          projects
        )
      } catch {
        case error: ConfigException => throw MessageOnlyException(s"baumkuchen: $file: ${error.getMessage}")
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
    // Scala Native and Scala.js twins compile the JVM project's sources again, so the JVM copy stands for them.
    def twin(ref: ProjectRef) = (ref / thisProject).get(data).exists(_.autoPlugins.exists(plugin =>
      plugin.label.endsWith("ScalaNativePlugin") || plugin.label.endsWith("ScalaJSPlugin")
    ))
    val jvm      = enabled.filterNot(twin(_))
    val analysed = ScopeFilter(inProjects((if (jvm.nonEmpty) jvm else enabled)*), inConfigurations(Compile, Test))
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
    Def.task {
      val outputs = Def.task {
        val _       = (compile.value, copyResources.value)
        val id      = thisProject.value.id
        val scope   = if (configuration.value.name == Test.name || fixtures(id)) "test" else "main"
        val library = Option.when(scope == "main" && baumkuchenLibrary.value)(s"library\t$id")
        s"output\t$id\t$scope\t${classDirectory.value}" +: library.toSeq
      }.all(analysed).value.flatten
      val classpath = Def.task {
        val converter = fileConverter.value
        externalDependencyClasspath.value.map(entry => converter.toPath(entry.data).toString)
      }.all(analysed).value.flatten.distinct.map(entry => s"classpath\t$entry")
      val compilerOptions = Def.task {
        val id = thisProject.value.id
        if (fixtures(id)) Nil else scalacOptions.value.map(option => s"option\t$id\t$option")
      }.all(ScopeFilter(inProjects((if (jvm.nonEmpty) jvm else enabled)*), inConfigurations(Compile))).value.flatten
      // Every platform's sources, so copies between the directories of a cross-built project show.
      val sources = Def.task {
        val id    = thisProject.value.id
        val scope = if (configuration.value.name == Test.name || fixtures(id)) "test" else "main"
        unmanagedSources.value.map(file => s"source\t$id\t$scope\t$file")
      }.all(ScopeFilter(inProjects(enabled*), inConfigurations(Compile, Test))).value.flatten
      val log      = streams.value.log
      val baseline = baumkuchenBaseline.value
      val purity   = baumkuchenPurity.value
      val manifest = target.value / "baumkuchen" / "manifest.tsv"
      val root     = s"root\t${(ThisBuild / baseDirectory).value}"
      IO.write(manifest, (outputs ++ classpath ++ compilerOptions ++ sources :+ root).mkString("", "\n", "\n"))
      val tool = analysisClasspath(dependencyResolution.value, scalaVersion.value, target.value, log)
      def option(name: String, ids: Set[String]) = (ids & Findings).toSeq.sorted.flatMap(id => Seq(s"--$name", id))
      val options = Seq("--mode", baumkuchenModeId.value) ++ option("disable", baumkuchenDisabled.value) ++
        option("error", baumkuchenErrors.value) ++ option("warn", baumkuchenWarnings.value) ++
        Seq("--baseline", baseline.getAbsolutePath, "--purity-lock", purity.getAbsolutePath) ++
        baumkuchenEffects.value.toSeq.sortBy(_._1).flatMap((prefix, kinds) =>
          Seq("--effects", s"$prefix=${kinds.toSeq.sorted.mkString(",")}")
        ) ++ baumkuchenConcurrent.value.toSeq.sorted.flatMap(method => Seq("--concurrent", method)) ++
        extra(baseline, purity)
      Fork.java(
        ForkOptions(),
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
