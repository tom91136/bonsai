import net.kurobako.bonsai.sbt.BonsaiConfiguration
import net.kurobako.bonsai.sbt.Fixtures
import net.kurobako.bonsai.sbt.ManifestLines

// The compiler plugin links against the compiler, so it is built for the latest patch of every minor line from the LTS
// line up, and published for the line. A compiler reads the TASTy of its own and older minor lines, so the analysis is
// built once, for the newest.
lazy val scala3Versions = Seq("3.3.9", "3.4.3", "3.5.2", "3.6.4", "3.7.4", "3.8.4", "3.9.0", "3.10.0")
lazy val scala3Version  = scala3Versions.last
lazy val munitVersion   = "1.3.6"

// The Scala version sbt 2 runs on, which its plugins and what they depend on are built with.
lazy val sbtScalaVersion = "3.8.4"

// The newest Scala version scalafix publishes its test kit for, which the rules and their fixtures build with.
lazy val scalafixScalaVersion = "3.9.0"

// The oldest patch of a line that its compiler plugin runs in, where it is not the first: the plugin is built against
// the latest patch, and calls compiler methods that earlier patches lack. The scripted test `oldest` runs each plugin in
// its oldest patch and in the one below, so a table that has gone wrong fails it.
lazy val oldestPatches = Map("3.3" -> "3.3.4", "3.7" -> "3.7.4")

def scalaLine(version: String): String = version.split('.').take(2).mkString(".")

def oldestPatch(line: String): String = oldestPatches.getOrElse(line, s"$line.0")

// Everything for the newest Scala version, then the compiler plugin of every other minor line, and back to the newest.
addCommandAlias(
  "publishLocalAll",
  (("publishLocal" +: scala3Versions.init.flatMap(version => Seq(s"++$version", "plugin/publishLocal"))) :+
    s"++$scala3Version").mkString("; ")
)

// The scripted tests build with older lines too, so their compiler plugins are published first.
addCommandAlias("scriptedAll", "publishLocalAll; sbtBonsai/scripted")

lazy val perLine = Seq(
  crossScalaVersions := scala3Versions,
  crossVersion       := CrossVersion.constant(scalaLine(scalaVersion.value))
)

ThisBuild / organization      := "net.kurobako"
ThisBuild / version           := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion      := scala3Version
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

lazy val analysisConfiguration = taskKey[BonsaiConfiguration.Configuration]("The .bonsai.conf of this build.")
lazy val analysisManifest      = taskKey[File]("Write the whole-build analysis manifest of this build.")
lazy val analyse               = taskKey[Unit]("Run the whole-build analysis over this build.")
lazy val analysisClasspath = taskKey[Unit]("Print the classpath that runs the analysis, for scripts to run it with.")
lazy val analysisArguments =
  taskKey[File]("Write the analysis arguments .bonsai.conf gives this build, one a line, for scripts to run it with.")

lazy val analysedOutputs =
  ScopeFilter(
    inAggregates(LocalRootProject, includeRoot = false) -- inProjects(ruleInput, ruleOutput),
    inConfigurations(Compile, Test)
  )

def classpathOf(entries: Classpath, converter: xsbti.FileConverter, extra: Seq[String] = Nil): String =
  (entries.map(entry => converter.toPath(entry.data).toString) ++ extra).mkString(java.io.File.pathSeparator)

lazy val checkSource   = "plugin/src/main/scala/net/kurobako/bonsai/plugin/Check.scala"
lazy val findingSource = "analysis/src/main/scala/net/kurobako/bonsai/analysis/Finding.scala"
lazy val lintSource    = "java-plugin/src/main/scala/net/kurobako/bonsai/java/JavaLint.scala"
lazy val ruleSource    = "scalafix/rules/src/main/resources/META-INF/services/scalafix.v1.Rule"

lazy val lawIds =
  taskKey[Seq[String]]("The ids of the laws of collection chains, which name the checks that report them.")
lazy val javaLawIds =
  taskKey[Seq[String]](
    "The ids of the laws of collection chains that read as Java, which the lints of the Java code use."
  )

/** A resource listing the ids the sources spell and the `extra` ids, one a line, so that a plugin cannot drift from
  * them.
  */
def idResource(name: String, extra: Def.Initialize[Task[Seq[String]]], sources: (String, scala.util.matching.Regex)*) =
  Compile / resourceGenerators += Def.task {
    val file = (Compile / resourceManaged).value / s"META-INF/bonsai/$name.id"
    val base = (ThisBuild / baseDirectory).value
    val ids  = sources.flatMap { case (source, pattern) => BonsaiIds.of(base / source, pattern) } ++ extra.value
    IO.write(file, ids.mkString("\n"))
    Seq(file)
  }.taskValue

def idResource(name: String, sources: (String, scala.util.matching.Regex)*): Setting[?] =
  idResource(name, Def.task(Seq.empty[String]), sources *)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-no-indent",
    "-deprecation",
    "-Wconf:cat=unchecked:error",
    "-Wconf:name=MatchCaseUnreachable:error",
    "-Wconf:name=PatternMatchExhaustivity:error",
    "-Wnonunit-statement",
    "-Wvalue-discard",
    "-Wunused:all",
    "-Yretain-trees",
    "-language:noAutoTupling"
  ),
  libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test,
  Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "--log=warn", "--logger=sbt"),
  scalafmtDetailedError := true
)

/** The algebra of collection chains: the laws and costs the compiler plugin and the analysis share. The plugin builds
  * on it, so it does not lint it; the whole-build analysis does.
  */
lazy val collections = linted(project.in(file("collections")))
  .settings(
    commonSettings,
    name                                   := "bonsai-collections",
    crossScalaVersions                     := scala3Versions,
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test,
    lawIds                                 := BonsaiIds.laws((Compile / fullClasspath).value.map(entry =>
      fileConverter.value.toPath(entry.data).toFile
    )),
    javaLawIds := BonsaiIds.laws(
      (Compile / fullClasspath).value.map(entry => fileConverter.value.toPath(entry.data).toFile),
      "javaIds"
    )
  )

/** The effects of calls to code with no body to read, and what throws: the axioms the compiler plugin and the analysis
  * share. It is free of the compiler, so the plugin bundles it.
  */
lazy val effects = linted(project.in(file("effects")))
  .settings(
    commonSettings,
    name               := "bonsai-effects",
    crossScalaVersions := scala3Versions
  )

/** `.bonsai.conf`, which the sbt plugin and the analysis read alike. It is built with the Scala version of sbt, whose
  * plugins depend on it, and the newer Scala of the analysis reads its TASTy.
  */
lazy val bonsaiConfig = linted(project.in(file("configuration")))
  .settings(
    name         := "bonsai-configuration",
    scalaVersion := sbtScalaVersion,
    scalacOptions ++= Seq("-deprecation", "-Wunused:all"),
    libraryDependencies ++= Seq(
      "com.typesafe"   % "config" % "1.4.3",
      "org.scalameta" %% "munit"  % munitVersion % Test
    ),
    Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "--log=warn", "--logger=sbt")
  )

/** Helpers the test suites share, such as scratch directories that do not outlive the test JVM. */
lazy val testSupport = linted(project.in(file("test-support")))
  .settings(
    commonSettings,
    name               := "bonsai-test-support",
    crossScalaVersions := scala3Versions,
    publish / skip     := true
  )

/** Lifts typed Scala 3 trees into the chains of the algebra, for the compiler plugin, which bundles it, and the
  * analysis. It compiles against the compiler of the build.
  */
lazy val lifting = linted(project.in(file("lifting")))
  .dependsOn(collections, effects, testSupport % Test)
  .settings(
    commonSettings,
    perLine,
    name        := "bonsai-lifting",
    Test / fork := true,
    Test / javaOptions +=
      "-Dbonsai.lifting.classpath=" + classpathOf((Test / fullClasspath).value, fileConverter.value),
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Provided,
      "org.scalameta"  %% "munit"           % munitVersion       % Test
    )
  )

lazy val rules = project.in(file("scalafix/rules"))
  .settings(
    name               := "bonsai-scalafix",
    scalaVersion       := scalafixScalaVersion,
    crossScalaVersions := scala3Versions,
    // The rules run in the Scala library of the scalafix that loads them, which a newer library may lack methods of; the
    // other versions the build switches to, so that the plugin resolves, must not publish them.
    publish / skip      := scalaVersion.value != scalafixScalaVersion,
    publishLocal / skip := scalaVersion.value != scalafixScalaVersion,
    scalacOptions ++= Seq("-deprecation", "-Wunused:all"),
    // The rules lint themselves from their own classes, which a project dependency on itself cannot express.
    ScalafixConfig / products := (Compile / products).value,
    libraryDependencies +=
      ("ch.epfl.scala" %% "scalafix-core" % _root_.scalafix.sbt.BuildInfo.scalafixVersion)
        .cross(CrossVersion.for3Use2_13),
    idResource("rules", ruleSource -> BonsaiIds.Rules),
    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "META-INF/bonsai/versions.properties"
      IO.write(
        file,
        s"scalafix.version=${_root_.scalafix.sbt.BuildInfo.scalafixVersion}\n" +
          s"scalafix.scalaVersion=$scalafixScalaVersion\n"
      )
      Seq(file)
    }.taskValue
  )

lazy val fixtureSettings = Seq(
  scalaVersion   := scalafixScalaVersion,
  publish / skip := true,
  scalacOptions ++= Seq("-deprecation", "-feature"),
  libraryDependencies += "org.scalameta" %% "munit" % munitVersion
)

// The fixtures hold code that breaks the rules on purpose, so no rule lints them.
lazy val ruleInput = project.in(file("scalafix/input"))
  .disablePlugins(_root_.scalafix.sbt.ScalafixPlugin)
  .settings(fixtureSettings)

lazy val ruleOutput = project.in(file("scalafix/output"))
  .disablePlugins(_root_.scalafix.sbt.ScalafixPlugin)
  .settings(fixtureSettings)

lazy val ruleTests = linted(project.in(file("scalafix/tests")))
  .dependsOn(ruleInput, rules)
  .enablePlugins(_root_.scalafix.sbt.ScalafixTestkitPlugin)
  .settings(
    scalaVersion   := scalafixScalaVersion,
    publish / skip := true,
    scalacOptions += "-Wunused:all",
    scalafixTestkitOutputSourceDirectories := (ruleOutput / Compile / sourceDirectories).value,
    scalafixTestkitInputSourceDirectories  := (ruleInput / Compile / sourceDirectories).value,
    scalafixTestkitInputClasspath          := (ruleInput / Compile / fullClasspath).value,
    scalafixTestkitInputScalacOptions      := (ruleInput / Compile / scalacOptions).value,
    scalafixTestkitInputScalaVersion       := (ruleInput / Compile / scalaVersion).value,
    Test / resourceGenerators += Def.task {
      val converter  = fileConverter.value
      val semanticdb = (ruleOutput / Compile / scalacOptions).value.sliding(2).collect {
        case Seq("-semanticdb-target", directory) => directory
      }.toSeq
      val classpath = classpathOf((ruleOutput / Compile / fullClasspath).value, converter, semanticdb)
      val file      = (Test / managedResourceDirectories).value.head / "bonsai-output-classpath.txt"
      IO.write(file, classpath)
      Seq(file)
    }.taskValue
  )

lazy val plugin = linted(project.in(file("plugin")))
  .dependsOn(lifting % "compile-internal")
  .dependsOn(effects % "compile-internal")
  // The ids of the lints of the Java code, which the checks of the plugin must not be the ids of.
  .dependsOn(javaPlugin % Test, testSupport % Test)
  .settings(
    perLine,
    name := "bonsai-plugin",
    Compile / packageBin / mappings ++=
      (lifting / Compile / packageBin / mappings).value ++ (collections / Compile / packageBin / mappings).value ++
        (effects / Compile / packageBin / mappings).value,
    Compile / packageBin := {
      val packaged      = (Compile / packageBin).value
      val converter     = fileConverter.value
      val original      = converter.toPath(packaged)
      val fingerprinted =
        original.resolveSibling(s"bonsai-plugin-${sbt.io.Hash.toHex(sbt.io.Hash(original.toFile))}.jar")
      java.nio.file.Files.copy(original, fingerprinted, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      val output = converter.toVirtualFile(fingerprinted)
      Def.declareOutput(output)
      output
    },
    Compile / exportJars := true,
    // The compilers before 3.5 have no replacement for the deprecated `StandardPlugin.init`.
    scalacOptions ++= Seq("-deprecation", "-Wunused:all", "-Wconf:cat=deprecation&msg=StandardPlugin:s"),
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Provided,
      "org.scalameta"  %% "munit"           % munitVersion       % Test
    ),
    idResource("checks", collections / lawIds, checkSource -> BonsaiIds.Checks),
    idResource(
      "oldest",
      Def.task(Seq(oldestPatch(scalaLine(scalaVersion.value))))
    ),
    Test / fork := true,
    Test / javaOptions ++= {
      val converter = fileConverter.value
      Seq(
        s"-Dbonsai.plugin.jar=${converter.toPath((Compile / packageBin).value)}",
        s"-Dbonsai.chains=${(Test / resourceDirectory).value / "chains"}",
        "-Dbonsai.plugin.classpath=" + classpathOf((Test / fullClasspath).value, converter)
      )
    }
  )

/** A module linted by bonsai's scalafix rules, which scalafix loads from the sources of this build. The compiler plugin
  * cannot check the modules it is built from or tested with, so these are linted only.
  */
def linted(module: Project): Project = module.dependsOn(rules % ScalafixConfig)

/** A module linted by bonsai's scalafix rules and checked by its own compiler plugin, with every check an error. */
def analysed(module: Project): Project = linted(module)
  .dependsOn(plugin % "plugin->compile")
  .settings(commonSettings, scalacOptions += "-P:bonsai:error=*")

lazy val analysis = analysed(project.in(file("analysis")))
  .dependsOn(javaPlugin, collections, effects, lifting, bonsaiConfig, testSupport % Test)
  .settings(
    name                                    := "bonsai-analysis",
    Compile / mainClass                     := Some("net.kurobako.bonsai.analysis.CLI"),
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    // The newest TASTy the analysis reads is the one of its own compiler.
    idResource("scala", Def.task(Seq(scalaVersion.value))),
    // The lints of the Java code are reported as findings of their own, so their ids are published alike.
    idResource(
      "findings",
      collections / javaLawIds,
      findingSource -> BonsaiIds.Findings,
      lintSource    -> BonsaiIds.Lints
    ),
    run / fork := true,
    run / javaOptions += "-Xmx8g",
    Test / fork := true,
    // Each test runner compiles the suites' fixtures before it is warm, so past half the cores more runners only
    // repeat that work.
    strykerConcurrency := (sys.runtime.availableProcessors / 2).max(1),
    // Stryker compiles mutated copies of the sources, whose mutation switching makes cases unreachable and trips the
    // compiler lints this build makes errors; it lifts -Werror itself, but not errors chosen by -Wconf.
    scalacOptions := {
      val mutating = (Compile / unmanagedSourceDirectories).value.exists(_.getPath.contains("stryker4s-"))
      scalacOptions.value.filterNot(option => mutating && option.startsWith("-Wconf:") && option.endsWith(":error"))
    },
    Test / javaOptions +=
      "-Dbonsai.analysis.classpath=" + classpathOf((Test / fullClasspath).value, fileConverter.value),
    analysisClasspath := Def.uncached(println(classpathOf((Runtime / fullClasspath).value, fileConverter.value)))
  )

lazy val javaPlugin = linted(project.in(file("java-plugin")))
  .dependsOn(collections, testSupport % Test)
  .settings(
    commonSettings,
    name               := "bonsai-java-plugin",
    crossScalaVersions := scala3Versions,
    // The plugin runs inside the compiler of the build, so it must not drag the compiler of the tools along.
    libraryDependencies += "org.scala-lang" %% "scala3-library" % scalaVersion.value,
    // The nullness lint reads the JSpecify annotations the code of the build writes.
    libraryDependencies += "org.jspecify" % "jspecify" % "1.0.1" % Test,
    // The log-message lint reads the SLF4J logger methods of the code of the build.
    libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.18" % Test,
    Test / fork                       := true
  )

lazy val sbtBonsai = linted(project.in(file("sbt-plugin")))
  .enablePlugins(SbtPlugin)
  .dependsOn(bonsaiConfig)
  .settings(
    name         := "sbt-bonsai",
    scalaVersion := sbtScalaVersion,
    scalacOptions ++= Seq("-deprecation", "-Wunused:all"),
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test,
    addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % Versions.scalafix),
    Compile / sourceGenerators += Def.task {
      val file = (Compile / sourceManaged).value / "BonsaiArtifacts.scala"
      val base = (ThisBuild / baseDirectory).value
      // The ids each tool accepts, read from the tools themselves so the sbt plugin cannot drift from them.
      def ids(source: File, pattern: scala.util.matching.Regex): Seq[String] =
        BonsaiIds.of(source, pattern).map(id => s"\"$id\"")
      val checks = ids(base / checkSource, BonsaiIds.Checks) ++ (collections / lawIds).value.map(id => s"\"$id\"")
      // The lints of the Java code are reported as findings of their own, so their ids are published alike.
      val findings = ids(base / findingSource, BonsaiIds.Findings) ++ ids(base / lintSource, BonsaiIds.Lints) ++
        (collections / javaLawIds).value.map(id => s"\"$id\"")
      val scalafix = ids(base / ruleSource, BonsaiIds.Rules)
      IO.write(
        file,
        s"""package net.kurobako.bonsai.sbt
           |
           |private object BonsaiArtifacts {
           |  val organization: String       = "${organization.value}"
           |  val version: String            = "${version.value}"
           |  val lines: Seq[String]         = Seq(${scala3Versions.map(version =>
            s"\"${scalaLine(version)}\""
          ).mkString(", ")})
           |  val oldest: Map[String, String] = Map(${oldestPatches.map((line, version) =>
            s"\"$line\" -> \"$version\""
          ).mkString(", ")})
           |  val analysisScala: String      = "$scala3Version"
           |  val checks: Set[String]        = Set(${checks.mkString(", ")})
           |  val findings: Set[String]      = Set(${findings.mkString(", ")})
           |  val rules: Seq[String]         = Seq(${scalafix.mkString(", ")})
           |}
           |""".stripMargin
      )
      Seq(file)
    }.taskValue,
    scriptedLaunchOpts ++= Seq(
      "-Xmx4g",
      s"-Dplugin.version=${version.value}",
      s"-Dbonsai.oldest=${scala3Versions.map(scalaLine).map(line => s"$line:${oldestPatch(line)}").mkString(",")}",
      s"-Dbonsai.fixtures=${(ThisBuild / baseDirectory).value}"
    ),
    scriptedBufferLog    := false,
    scriptedDependencies := Def.uncached {
      val _ = (
        (rules / publishLocal).value,
        (plugin / publishLocal).value,
        (analysis / publishLocal).value,
        (bonsaiConfig / publishLocal).value,
        publishLocal.value
      )
    }
  )

lazy val root = project.in(file("."))
  .disablePlugins(_root_.scalafix.sbt.ScalafixPlugin)
  .aggregate(
    collections,
    effects,
    lifting,
    rules,
    ruleInput,
    ruleOutput,
    ruleTests,
    plugin,
    analysis,
    javaPlugin,
    testSupport,
    bonsaiConfig,
    sbtBonsai
  )
  .settings(
    name                  := "bonsai",
    publish / skip        := true,
    analysisConfiguration := Def.uncached(BonsaiConfiguration.read((ThisBuild / baseDirectory).value / ".bonsai.conf")),
    analysisManifest      := Def.uncached {
      val base       = (ThisBuild / baseDirectory).value
      val configured = analysisConfiguration.value
      val fixtures   = Fixtures.fixtures(buildStructure.value)
      val outputs    = Def.task {
        val _ = (compile.value, copyResources.value)
        (thisProject.value.id, configuration.value.name == Test.name, classDirectory.value, unmanagedSources.value)
      }.all(analysedOutputs).value.flatMap { (id, test, classes, sources) =>
        val scope = ManifestLines.scope(fixtures, id, test)
        ManifestLines.outputs(id, scope, classes.toString, configured.library(id).getOrElse(false)) ++
          ManifestLines.sources(id, scope, sources.map(_.toString))
      }
      val classpath = Def.task {
        val converter = fileConverter.value
        thisProject.value.id -> externalDependencyClasspath.value.map(entry => converter.toPath(entry.data).toString)
      }.all(analysedOutputs).value.groupMap(_._1)(_._2).toSeq.flatMap((id, entries) =>
        ManifestLines.classpath(id, entries.flatten.distinct)
      )
      val options = Def.task((thisProject.value.id, scalacOptions.value)).all(analysedOutputs).value
        .flatMap((id, flags) => ManifestLines.options(id, flags)).distinct
      val manifest = target.value / "bonsai" / "manifest.tsv"
      IO.write(
        manifest,
        ManifestLines.render(outputs ++ classpath ++ options ++ configured.text, base)
      )
      manifest
    },
    analysisArguments := Def.uncached {
      val base       = (ThisBuild / baseDirectory).value
      val configured = analysisConfiguration.value
      val findings   =
        (BonsaiIds.of(base / findingSource, BonsaiIds.Findings) ++
          BonsaiIds.of(base / lintSource, BonsaiIds.Lints) ++ (collections / javaLawIds).value).toSet
      val generated = sourceManaged.all(analysedOutputs).value.map(BonsaiConfiguration.generatedGlob(base, _)).toSet
      val built     = configured.analysis(configured.build)
      val chosen    = built.copy(generated = built.generated ++ generated)
      val arguments = Seq("--manifest", analysisManifest.value.toString) ++ chosen.options(
        findings,
        base / configured.baseline.getOrElse(BonsaiConfiguration.DefaultBaseline),
        base / configured.purity.getOrElse(BonsaiConfiguration.DefaultPurity)
      )
      val file = target.value / "bonsai" / "arguments"
      IO.writeLines(file, arguments)
      file
    },
    analyse := Def.uncached(Def.taskDyn {
      val arguments = IO.readLines(analysisArguments.value) :+ "--error" :+ "*"
      (analysis / Compile / run).toTask(arguments.map(argument => s" \"$argument\"").mkString)
    }.value)
  )
