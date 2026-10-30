// The compiler plugin and the analysis link against the compiler, so each is published for every version listed.
lazy val scala3Versions = Seq("3.9.0", "3.10.0")
lazy val scala3Version  = scala3Versions.head
lazy val munitVersion   = "1.3.6"

ThisBuild / organization      := "net.kurobako"
ThisBuild / version           := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion      := scala3Version
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

lazy val analysisManifest  = taskKey[File]("Write the whole-build analysis manifest of this build.")
lazy val analyse           = taskKey[Unit]("Run the whole-build analysis over this build.")
lazy val analysisClasspath = taskKey[Unit]("Print the classpath that runs the analysis, for scripts to run it with.")

lazy val analysedOutputs =
  ScopeFilter(
    inAggregates(LocalRootProject, includeRoot = false) -- inProjects(ruleInput, ruleOutput),
    inConfigurations(Compile, Test)
  )

def classpathOf(entries: Classpath, converter: xsbti.FileConverter): String =
  entries.map(entry => converter.toPath(entry.data)).mkString(java.io.File.pathSeparator)

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

lazy val rules = project.in(file("scalafix/rules"))
  .settings(
    name := "bonsai-scalafix",
    scalacOptions ++= Seq("-deprecation", "-Wunused:all"),
    // The rules lint themselves from their own classes, which a project dependency on itself cannot express.
    ScalafixConfig / products := (Compile / products).value,
    libraryDependencies +=
      ("ch.epfl.scala" %% "scalafix-core" % _root_.scalafix.sbt.BuildInfo.scalafixVersion)
        .cross(CrossVersion.for3Use2_13),
    // The names the rules have, in a resource a plugin reads, so that it cannot drift from them.
    Compile / resourceGenerators += Def.task {
      val file   = (Compile / resourceManaged).value / "META-INF/bonsai/rules.id"
      val source = (ThisBuild / baseDirectory).value /
        "scalafix/rules/src/main/resources/META-INF/services/scalafix.v1.Rule"
      IO.write(file, BonsaiIds.of(source, BonsaiIds.Rules).mkString("\n"))
      Seq(file)
    }.taskValue
  )

lazy val ruleInput = project.in(file("scalafix/input"))
  .disablePlugins(_root_.scalafix.sbt.ScalafixPlugin)
  .settings(
    publish / skip := true,
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion
  )

lazy val ruleOutput = project.in(file("scalafix/output"))
  .disablePlugins(_root_.scalafix.sbt.ScalafixPlugin)
  .settings(
    publish / skip := true,
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion
  )

lazy val ruleTests = project.in(file("scalafix/tests"))
  .dependsOn(ruleInput, rules)
  .enablePlugins(_root_.scalafix.sbt.ScalafixTestkitPlugin)
  .disablePlugins(_root_.scalafix.sbt.ScalafixPlugin)
  .settings(
    publish / skip                         := true,
    scalafixTestkitOutputSourceDirectories := (ruleOutput / Compile / sourceDirectories).value,
    scalafixTestkitInputSourceDirectories  := (ruleInput / Compile / sourceDirectories).value,
    scalafixTestkitInputClasspath          := (ruleInput / Compile / fullClasspath).value,
    scalafixTestkitInputScalacOptions      := (ruleInput / Compile / scalacOptions).value,
    scalafixTestkitInputScalaVersion       := (ruleInput / Compile / scalaVersion).value,
    Test / resourceGenerators += Def.task {
      val converter = fileConverter.value
      val semanticdb = (ruleOutput / Compile / scalacOptions).value.sliding(2).collect {
        case Seq("-semanticdb-target", directory) => directory
      }.toSeq
      val classpath = ((ruleOutput / Compile / fullClasspath).value
        .map(entry => converter.toPath(entry.data).toString) ++ semanticdb).mkString(java.io.File.pathSeparator)
      val file      = (Test / managedResourceDirectories).value.head / "bonsai-output-classpath.txt"
      IO.write(file, classpath)
      Seq(file)
    }.taskValue
  )

lazy val plugin = project.in(file("plugin"))
  .dependsOn(rules % ScalafixConfig)
  // The ids of the lints of the Java code, which the checks of the plugin must not be the ids of.
  .dependsOn(javaPlugin % "compile->test")
  .settings(
    name                 := "bonsai-plugin",
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
    crossVersion         := CrossVersion.full,
    crossScalaVersions   := scala3Versions,
    scalacOptions ++= Seq("-deprecation", "-Wunused:all"),
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Provided,
      "org.scalameta"  %% "munit"           % munitVersion       % Test
    ),
    // The ids the checks have, in a resource a plugin reads, so that it cannot drift from them.
    Compile / resourceGenerators += Def.task {
      val file   = (Compile / resourceManaged).value / "META-INF/bonsai/checks.id"
      val source = (ThisBuild / baseDirectory).value /
        "plugin/src/main/scala/net/kurobako/bonsai/plugin/Check.scala"
      IO.write(file, BonsaiIds.of(source, BonsaiIds.Checks).mkString("\n"))
      Seq(file)
    }.taskValue,
    Test / fork := true,
    Test / javaOptions ++= {
      val converter = fileConverter.value
      Seq(
        s"-Dbonsai.plugin.jar=${converter.toPath((Compile / packageBin).value)}",
        "-Dbonsai.plugin.classpath=" + classpathOf((Test / fullClasspath).value, converter)
      )
    }
  )

/** A module linted by bonsai's scalafix rules and checked by its own compiler plugin, with every check an error. */
def analysed(module: Project): Project = module
  .dependsOn(rules % ScalafixConfig, plugin % "plugin->compile")
  .settings(commonSettings, scalacOptions += "-P:bonsai:error=*")

lazy val analysis = analysed(project.in(file("analysis")))
  .dependsOn(javaPlugin)
  .settings(
    name                                    := "bonsai-analysis",
    crossVersion                            := CrossVersion.full,
    crossScalaVersions                      := scala3Versions,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    // The ids the rules have, in a resource a plugin reads, so that it cannot drift from them.
    Compile / resourceGenerators += Def.task {
      val file   = (Compile / resourceManaged).value / "META-INF/bonsai/findings.id"
      val source = (ThisBuild / baseDirectory).value /
        "analysis/src/main/scala/net/kurobako/bonsai/analysis/Finding.scala"
      // The lints of the Java code are reported as findings of their own, so their ids are published alike.
      val lints = (ThisBuild / baseDirectory).value /
        "java-plugin/src/main/scala/net/kurobako/bonsai/java/JavaLint.scala"
      val ids = BonsaiIds.of(source, BonsaiIds.Findings) ++ BonsaiIds.of(lints, BonsaiIds.Lints)
      IO.write(file, ids.mkString("\n"))
      Seq(file)
    }.taskValue,
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

lazy val annotation =
  analysed(project.in(file("annotation"))).settings(name := "bonsai-annotation", publish / skip := true)

lazy val javaPlugin = project.in(file("java-plugin"))
  .settings(
    commonSettings,
    name := "bonsai-java-plugin",
    // The plugin runs inside the compiler of the build, so it must not drag the compiler of the tools along.
    libraryDependencies += "org.scala-lang" %% "scala3-library" % scalaVersion.value,
    // The nullness lint reads the JSpecify annotations the code of the build writes.
    libraryDependencies += "org.jspecify" % "jspecify" % "1.0.1" % Test,
    // The log-message lint reads the SLF4J logger methods of the code of the build.
    libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.18" % Test,
    Test / fork                             := true
  )

lazy val sbtBonsai = project.in(file("sbt-plugin"))
  .enablePlugins(SbtPlugin)
  .disablePlugins(_root_.scalafix.sbt.ScalafixPlugin)
  .settings(
    name         := "sbt-bonsai",
    scalaVersion := "3.8.4",
    scalacOptions ++= Seq("-deprecation", "-Wunused:all"),
    addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.9"),
    Compile / sourceGenerators += Def.task {
      val file = (Compile / sourceManaged).value / "BonsaiArtifacts.scala"
      // The ids each tool accepts, read from the tools themselves so the sbt plugin cannot drift from them.
      def ids(source: File, pattern: scala.util.matching.Regex): Seq[String] =
        BonsaiIds.of(source, pattern).map(id => s"\"$id\"")
      val checks = ids(
        (plugin / baseDirectory).value / "src/main/scala/net/kurobako/bonsai/plugin/Check.scala",
        BonsaiIds.Checks
      )
      // The lints of the Java code are reported as findings of their own, so their ids are published alike.
      val findings = ids(
        (analysis / baseDirectory).value / "src/main/scala/net/kurobako/bonsai/analysis/Finding.scala",
        BonsaiIds.Findings
      ) ++ ids(
        (javaPlugin / baseDirectory).value / "src/main/scala/net/kurobako/bonsai/java/JavaLint.scala",
        BonsaiIds.Lints
      )
      val scalafix = ids(
        (rules / baseDirectory).value / "src/main/resources/META-INF/services/scalafix.v1.Rule",
        BonsaiIds.Rules
      )
      IO.write(
        file,
        s"""package net.kurobako.bonsai.sbt
           |
           |private object BonsaiArtifacts {
           |  val organization: String       = "${organization.value}"
           |  val version: String            = "${version.value}"
           |  val scalaVersions: Set[String] = Set(${scala3Versions.map(version => s"\"$version\"").mkString(", ")})
           |  val checks: Set[String]        = Set(${checks.mkString(", ")})
           |  val findings: Set[String]      = Set(${findings.mkString(", ")})
           |  val rules: Seq[String]         = Seq(${scalafix.mkString(", ")})
           |}
           |""".stripMargin
      )
      Seq(file)
    }.taskValue,
    scriptedLaunchOpts ++= Seq("-Xmx4g", s"-Dplugin.version=${version.value}"),
    scriptedBufferLog    := false,
    scriptedDependencies := Def.uncached {
      val _ = (
        (rules / publishLocal).value,
        (plugin / publishLocal).value,
        (analysis / publishLocal).value,
        publishLocal.value
      )
    }
  )

lazy val root = project.in(file("."))
  .disablePlugins(_root_.scalafix.sbt.ScalafixPlugin)
  .aggregate(rules, ruleInput, ruleOutput, ruleTests, plugin, analysis, annotation, javaPlugin, sbtBonsai)
  .settings(
    name             := "bonsai",
    publish / skip   := true,
    analysisManifest := Def.uncached {
      val outputs = Def.task {
        val _     = (compile.value, copyResources.value)
        val scope = if (configuration.value.name == Test.name) "test" else "main"
        s"output\t${thisProject.value.id}\t$scope\t${classDirectory.value}"
      }.all(analysedOutputs).value
      val classpath = Def.task {
        val converter = fileConverter.value
        externalDependencyClasspath.value.map(entry => converter.toPath(entry.data).toString)
      }.all(analysedOutputs).value.flatten.distinct.map(entry => s"classpath\t$entry")
      val manifest = target.value.toPath.resolve("bonsai/manifest.tsv")
      java.nio.file.Files.createDirectories(manifest.getParent)
      java.nio.file.Files.writeString(manifest, (outputs ++ classpath).mkString("", "\n", "\n"))
      manifest.toFile
    },
    analyse := Def.uncached(Def.taskDyn {
      (analysis / Compile / run).toTask(s" --manifest ${analysisManifest.value} --error *")
    }.value)
  )
