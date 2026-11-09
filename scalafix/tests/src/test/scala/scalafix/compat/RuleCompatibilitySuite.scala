package scalafix.compat

import com.github.difflib.DiffUtils
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import metaconfig.Conf
import metaconfig.typesafeconfig.typesafeConfigMetaconfigParser
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable
import scala.io.Source as IOSource
import scala.jdk.CollectionConverters.*
import scala.meta.*
import scala.meta.io.RelativePath
import scala.util.Using
import scalafix.internal.config.ScalaVersion
import scalafix.internal.config.ScalafixConfig
import scalafix.internal.diff.DiffDisable
import scalafix.internal.reflect.ClasspathOps
import scalafix.internal.symtab.SymbolTable
import scalafix.internal.testkit.CommentAssertion
import scalafix.internal.v1.Args
import scalafix.internal.v1.LazyValue
import scalafix.testkit.SemanticRuleSuite
import scalafix.testkit.TestkitPath
import scalafix.testkit.TestkitProperties
import scalafix.v1
import scalafix.v1.Configuration
import scalafix.v1.RuleDecoder

object RuleCompatibilitySuite {
  enum Stage {
    case Output
    case Input
    case Configured(name: String)
  }

  case class CombinedRewrite(fixture: String, rules: List[String], expected: String)

  case class Key(stage: Stage, fixture: String, id: String)

  case class Candidate(stage: Stage, fixture: String, id: String, detail: String) {
    def key: Key     = Key(stage, fixture, id)
    def show: String = s"  Key($stage, \"$fixture\", \"$id\")  $detail"
  }

  case class Rewrite(file: java.nio.file.Path, hunks: List[(Set[Int], Set[Int])]) {
    def removed: Set[Int] = hunks.flatMap(_._1).toSet
    def created: Set[Int] = hunks.flatMap(_._2).toSet
  }

  case class Finding(rule: String, id: String, line: Int, message: String, rewrites: Boolean)

  case class Fixture(
      path: TestkitPath,
      text: String,
      headerEnd: Int,
      conf: Conf,
      scalafixConfig: ScalafixConfig,
      headerRules: Set[String],
      output: Option[java.nio.file.Path]
  )

  private def describe(found: List[Finding], headerRules: Set[String]): String =
    s"lines ${found.map(_.line).distinct.sorted.mkString(",")} vs ${headerRules.toList.sorted.mkString("+")}: " +
      found.head.message.linesIterator.nextOption().getOrElse("").take(110)

  private def normalise(line: String): String = line.replaceFirst("""\s*//\s*assert:.*$""", "").stripTrailing()

  def hunks(before: Vector[String], after: Vector[String]): List[(Range, Range)] =
    DiffUtils.diff(before.asJava, after.asJava).getDeltas.asScala.toList.map { delta =>
      val (source, target) = (delta.getSource, delta.getTarget)
      (
        source.getPosition until source.getPosition + source.size,
        target.getPosition until target.getPosition + target.size
      )
    }

  def overlay(conf: Conf, rule: String, section: Conf): Conf = conf match {
    case Conf.Obj(values) => Conf.Obj(values.filterNot(_._1 == rule) :+ (rule -> section))
    case other            => other
  }

  val extraConfigurations: List[(String, String, Conf)] = List(
    (
      "qualified-collection-namespaces",
      "QualifiedCollectionNamespaces",
      """namespaces = [ { package = "java.util.concurrent", prefix = "java.util.concurrent", subpackages = false } ]"""
    ),
    ("namespace-imports", "NamespaceImports", """packages = ["java.util", "scala.collection"]"""),
    ("no-package-private", "NoPackagePrivate", """tiers = ["fix"]"""),
    (
      "configured-idioms",
      "ConfiguredIdioms",
      """idioms = [ { pattern = "($v << (64 - $w)) >> (64 - $w)", helper = "fix.ConfiguredIdioms.Bits.signExtend" } ]"""
    ),
    (
      "forbidden-apis",
      "ForbiddenAPIs",
      """symbols = { "java/lang/Runtime#" = "use scala.sys.runtime instead of java.lang.Runtime" }"""
    ),
    ("tooling-comments", "ToolingComments", """phrases = ["would\\s+otherwise", "\\bthe\\s+expansion\\b"]"""),
    (
      "package-boundary",
      "PackageBoundary",
      """boundaries = [ { sources = "isa/ptx/src/", exempt = "/asm/", forbidden = ["org/example/isa/ptx/asm/"] } ]"""
    )
  ).map { case (name, rule, text) => (name, rule, Conf.parseString(name, text).get) }

  private val reconfigured =
    "the expected output was written under the default list, so the reconfigured rule rejects it"
  private val followsConfig =
    "FullyQualifiedReference reads the reconfigured list and now wants the form the default list forbade"

  private def configured(name: String, fixture: String, id: String): Key =
    Key(Stage.Configured(name), s"scala/fix/$fixture.scala", id)

  val combinedRewrites: List[CombinedRewrite] = List(
    CombinedRewrite(
      "EmptyCollectionQualified",
      List("CollectionIdioms", "QualifiedCollectionNamespaces"),
      """package fix
        |
        |import scala.collection.mutable
        |
        |object EmptyCollectionQualified {
        |  val plain = mutable.ArrayBuffer.empty
        |  val typed = mutable.ListBuffer.empty[Int]
        |}""".stripMargin
    )
  )

  val outputArtefacts: Map[Key, String] = Map.empty

  val outputPending: Map[Key, String] = Map.empty

  val inputArtefacts: Map[Key, String] = Map(
    Key(Stage.Input, "scala/fix/EmptyCollectionQualified.scala", "QualifiedCollectionNamespaces.unqualified") ->
      "both rules patch the call; the combined rewrite is checked by combinedRewrites",
    Key(
      Stage.Input,
      "scala/fix/FullyQualifiedReferenceSubpackages.scala",
      "QualifiedCollectionNamespaces.unqualified"
    ) ->
      "the fixture's namespace for scala.collection asks for collection.mutable, which FullyQualifiedReference's import avoids"
  )
  val inputPending: Map[Key, String] = Map.empty

  val configuredArtefacts: Map[Key, String] = Map(
    configured(
      "qualified-collection-namespaces",
      "FullyQualifiedReference",
      "FullyQualifiedReference.fully-qualified-reference"
    ) -> followsConfig,
    configured(
      "qualified-collection-namespaces",
      "FullyQualifiedReference",
      "QualifiedCollectionNamespaces.member-import"
    ) -> reconfigured,
    configured(
      "qualified-collection-namespaces",
      "FullyQualifiedReference",
      "QualifiedCollectionNamespaces.unqualified"
    ) -> reconfigured,
    configured(
      "qualified-collection-namespaces",
      "FullyQualifiedReferenceClash",
      "QualifiedCollectionNamespaces.member-import"
    ) -> reconfigured,
    configured(
      "qualified-collection-namespaces",
      "FullyQualifiedReferenceClash",
      "QualifiedCollectionNamespaces.unqualified"
    ) -> reconfigured,
    configured(
      "qualified-collection-namespaces",
      "FullyQualifiedReferenceNoImport",
      "QualifiedCollectionNamespaces.member-import"
    ) -> reconfigured,
    configured(
      "qualified-collection-namespaces",
      "FullyQualifiedReferenceNoImport",
      "QualifiedCollectionNamespaces.unqualified"
    ) -> reconfigured,
    configured(
      "qualified-collection-namespaces",
      "QualifiedCollectionNamespaces",
      "FullyQualifiedReference.fully-qualified-reference"
    ) -> followsConfig,
    configured(
      "qualified-collection-namespaces",
      "FullyQualifiedReferenceSubpackages",
      "FullyQualifiedReference.fully-qualified-reference"
    ) -> followsConfig,
    configured(
      "qualified-collection-namespaces",
      "QualifiedCollectionNamespacesJavaOnly",
      "FullyQualifiedReference.fully-qualified-reference"
    )                                                                                               -> followsConfig,
    configured("namespace-imports", "FullyQualifiedReference", "NamespaceImports.namespace-import") -> reconfigured,
    configured("namespace-imports", "QualifiedCollectionNamespacesJavaOnly", "NamespaceImports.qualified-reference") ->
      reconfigured,
    configured("namespace-imports", "FullyQualifiedReferenceClash", "NamespaceImports.namespace-import") ->
      reconfigured,
    configured("namespace-imports", "FullyQualifiedReferenceConfigured", "NamespaceImports.namespace-import") ->
      reconfigured,
    configured("namespace-imports", "FullyQualifiedReferenceNoImport", "NamespaceImports.namespace-import") ->
      reconfigured,
    configured("namespace-imports", "FullyQualifiedReferenceSubpackages", "NamespaceImports.namespace-import") ->
      reconfigured
  )

  val configuredPending: Map[Key, String] = Map.empty
}

/** Runs every registered rule over the fixtures to find rules that reject or rewrite each other's results. */
class RuleCompatibilitySuite extends AnyFunSuite {
  import RuleCompatibilitySuite.*

  private val props   = TestkitProperties.loadFromResources()
  private val outputs = Classpath(readResource("bonsai-output-classpath.txt"))
  private val args    = Args.default.copy(
    scalaVersion = ScalaVersion.from(props.scalaVersion).get,
    scalacOptions = props.scalacOptions,
    classpath = props.inputClasspath
  )
  private val inputLoader  = ClasspathOps.toClassLoader(args.validatedClasspath)
  private val inputSymtab  = ClasspathOps.newSymbolTable(props.inputClasspath)
  private val outputLoader = ClasspathOps.toClassLoader(outputs)
  private val outputSymtab = ClasspathOps.newSymbolTable(outputs)
  private val ruleNames    = registeredRules()
  private val stats        = mutable.LinkedHashMap.empty[String, Int].withDefaultValue(0)

  private val fixtures: List[Fixture] = TestkitPath.fromProperties(props).sortBy(_.testName).map { path =>
    val text                          = Files.readString(path.input.toNIO)
    val (header, conf, rulesConf, sc) = SemanticRuleSuite.parseTestkitCommentOpt(path.toInput.text.tokenize.get).get
    val headerRules                   =
      rulesConf.as[List[String]].orElse(rulesConf.as[String].map(List(_))).getOrElse(Nil).toSet
    Fixture(path, text, header.pos.endLine, conf, sc, headerRules, path.resolveOutput(props).toOption.map(_.toNIO))
  }

  private def readResource(name: String): String =
    Using.resource(getClass.getClassLoader.getResourceAsStream(name))(stream =>
      IOSource.fromInputStream(stream, StandardCharsets.UTF_8.name).mkString
    )

  private def registeredRules(): List[String] =
    readResource("META-INF/services/scalafix.v1.Rule").linesIterator.map(_.trim).filter(_.nonEmpty)
      .map(_.split('.').last).toList.sorted

  private def document(
      name: String,
      text: String,
      sc: ScalafixConfig,
      relative: RelativePath,
      loader: ClassLoader,
      symtab: SymbolTable
  ): v1.SemanticDocument = {
    val input = Input.VirtualFile(name, text)
    val tree  = args.scalaVersion.dialect(args.sourceScalaVersion)(input).parse[Source].get
    val doc   = v1.SyntacticDocument(tree.pos.input, LazyValue.now(tree), DiffDisable.empty, sc)
    v1.SemanticDocument.fromPath(doc, relative, loader, symtab)
  }

  private def inputDocument(f: Fixture): v1.SemanticDocument =
    document(f.path.testName, f.text, f.scalafixConfig, f.path.semanticdbPath, inputLoader, inputSymtab)

  private def outputDocument(f: Fixture, file: java.nio.file.Path): v1.SemanticDocument = {
    val relative = RelativePath(Paths.get(f.path.semanticdbPath.toString.replace("/input/", "/output/")))
    document(
      f.path.testName,
      Files.readString(file),
      f.scalafixConfig,
      relative,
      outputLoader,
      outputSymtab
    )
  }

  private val decoded = mutable.Map.empty[(String, Conf), scalafix.internal.v1.Rules]

  private def findings(rule: String, conf: Conf, f: Fixture, sdoc: v1.SemanticDocument): List[Finding] = {
    val rules = decoded.getOrElseUpdate(
      (rule, conf), {
        val decoder = RuleDecoder.decoder(RuleDecoder.Settings().withConfig(f.scalafixConfig))
        val config  = Configuration().withConf(conf).withScalaVersion(args.scalaVersion.value)
          .withScalacOptions(args.scalacOptions).withScalacClasspath(args.classpath.entries)
        decoder.read(Conf.Lst(Conf.Str(rule))).get.withConfiguration(config).get
      }
    )
    rules.beforeStart()
    val result =
      try rules.semanticPatch(sdoc, suppress = false)
      finally rules.afterComplete()
    val linted = result.diagnostics.map(d =>
      Finding(rule, d.id.fullID, d.position.startLine + 1, d.message, result.fixed != sdoc.input.text)
    )
    if (linted.nonEmpty || result.fixed == sdoc.input.text) linted
    else {
      val before = sdoc.input.text.linesIterator.toVector
      val after  = result.fixed.linesIterator.toVector
      val lines  = hunks(before, after).flatMap(hunk => hunk._1.map(_ + 1))
      (if (lines.isEmpty) List(1) else lines).map(Finding(rule, rule, _, "rewritten", rewrites = true))
    }
  }

  private def allFindings(conf: Conf, f: Fixture, sdoc: v1.SemanticDocument): List[Finding] =
    ruleNames.flatMap(findings(_, conf, f, sdoc))

  private def rewrite(f: Fixture): Option[Rewrite] = f.output.map { file =>
    val original  = f.text.linesIterator.toVector
    val body      = original.indices.drop(f.headerEnd + 1).toVector
    val rewritten = Files.readString(file).linesIterator.toVector.map(normalise(_))
    Rewrite(
      file,
      hunks(body.map(i => normalise(original(i))), rewritten).map { case (gone, made) =>
        (gone.map(body(_) + 1).toSet, made.map(_ + 1).toSet)
      }
    )
  }

  private def introduced(
      r: Rewrite,
      header: Set[String],
      before: List[Finding],
      after: List[Finding]
  ): List[Finding] = {
    val idsAt  = before.groupMap(_.line)(_.id)
    val known  = r.hunks.toVector.map((gone, _) => gone.flatMap(idsAt.getOrElse(_, Nil)))
    val hunkAt = r.hunks.zipWithIndex.flatMap { case ((_, made), hunk) => made.map(_ -> hunk) }.toMap
    after.flatMap(x => hunkAt.get(x.line).filter(hunk => header(x.rule) || !known(hunk)(x.id)).map(_ -> x))
      .sortBy(_._1).map(_._2)
  }

  private def baseline(): Map[String, Set[(String, Int)]] = fixtures.flatMap { f =>
    rewrite(f).map(r =>
      f.path.testName -> allFindings(f.conf, f, outputDocument(f, r.file)).map(x => (x.id, x.line)).toSet
    )
  }.toMap

  private def outputCandidates(): List[Candidate] = fixtures.flatMap { f =>
    rewrite(f).toList.flatMap { r =>
      val found = allFindings(f.conf, f, outputDocument(f, r.file))
      val fresh = introduced(r, f.headerRules, allFindings(f.conf, f, inputDocument(f)), found)
      count("output findings", found.size)
      count("output findings on created lines", found.count(x => r.created(x.line)))
      count("output findings introduced by the rewrite", fresh.size)
      fresh.groupBy(_.id).toList.sortBy(_._1).map { case (id, hits) =>
        Candidate(Stage.Output, f.path.testName, id, describe(hits, f.headerRules))
      }
    }
  }

  private def inputCandidates(): List[Candidate] = fixtures.flatMap { f =>
    rewrite(f).toList.flatMap { r =>
      val sdoc     = inputDocument(f)
      val asserted = CommentAssertion.extract(sdoc.tokens).map(_.anchorPosition.startLine + 1).toSet
      ruleNames.view.filterNot(f.headerRules).flatMap { rule =>
        val all     = findings(rule, f.conf, f, sdoc)
        val found   = all.filterNot(x => asserted(x.line) && !x.rewrites)
        val touched = found.filter(x => r.removed(x.line))
        count("input findings of other rules", all.size)
        count("input findings of other rules on asserted lines", all.size - found.size)
        count("input findings of other rules on rewritten lines", touched.size)
        touched.groupBy(_.id).toList.sortBy(_._1).map { case (id, hits) =>
          Candidate(Stage.Input, f.path.testName, id, describe(hits, f.headerRules))
        }
      }.toList
    }
  }

  private def configuredCandidates(base: Map[String, Set[(String, Int)]]): List[Candidate] =
    extraConfigurations.flatMap { case (name, rule, section) =>
      fixtures.flatMap { f =>
        rewrite(f).toList.flatMap { r =>
          val conf    = overlay(f.conf, rule, section)
          val known   = base(f.path.testName)
          val found   = allFindings(conf, f, outputDocument(f, r.file))
          val changed = found.filterNot(x => known((x.id, x.line)))
          val fresh   = introduced(r, f.headerRules, allFindings(conf, f, inputDocument(f)), found)
          val chosen  = changed.filterNot(_.rule == rule) ++ fresh.filter(_.rule == rule)
          count(s"$name new findings", changed.size)
          chosen.groupBy(_.id).toList.sortBy(_._1).map { case (id, hits) =>
            Candidate(Stage.Configured(name), f.path.testName, id, describe(hits, Set(rule)))
          }
        }
      }
    }

  private def count(key: String, n: Int): Unit = stats(key) += n

  private def check(found: List[Candidate], artefacts: Map[Key, String], pending: Map[Key, String]): Unit = {
    val keys = found.map(_.key).toSet
    info(stats.map { case (k, v) => s"$k: $v" }.mkString("; "))
    val unexplained = found.filterNot(c => artefacts.contains(c.key) || pending.contains(c.key))
    val stale       = (artefacts.keySet ++ pending.keySet).diff(keys)
    assert(unexplained.isEmpty, unexplained.map(_.show).mkString("unexplained candidates:\n", "\n", ""))
    assert(stale.isEmpty, stale.toList.sortBy(_.toString).mkString("entries without a candidate:\n", "\n", ""))
  }

  test("rules rewriting the same tokens combine into the expected output") {
    combinedRewrites.foreach { case CombinedRewrite(fixture, rules, expected) =>
      val f      = fixtures.find(_.path.testName == s"scala/fix/$fixture.scala").get
      val sdoc   = inputDocument(f)
      val config = Configuration().withConf(f.conf).withScalaVersion(args.scalaVersion.value)
        .withScalacOptions(args.scalacOptions).withScalacClasspath(args.classpath.entries)
      val decoder = RuleDecoder.decoder(RuleDecoder.Settings().withConfig(f.scalafixConfig))
      val decoded = decoder.read(Conf.Lst(rules.map(Conf.Str(_)))).get.withConfiguration(config).get
      decoded.beforeStart()
      val result =
        try decoded.semanticPatch(sdoc, suppress = false)
        finally decoded.afterComplete()
      val body = result.fixed.linesIterator.drop(f.headerEnd + 1).map(normalise(_)).mkString("\n")
      assert(body == expected, s"$fixture with ${rules.mkString("+")}")
    }
  }

  test("every registered rule accepts the expected output of every fixture") {
    stats.clear()
    val found = outputCandidates()
    check(found, outputArtefacts, outputPending)
  }

  test("findings on input lines rewritten by another rule are accounted for") {
    stats.clear()
    check(inputCandidates(), inputArtefacts, inputPending)
  }

  test("extra configurations leave the expected outputs accepted") {
    stats.clear()
    val base = baseline()
    check(configuredCandidates(base), configuredArtefacts, configuredPending)
  }
}
