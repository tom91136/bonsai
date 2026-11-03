package net.kurobako.bonsai.configuration

import com.typesafe.config.Config
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigUtil
import java.io.File
import scala.jdk.CollectionConverters.*

/** `.bonsai.conf`, read alike by the sbt plugin, by the analysis for the Maven plugin, and by a build that analyses
  * itself without either.
  */
object BonsaiConfiguration {

  /** A configuration that cannot be read, which the message says why. */
  final class ConfigurationError(message: String) extends Exception(message)

  case class Settings(
      disabled: Option[Set[String]],
      errors: Option[Set[String]],
      warnings: Option[Set[String]],
      wildcards: Option[Int],
      library: Option[Boolean]
  )

  case class Configuration(
      build: Settings,
      baseline: Option[String],
      purity: Option[String],
      effects: Option[Map[String, Set[String]]],
      concurrent: Option[Set[String]],
      generated: Option[Set[String]],
      generatedDisabled: Option[Set[String]],
      hot: Option[Set[String]],
      reflective: Option[Set[String]],
      intended: Option[Set[String]],
      checked: Option[Set[String]],
      bounded: Vector[(String, Int, String)],
      nesting: Map[String, Int],
      text: Vector[String],
      projects: Map[String, Settings]
  ) {

    def library(id: String): Option[Boolean] = resolve(id).library

    /** The settings of the module: its own where it names them, the build's elsewhere. */
    def resolve(id: String): Settings = projects.get(id).fold(build)(own =>
      Settings(
        own.disabled.orElse(build.disabled),
        own.errors.orElse(build.errors),
        own.warnings.orElse(build.warnings),
        own.wildcards.orElse(build.wildcards),
        own.library.orElse(build.library)
      )
    )

    def analysis(settings: Settings): Analysis = Analysis(
      disabled = settings.disabled.getOrElse(Set.empty),
      errors = settings.errors.getOrElse(Set.empty),
      warnings = settings.warnings.getOrElse(Set.empty),
      effects = effects.getOrElse(Map.empty),
      concurrent = concurrent.getOrElse(Set.empty),
      generated = generated.getOrElse(Set.empty),
      generatedDisabled = generatedDisabled.getOrElse(Set.empty),
      reflective = reflective.getOrElse(Set.empty),
      hot = hot.getOrElse(Set.empty),
      intended = intended.getOrElse(Set.empty),
      bounded = bounded,
      nesting = nesting
    )
  }

  val DefaultBaseline = "bonsai-baseline.tsv"
  val DefaultPurity   = "bonsai-purity.tsv"

  /** The settings of the whole-build analysis, which it takes as options. */
  case class Analysis(
      disabled: Set[String],
      errors: Set[String],
      warnings: Set[String],
      effects: Map[String, Set[String]],
      concurrent: Set[String],
      generated: Set[String],
      generatedDisabled: Set[String],
      reflective: Set[String],
      hot: Set[String],
      intended: Set[String],
      bounded: Vector[(String, Int, String)],
      nesting: Map[String, Int]
  ) {

    /** The options of these settings, naming of the ids only the `findings` the analysis knows, apart from those
      * disabled over generated code, which the analysis rejects when it does not know them.
      */
    def options(findings: Set[String], baseline: File, purity: File): Seq[String] =
      flags("--disable", disabled & findings) ++ flags("--error", errors & findings) ++
        flags("--warn", warnings & findings) ++
        Seq("--baseline", baseline.getAbsolutePath, "--purity-lock", purity.getAbsolutePath) ++
        effects.toSeq.sortBy(_._1).flatMap((prefix, kinds) =>
          Seq("--effects", s"$prefix=${kinds.toSeq.sorted.mkString(",")}")
        ) ++ flags("--concurrent", concurrent) ++ flags("--generated", generated) ++
        flags("--generated-disable", generatedDisabled) ++
        flags("--reflective", reflective) ++ flags("--hot", hot) ++ flags("--intended", intended) ++
        bounded.flatMap((member, max, reason) => Seq("--bounded", member, max.toString, reason)) ++
        (if (nesting.isEmpty) Nil
         else "--nesting" +:
           nesting.toSeq.sortBy(_._1).flatMap((limit, value) => Seq(s"--nesting-$limit", value.toString)))
  }

  def read(file: File): Configuration =
    try {
      val config   = if (file.exists) ConfigFactory.parseFile(file).resolve() else ConfigFactory.empty()
      val projects = Option.when(config.hasPath("projects"))(config.getConfig("projects")).fold(Map.empty) {
        projects =>
          projects.root.keySet.asScala.toSeq.map(id =>
            id -> settings(projects.getConfig(ConfigUtil.joinPath(id)), s"projects.$id.", Set.empty)
          ).toMap
      }
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
            "generated-disabled",
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
        optionalString(config, "baseline"),
        optionalString(config, "purity"),
        Option.when(config.hasPath("effects"))(config.getConfig("effects")).map(effects =>
          effects.root.keySet.asScala.toSeq.map(prefix =>
            prefix -> effects.getStringList(ConfigUtil.joinPath(prefix)).asScala.toSet
          ).toMap
        ),
        stringSet(config, "concurrent"),
        stringSet(config, "generated"),
        stringSet(config, "generated-disabled"),
        stringSet(config, "hot"),
        stringSet(config, "reflective"),
        stringSet(config, "intended"),
        stringSet(config, "checked"),
        Option.when(config.hasPath("bounded"))(config.getConfigList("bounded").asScala.toVector.map(entry =>
          (entry.getString("member"), entry.getInt("max"), entry.getString("reason"))
        )).getOrElse(Vector.empty),
        nesting(config),
        Option.when(config.hasPath("text"))(config.getConfig("text")).fold(Vector.empty)(texts(_)),
        projects
      )
    } catch {
      case error: ConfigException => throw ConfigurationError(s"bonsai: $file: ${error.getMessage}")
    }

  private def stringSet(config: Config, key: String): Option[Set[String]] =
    Option.when(config.hasPath(key))(config.getStringList(key).asScala.toSet)

  private def optionalString(config: Config, key: String): Option[String] =
    Option.when(config.hasPath(key))(config.getString(key))

  private def rejectUnknown(unknown: Set[String], where: String, keys: Seq[String]): Unit =
    if (unknown.nonEmpty)
      throw ConfigurationError(
        s"bonsai: .bonsai.conf has unknown keys ${unknown.toSeq.sorted.map(where + _).mkString(", ")}; the " +
          s"keys are ${keys.mkString(", ")}"
      )

  private def flags(flag: String, values: Set[String]): Seq[String] =
    values.toSeq.sorted.flatMap(value => Seq(flag, value))

  private val NestingLimits = Seq("depth", "damping", "size")

  /** The limits of the nesting rule the configuration names. */
  private def nesting(config: Config): Map[String, Int] =
    if (!config.hasPath("nesting")) Map.empty
    else {
      val block   = config.getConfig("nesting")
      val unknown = block.root.keySet.asScala.toSet -- NestingLimits
      rejectUnknown(unknown, "nesting.", NestingLimits.map("nesting." + _))
      NestingLimits.view.filter(block.hasPath).map(limit => limit -> block.getInt(limit)).toMap
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
    def known(found: Set[String], keys: Set[String], where: String) =
      rejectUnknown(found -- keys, where, keys.toSeq.sorted)
    // The manifest is tab-separated, one entry a line.
    def plain(value: String) =
      if (value.exists(character => character == '\t' || character == '\n'))
        throw ConfigurationError(s"bonsai: .bonsai.conf text values cannot hold tabs or newlines: $value")
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
    rejectUnknown(unknown, where, (keys ++ others).toSeq.sorted)
    Settings(
      stringSet(config, "disabled"),
      stringSet(config, "errors"),
      stringSet(config, "warnings"),
      Option.when(config.hasPath("wildcards"))(config.getInt("wildcards")),
      Option.when(config.hasPath("library"))(config.getBoolean("library"))
    )
  }
}
