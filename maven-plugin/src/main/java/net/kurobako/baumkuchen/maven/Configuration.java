package net.kurobako.baumkuchen.maven;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigFactory;
import com.typesafe.config.ConfigUtil;
import com.typesafe.config.ConfigValue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * The baumkuchen configuration of a build, which the tools take their options from.
 *
 * <p>The configuration is the HOCON the sbt plugin reads, so both builds configure baumkuchen the same way. A
 * {@code projects.<id>} block overrides the build's settings for one module, as it does in sbt, and a module without
 * such a block takes the build's.
 */
public final class Configuration {

  /** The kinds of effect a policy may allow. */
  private static final Set<String> EFFECTS = Set.of("reads", "writes", "input", "output", "random", "unknown");

  /** The settings the build itself and a project of it may hold. */
  private static final Set<String> SETTINGS =
    Set.of("disabled", "errors", "warnings", "wildcards", "library");

  /** The limits of the nesting rule. */
  private static final List<String> NESTING = List.of("depth", "damping", "size");

  private final Set<String> disabled;
  private final Set<String> errors;
  private final Set<String> warnings;
  private final int wildcards;
  private final Path baseline;
  private final Path purity;
  private final Map<String, Set<String>> effects;
  private final Set<String> concurrent;
  private final Set<String> generated;
  private final Set<String> hot;
  private final Set<String> reflective;
  private final Set<String> checked;
  private final List<Bounded> bounded;
  private final Map<String, Integer> nesting;
  private final boolean library;
  private final List<TextRule> textRules;
  private final List<String> textExclusions;

  private Configuration(Settings settings, Settings own, Path root, Config config) throws IOException {
    this.disabled = orEmpty(own == null ? null : own.disabled, settings.disabled);
    this.errors = orEmpty(own == null ? null : own.errors, settings.errors);
    this.warnings = orEmpty(own == null ? null : own.warnings, settings.warnings);
    this.wildcards = own != null && own.wildcards != null ? own.wildcards
      : settings.wildcards != null ? settings.wildcards : 2;
    this.baseline = root.resolve(settings.baseline);
    this.purity = root.resolve(settings.purity);
    this.effects = settings.effects;
    this.concurrent = orEmpty(null, settings.concurrent);
    this.generated = orEmpty(null, settings.generated);
    this.hot = orEmpty(null, settings.hot);
    this.reflective = orEmpty(null, settings.reflective);
    this.checked = orEmpty(null, settings.checked);
    this.bounded = settings.bounded;
    this.nesting = settings.nesting;
    this.library = own != null && own.library != null ? own.library
      : settings.library != null ? settings.library : false;
    this.textRules = TextRule.read(config, root);
    this.textExclusions = TextRule.exclusions(config);
  }

  /** The set of a setting, or the build's, or the empty one. */
  private static Set<String> orEmpty(Set<String> own, Set<String> build) {
    if (own != null) {
      return own;
    }
    return build == null ? Set.of() : build;
  }

  /** The ids the tools leave out. */
  public Set<String> disabled() {
    return disabled;
  }

  /** The ids the tools fail on in every mode. */
  public Set<String> errors() {
    return errors;
  }

  /** The ids the tools report without failing. */
  public Set<String> warnings() {
    return warnings;
  }

  /** The positional wildcards a case class pattern may carry. */
  public int wildcards() {
    return wildcards;
  }

  /** The file the findings the build already has are recorded in. */
  public Path baseline() {
    return baseline;
  }

  /** The file the methods the build records as pure are in. */
  public Path purity() {
    return purity;
  }

  /** The kinds of effect the methods of a package or class may have, by its qualified name. */
  public Map<String, Set<String>> effects() {
    return effects;
  }

  /** The methods that run the functions passed to them on other threads. */
  public Set<String> concurrent() {
    return concurrent;
  }

  /** The globs of the sources the build generates. */
  public Set<String> generated() {
    return generated;
  }

  /** The globs of the classes whose code runs hot. */
  public Set<String> hot() {
    return hot;
  }

  /** The globs of the classes that code outside the sources names. */
  public Set<String> reflective() {
    return reflective;
  }

  /** The refinement types whose checked operations a conversion can bypass. */
  public Set<String> checked() {
    return checked;
  }

  /** The members whose collections hold a declared number of elements. */
  public List<Bounded> bounded() {
    return bounded;
  }

  /** The limits of the nesting rule by name, which the analysis' defaults take the place of when one is left out. */
  public Map<String, Integer> nesting() {
    return nesting;
  }

  /** Whether other builds use this module's public API, so that the analysis counts it as used. */
  public boolean library() {
    return library;
  }

  /** The rules over the text of the files the build holds. */
  public List<TextRule> textRules() {
    return textRules;
  }

  /** The globs no text rule reads. */
  public List<String> textExclusions() {
    return textExclusions;
  }

  /** Reads the configuration of the build at the file, for the module of the given id. */
  public static Configuration read(Path file, String module) throws IOException {
    if (!Files.exists(file)) {
      return new Configuration(Settings.empty(), null, file.toAbsolutePath().getParent(), ConfigFactory.empty());
    }
    Config config;
    try {
      config = ConfigFactory.parseFile(file.toFile()).resolve();
    } catch (ConfigException error) {
      throw new IOException("baumkuchen: " + file + ": " + error.getMessage(), error);
    }
    Path root = file.toAbsolutePath().getParent();
    Settings settings = settings(config, "", Set.of(
      "baseline", "purity", "effects", "concurrent", "generated", "hot", "reflective", "checked", "bounded", "nesting",
      "text", "projects"));
    Settings own = config.hasPath("projects." + module)
      ? settings(config.getConfig("projects." + module), "projects." + module, Set.of())
      : null;

    Set<String> kinds = new TreeSet<>();
    settings.effects.values().forEach(kinds::addAll);
    kinds.removeAll(EFFECTS);
    if (!kinds.isEmpty()) {
      throw new IOException(
        "baumkuchen: unknown effect kinds " + String.join(", ", kinds) + "; the kinds are " +
          String.join(", ", new TreeSet<>(EFFECTS)));
    }
    return new Configuration(settings, own, root, config);
  }

  /** The settings a configuration holds, rejecting the keys it does not know. */
  private static Settings settings(Config config, String where, Set<String> others) throws IOException {
    Set<String> unknown = new TreeSet<>(config.root().keySet());
    unknown.removeAll(SETTINGS);
    unknown.removeAll(others);
    if (!unknown.isEmpty()) {
      throw new IOException(
        "baumkuchen: " + where + " has unknown keys " + String.join(", ", unknown) + "; the keys are " +
          String.join(", ", new TreeSet<>(SETTINGS)));
    }
    Settings settings = new Settings();
    settings.disabled = list(config, "disabled");
    settings.errors = list(config, "errors");
    settings.warnings = list(config, "warnings");
    settings.wildcards = config.hasPath("wildcards") ? config.getInt("wildcards") : null;
    settings.baseline = config.hasPath("baseline") ? config.getString("baseline") : "baumkuchen-baseline.tsv";
    settings.purity = config.hasPath("purity") ? config.getString("purity") : "baumkuchen-purity.tsv";
    settings.concurrent = list(config, "concurrent");
    settings.generated = list(config, "generated");
    settings.hot = list(config, "hot");
    settings.reflective = list(config, "reflective");
    settings.checked = list(config, "checked");
    settings.bounded = bounded(config);
    settings.nesting = nesting(config);
    settings.library = config.hasPath("library") ? config.getBoolean("library") : null;
    Map<String, Set<String>> effects = new LinkedHashMap<>();
    if (config.hasPath("effects")) {
      Config block = config.getConfig("effects");
      for (String prefix : block.root().keySet()) {
        // The prefix is a qualified name, so it holds dots that are part of the name rather than a path.
        effects.put(prefix, new LinkedHashSet<>(block.getStringList(ConfigUtil.joinPath(prefix))));
      }
    }
    settings.effects = effects;
    return settings;
  }

  /** The limits of the nesting rule a configuration names. */
  private static Map<String, Integer> nesting(Config config) throws IOException {
    Map<String, Integer> limits = new LinkedHashMap<>();
    if (!config.hasPath("nesting")) {
      return limits;
    }
    Config block = config.getConfig("nesting");
    Set<String> unknown = new TreeSet<>(block.root().keySet());
    unknown.removeAll(NESTING);
    if (!unknown.isEmpty()) {
      throw new IOException(
        "baumkuchen: .baumkuchen.conf has unknown keys nesting." + String.join(", nesting.", unknown) +
          "; the keys are nesting." + String.join(", nesting.", NESTING));
    }
    for (String limit : NESTING) {
      if (block.hasPath(limit)) {
        limits.put(limit, block.getInt(limit));
      }
    }
    return limits;
  }

  /** The members a configuration declares bounded, in the order it names them. */
  private static List<Bounded> bounded(Config config) {
    if (!config.hasPath("bounded")) {
      return List.of();
    }
    return config.getConfigList("bounded").stream()
      .map(entry -> new Bounded(entry.getString("member"), entry.getInt("max"), entry.getString("reason")))
      .toList();
  }

  /** The list a setting holds, or null when the configuration does not name it. */
  private static Set<String> list(Config config, String path) {
    if (!config.hasPath(path)) {
      return null;
    }
    return new LinkedHashSet<>(config.getStringList(path));
  }

  /** The settings of a build or of one project of it; a setting the configuration does not name is null, so that the
    * build's setting takes its place. */
  private static final class Settings {
    private Set<String> disabled = null;
    private Set<String> errors = null;
    private Set<String> warnings = null;
    private Integer wildcards = null;
    private String baseline = "baumkuchen-baseline.tsv";
    private String purity = "baumkuchen-purity.tsv";
    private Map<String, Set<String>> effects = Collections.emptyMap();
    private Set<String> concurrent = null;
    private Set<String> generated = null;
    private Set<String> hot = null;
    private Set<String> reflective = null;
    private Set<String> checked = null;
    private List<Bounded> bounded = List.of();
    private Map<String, Integer> nesting = Collections.emptyMap();
    private Boolean library = null;

    private static Settings empty() {
      Settings settings = new Settings();
      settings.disabled = Set.of();
      settings.errors = Set.of();
      settings.warnings = Set.of();
      settings.concurrent = Set.of();
      settings.generated = Set.of();
      settings.hot = Set.of();
      settings.reflective = Set.of();
      settings.checked = Set.of();
      return settings;
    }
  }

  /** A member, by qualified name glob, whose collections hold at most {@code max} elements, for the {@code reason}. */
  public record Bounded(String member, int max, String reason) {
  }
}
