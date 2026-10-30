package net.kurobako.baumkuchen.maven;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.typesafe.config.ConfigValue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * A rule over the text of the files the build holds, whatever their language, which the analysis reads from the
 * manifest it takes.
 *
 * <p>The kinds are the terms a file may not contain, the patterns it may not contain, the binary files, and the lines
 * that need a blank line before them or after their block.
 */
public final class TextRule {

  /** The kinds of text rule, and the fields each takes besides the ones they all take. */
  private static final Map<String, Set<String>> KINDS = Map.of(
    "terms", Set.of("words", "message"),
    "patterns", Set.of("pattern", "message"),
    "binary", Set.of("message"),
    "preceded", Set.of("pattern", "message"),
    "followed", Set.of("pattern", "continues", "neutral", "message")
  );

  /** The fields a text rule holds that take a list rather than a single value. */
  private static final Set<String> LISTS = Set.of("words", "files", "exclude");

  private final String kind;
  private final Map<String, List<String>> fields;

  private TextRule(String kind, Map<String, List<String>> fields) {
    this.kind = kind;
    this.fields = fields;
  }

  /** The kind of the rule, which the analysis takes by its index. */
  public String kind() {
    return kind;
  }

  /** The manifest lines of the rule and of the rules before it, which the analysis reads. */
  public List<String> manifest(int index) {
    List<String> lines = new ArrayList<>();
    lines.add("text\t" + index + "\tkind\t" + kind);
    for (String field : new TreeSet<>(fields.keySet())) {
      for (String value : fields.get(field)) {
        lines.add("text\t" + index + "\t" + field + "\t" + value);
      }
    }
    return lines;
  }

  /** The rules the configuration holds, and the globs no text rule reads. */
  public static List<TextRule> read(Config config, java.nio.file.Path root) throws IOException {
    List<TextRule> rules = new ArrayList<>();
    if (!config.hasPath("text")) {
      return rules;
    }
    Config block = config.getConfig("text");
    Set<String> unknown = new TreeSet<>(block.root().keySet());
    unknown.removeAll(KINDS.keySet());
    unknown.remove("exclude");
    if (!unknown.isEmpty()) {
      throw new IOException(
        "baumkuchen: text has unknown keys " + String.join(", ", unknown) + "; the keys are " +
          String.join(", ", KINDS.keySet()) + " and exclude");
    }
    for (String kind : KINDS.keySet()) {
      if (!block.hasPath(kind)) {
        continue;
      }
      if (kind.equals("binary")) {
        rules.add(rule(kind, block.getConfig(kind)));
      } else {
        for (Config rule : block.getConfigList(kind)) {
          rules.add(rule(kind, rule));
        }
      }
    }
    return rules;
  }

  /** The globs no text rule reads, which the analysis takes from the manifest. */
  public static List<String> exclusions(Config config) {
    if (!config.hasPath("text") || !config.getConfig("text").hasPath("exclude")) {
      return List.of();
    }
    return config.getConfig("text").getStringList("exclude");
  }

  /** The rule a configuration holds, rejecting the fields its kind does not take. */
  private static TextRule rule(String kind, Config config) throws IOException {
    Set<String> known = new LinkedHashSet<>(KINDS.get(kind));
    known.add("files");
    known.add("exclude");
    known.add("level");
    Set<String> unknown = new TreeSet<>(config.root().keySet());
    unknown.removeAll(known);
    if (!unknown.isEmpty()) {
      throw new IOException(
        "baumkuchen: text." + kind + " has unknown keys " + String.join(", ", unknown) + "; the keys are " +
          String.join(", ", new TreeSet<>(known)));
    }
    Map<String, List<String>> fields = new LinkedHashMap<>();
    for (String field : new TreeSet<>(config.root().keySet())) {
      fields.put(field, LISTS.contains(field)
        ? config.getStringList(field)
        : List.of(config.getString(field)));
    }
    return new TextRule(kind, fields);
  }
}
