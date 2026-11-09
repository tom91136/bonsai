package net.kurobako.bonsai.maven;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * The ids the tools accept, which each tool publishes in a resource, so that a plugin cannot drift from them.
 *
 * <p>The compiler plugin publishes the ids of its checks, the analysis the ids of its rules, and the scalafix rules
 * the names they have, with the rules scalafix itself provides that bonsai turns on, their bans and settings, and the
 * compiler options the unused-code rule reads.
 */
public final class Ids {

  private final Set<String> checks;
  private final Set<String> findings;
  private final Set<String> rules;
  private final Set<String> optIn = new TreeSet<>();
  private final List<String> bans = new ArrayList<>();
  private final List<String> settings = new ArrayList<>();
  private final List<String> unusedOptions = new ArrayList<>();

  /**
   * The built-in entries are the rule names scalafix provides, their bans, which each contain a dot, their settings,
   * which each contain {@code =}, and the compiler options the unused-code rule reads, which each start with
   * {@code -}. A rule which starts with {@code ?} is opt-in.
   */
  private Ids(Set<String> checks, Set<String> findings, Set<String> rules, Set<String> builtin) {
    this.checks = checks;
    this.findings = findings;
    this.rules = new LinkedHashSet<>(rules);
    for (String entry : builtin) {
      if (entry.startsWith("-")) {
        unusedOptions.add(entry);
      } else if (entry.startsWith("?")) {
        optIn.add(entry.substring(1));
        this.rules.add(entry.substring(1));
      } else if (entry.contains("=")) {
        settings.add(entry);
      } else if (entry.contains(".")) {
        bans.add(entry);
      } else {
        this.rules.add(entry);
      }
    }
  }

  /** The ids the compiler plugin knows. */
  public Set<String> checks() {
    return checks;
  }

  /** The ids the analysis knows. */
  public Set<String> findings() {
    return findings;
  }

  /** The names the scalafix rules have, and the syntax scalafix itself provides. */
  public Set<String> rules() {
    return rules;
  }

  /** The rules the configuration has to enable before they run. */
  public Set<String> optIn() {
    return optIn;
  }

  /** The bans the DisableSyntax rule holds, each turned off by its own name. */
  public List<String> bans() {
    return bans;
  }

  /** The settings the built-in rules always run with, as scalafix configuration lines. */
  public List<String> settings() {
    return settings;
  }

  /** The compiler options the unused-code rule reads. */
  public List<String> unusedOptions() {
    return unusedOptions;
  }

  /** The ids of the tools, read from the resources they publish them in. */
  public static Ids of(Path checks, Path findings, Path rules, Path builtin) throws IOException {
    return new Ids(read(checks), read(findings), read(rules), read(builtin));
  }

  /** The ids of the tools, read from the jars they publish them in. */
  public static Ids of(File compilerPlugin, File analysis, File rules) throws IOException {
    return new Ids(
      read(compilerPlugin, "checks.id"), read(analysis, "findings.id"), read(rules, "rules.id"),
      read(rules, "builtin.id"));
  }

  /** The ids the resource holds, one a line. */
  private static Set<String> read(Path file) throws IOException {
    return parse(Files.readString(file, StandardCharsets.UTF_8));
  }

  /** The ids the resource of the jar holds, one a line. */
  private static Set<String> read(File jar, String name) throws IOException {
    return parse(Versions.resource(jar, name));
  }

  /** The ids the text holds, one a line. */
  private static Set<String> parse(String text) {
    Set<String> ids = new LinkedHashSet<>();
    for (String line : text.split("\n")) {
      String id = line.trim();
      if (!id.isEmpty()) {
        ids.add(id);
      }
    }
    return ids;
  }

  /** Fails on an id no tool knows, which catches a typo, and on an enabled id that is not an opt-in rule. */
  public void validate(Set<String> disabled, Set<String> enabled, Set<String> errors, Set<String> nonfatal)
      throws IOException {
    Set<String> unknown = new TreeSet<>();
    unknown.addAll(disabled);
    unknown.addAll(errors);
    unknown.addAll(nonfatal);
    unknown.removeAll(checks);
    unknown.removeAll(findings);
    unknown.removeAll(rules);
    unknown.removeAll(bans);
    if (!unknown.isEmpty()) {
      throw new IOException(
        "bonsai: unknown ids " + String.join(", ", unknown) + "; the ids are the compiler plugin checks " +
          String.join(", ", new TreeSet<>(checks)) + "; the analysis rules " + String.join(", ", new TreeSet<>(findings)) +
          "; and the scalafix rules " + String.join(", ", new TreeSet<>(rules)));
    }
    Set<String> notOptIn = new TreeSet<>(enabled);
    notOptIn.removeAll(optIn);
    if (!notOptIn.isEmpty()) {
      throw new IOException(
        "bonsai: only opt-in rules can be enabled, not " + String.join(", ", notOptIn) + "; the opt-in rules are " +
          String.join(", ", optIn));
    }
  }
}
