package net.kurobako.bonsai.maven;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * The ids the tools accept, which each tool publishes in a resource, so that a plugin cannot drift from them.
 *
 * <p>The compiler plugin publishes the ids of its checks, the analysis the ids of its rules, and the scalafix rules
 * the names they have.
 */
public final class Ids {

  /** The syntax scalafix itself provides, which bonsai turns on and off as a whole. */
  private static final List<String> BUILT_INS = List.of("DisableSyntax", "RedundantSyntax");

  /** The bans the DisableSyntax rule holds, each turned off by its own name. */
  private static final List<String> BANS =
    List.of("DisableSyntax.noVars", "DisableSyntax.noReturns", "DisableSyntax.noNulls", "DisableSyntax.noAsInstanceOf",
      "DisableSyntax.noIsInstanceOf", "DisableSyntax.noXml", "DisableSyntax.noFinalize");

  private final Set<String> checks;
  private final Set<String> findings;
  private final Set<String> rules;

  private Ids(Set<String> checks, Set<String> findings, Set<String> rules) {
    this.checks = checks;
    this.findings = findings;
    this.rules = new LinkedHashSet<>(rules);
    this.rules.addAll(BUILT_INS);
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

  /** The bans the DisableSyntax rule holds, each turned off by its own name. */
  public static List<String> bans() {
    return BANS;
  }

  /** The ids of the tools, read from the resources they publish them in. */
  public static Ids of(Path checks, Path findings, Path rules) throws IOException {
    return new Ids(read(checks), read(findings), read(rules));
  }

  /** The ids of the tools, read from the jars they publish them in. */
  public static Ids of(File compilerPlugin, File analysis, File rules) throws IOException {
    return new Ids(read(compilerPlugin, "checks.id"), read(analysis, "findings.id"), read(rules, "rules.id"));
  }

  /** The ids the resource holds, one a line. */
  private static Set<String> read(Path file) throws IOException {
    return parse(Files.readString(file, StandardCharsets.UTF_8));
  }

  /** The ids the resource of the jar holds, one a line. */
  private static Set<String> read(File jar, String name) throws IOException {
    try (ZipFile file = new ZipFile(jar)) {
      ZipEntry entry = file.getEntry("META-INF/bonsai/" + name);
      if (entry == null) {
        throw new IOException("bonsai: " + jar + " does not publish " + name + "; the tools are not the ones this " +
          "plugin knows");
      }
      try (InputStream stream = file.getInputStream(entry)) {
        return parse(new String(stream.readAllBytes(), StandardCharsets.UTF_8));
      }
    }
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

  /** Fails when an id no tool knows, which catches a typo in the configuration. */
  public void validate(Set<String> disabled, Set<String> errors, Set<String> warnings) throws IOException {
    Set<String> unknown = new TreeSet<>();
    unknown.addAll(disabled);
    unknown.addAll(errors);
    unknown.addAll(warnings);
    unknown.removeAll(checks);
    unknown.removeAll(findings);
    unknown.removeAll(rules);
    unknown.removeAll(BANS);
    if (!unknown.isEmpty()) {
      throw new IOException(
        "bonsai: unknown ids " + String.join(", ", unknown) + "; the ids are the compiler plugin checks " +
          String.join(", ", new TreeSet<>(checks)) + "; the analysis rules " + String.join(", ", new TreeSet<>(findings)) +
          "; and the scalafix rules " + String.join(", ", new TreeSet<>(rules)));
    }
  }
}
