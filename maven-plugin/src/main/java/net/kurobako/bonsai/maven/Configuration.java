package net.kurobako.bonsai.maven;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * The bonsai configuration of a module, as the reader shipped with the analysis writes it.
 *
 * <p>The reader is the one the sbt plugin uses, so both builds configure bonsai the same way. It writes one entry a
 * line, a key and its value apart by a tab, and this class holds what the goals take from them.
 */
public final class Configuration {

  private final Set<String> disabled = new TreeSet<>();
  private final Set<String> errors = new TreeSet<>();
  private final Set<String> warnings = new TreeSet<>();
  private final Set<String> hot = new TreeSet<>();
  private final List<String> bounded = new ArrayList<>();
  private final Set<String> checked = new TreeSet<>();
  private final List<String> options = new ArrayList<>();
  private final List<String> text = new ArrayList<>();
  private int wildcards = 2;
  private boolean library;

  private Configuration() {
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

  /** The globs of the classes whose code runs hot. */
  public Set<String> hot() {
    return hot;
  }

  /** The members whose collections hold a bounded number of elements, as {@code <member>:<max>}. */
  public List<String> bounded() {
    return bounded;
  }

  /** The refinement types whose checked operations a conversion can bypass. */
  public Set<String> checked() {
    return checked;
  }

  /** Whether other builds use this module's public API, so that the analysis counts it as used. */
  public boolean library() {
    return library;
  }

  /** The options the analysis runs with, apart from its mode and the file it records the baseline in. */
  public List<String> options() {
    return options;
  }

  /** The manifest lines of the rules over the text of the files the build holds. */
  public List<String> text() {
    return text;
  }

  /** The configuration the lines of the reader hold. */
  public static Configuration parse(List<String> lines) throws IOException {
    Configuration configuration = new Configuration();
    for (String line : lines) {
      String[] entry = line.split("\t", 2);
      String value = entry.length > 1 ? entry[1] : "";
      switch (entry[0]) {
        case "disabled" -> configuration.disabled.add(value);
        case "errors" -> configuration.errors.add(value);
        case "warnings" -> configuration.warnings.add(value);
        case "hot" -> configuration.hot.add(value);
        case "bounded" -> configuration.bounded.add(value);
        case "checked" -> configuration.checked.add(value);
        case "option" -> configuration.options.add(value);
        case "wildcards" -> configuration.wildcards = Integer.parseInt(value);
        case "library" -> configuration.library = Boolean.parseBoolean(value);
        case "text", "text-exclude" -> configuration.text.add(line);
        default -> throw new IOException("bonsai: the configuration reader wrote an unknown entry: " + line);
      }
    }
    return configuration;
  }
}
