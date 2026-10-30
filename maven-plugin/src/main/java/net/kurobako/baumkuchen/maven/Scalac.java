package net.kurobako.baumkuchen.maven;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * The options the compiler plugin reads, which the scalac of the build takes from a response file.
 *
 * <p>The build may compile with fatal warnings, which would make every finding of the plugin fail it, so the findings
 * are routed by the mode instead: the dev mode reports them without failing, and the ci mode fails on them. A check
 * kept as a warning is reported without failing in either mode.
 */
public final class Scalac {

  /** The message every finding of the plugin ends with, which the filters match. */
  private static final String FINDING = ".*\\[baumkuchen.*\\]";

  private Scalac() {
  }

  /** The options that write the SemanticDB of the build under its output directory, with paths relative to the root. */
  public static List<String> semanticdb(File root, File output) {
    List<String> options = new ArrayList<>();
    options.add("-Xsemanticdb");
    options.add("-sourceroot");
    options.add(root.getAbsolutePath());
    options.add("-semanticdb-target");
    options.add(output.getAbsolutePath());
    return options;
  }

  /** The options the compiler plugin reads, for the mode. */
  public static List<String> options(Configuration configuration, Ids ids, String mode) {
    List<String> options = new ArrayList<>();
    options.add("-P:baumkuchen:mode=" + mode);
    options.add("-P:baumkuchen:wildcards=" + configuration.wildcards());
    options.add("-Wconf:msg=" + FINDING + ":" + (mode.equals("ci") ? "e" : "i"));
    if (mode.equals("ci")) {
      for (String id : sorted(configuration.warnings())) {
        if (ids.checks().contains(id)) {
          // The filter holds no colon, because the compiler splits the option on them, and the check's name is
          // anchored to the end of the tag so that one check's name does not match another's.
          options.add("-Wconf:msg=.*\\[baumkuchen[^]]*" + id + "\\]:i");
        }
      }
    }
    for (String id : sorted(configuration.disabled())) {
      if (ids.checks().contains(id)) {
        options.add("-P:baumkuchen:disable=" + id);
      }
    }
    for (String id : sorted(configuration.errors())) {
      if (ids.checks().contains(id)) {
        options.add("-P:baumkuchen:error=" + id);
      }
    }
    for (String id : sorted(configuration.warnings())) {
      if (ids.checks().contains(id)) {
        options.add("-P:baumkuchen:warn=" + id);
      }
    }
    for (String glob : sorted(configuration.hot())) {
      options.add("-P:baumkuchen:hot=" + glob);
    }
    for (String type : sorted(configuration.checked())) {
      options.add("-P:baumkuchen:checked=" + type);
    }
    return options;
  }

  private static List<String> sorted(Set<String> values) {
    return new ArrayList<>(new TreeSet<>(values));
  }
}
