package net.kurobako.bonsai.maven;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * The options the compiler plugin reads, which the scalac of the build takes from a response file.
 *
 * <p>The build may compile with fatal warnings, which would make every finding of the plugin fail it, so the findings
 * are routed by the mode instead: the dev mode reports them without failing, and the ci mode fails on them. A check
 * kept as a warning is reported without failing in either mode.
 */
public final class Scalac {

  /** The message every finding of the plugin ends with, which the filters match. */
  private static final String FINDING = ".*\\[bonsai.*\\]";

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
    options.add("-P:bonsai:mode=" + mode);
    options.add("-P:bonsai:wildcards=" + configuration.wildcards());
    options.add("-Wconf:msg=" + FINDING + ":" + (mode.equals("ci") ? "e" : "i"));
    if (mode.equals("ci")) {
      for (String id : configuration.warnings()) {
        if (ids.checks().contains(id)) {
          // The filter holds no colon, because the compiler splits the option on them, and the check's name is
          // anchored to the end of the tag so that one check's name does not match another's.
          options.add("-Wconf:msg=.*\\[bonsai[^]]*" + id + "\\]:i");
        }
      }
    }
    for (String id : configuration.disabled()) {
      if (ids.checks().contains(id)) {
        options.add("-P:bonsai:disable=" + id);
      }
    }
    for (String id : configuration.errors()) {
      if (ids.checks().contains(id)) {
        options.add("-P:bonsai:error=" + id);
      }
    }
    for (String id : configuration.warnings()) {
      if (ids.checks().contains(id)) {
        options.add("-P:bonsai:warn=" + id);
      }
    }
    for (String glob : configuration.hot()) {
      options.add("-P:bonsai:hot=" + glob);
    }
    for (String type : configuration.checked()) {
      options.add("-P:bonsai:checked=" + type);
    }
    return options;
  }
}
