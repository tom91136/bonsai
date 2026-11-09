package net.kurobako.bonsai.maven;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * The options the compiler plugin reads, which the scalac of the build takes from a response file.
 *
 * <p>The findings are routed by the mode, whether or not the build compiles with fatal warnings: the dev and audit
 * modes report them without failing, and the ci mode fails on them. A check kept as a warning is reported without
 * failing in any mode. In the audit mode the compiler plugin records what the ci mode would fail on, for the analysis
 * to fail on at the end.
 */
public final class Scalac {

  /** The ids of the checks, as the tag every finding of the plugin ends with spells them. */
  private static final String ANY = "[a-z0-9-]+";

  /** The options with which a build asks the compiler for unused warnings itself. */
  private static final Pattern UNUSED = Pattern.compile("-(Wunused|Ywarn-unused|Wall).*");

  private Scalac() {
  }

  /**
   * The option that records the path of every source from the root of the build, as an sbt build does, so the paths of
   * two modules stay apart and the tools that run from the root find the sources.
   */
  public static List<String> sourceroot(File root) {
    return List.of("-sourceroot", root.getAbsolutePath());
  }

  /** The options that write the SemanticDB of the module under its output directory. */
  public static List<String> semanticdb(File output) {
    return List.of("-Xsemanticdb", "-semanticdb-target", output.getAbsolutePath());
  }

  /**
   * The options that ask the compiler for the unused warnings the RemoveUnused rule reads, and keep fatal warnings from
   * failing the build on what the rule fixes. A build that asks for unused warnings itself, or leaves the rule out,
   * gets none.
   */
  public static List<String> unused(
    Configuration configuration, Ids ids, Versions.Support support, String scala, List<String> given) {
    if (configuration.disabled().contains("RemoveUnused") || !support.removeUnused(scala) || !ownUnused(given).isEmpty()) {
      return List.of();
    }
    return ids.unusedOptions();
  }

  /** The options with which the build asks the compiler for unused warnings itself. */
  public static List<String> ownUnused(List<String> given) {
    return given.stream().filter(option -> option != null && UNUSED.matcher(option).matches()).toList();
  }

  /** The option that gives the findings of a check the action. The tag holds any character beside the id, as the compiler
   * splits the option on a colon. */
  private static String tagged(String id, String action) {
    return "-Wconf:msg=\\[bonsai." + id + "\\]:" + action;
  }

  private static String escape(String path) {
    return path.replaceAll("([*?\\[\\]{}\\\\])", "\\\\$1");
  }

  /** The options the compiler plugin reads, for the mode and the directories the build generates sources into. */
  public static List<String> options(Configuration configuration, Ids ids, String mode, List<File> managed) {
    List<String> options = new ArrayList<>();
    options.add("-P:bonsai:mode=" + mode);
    options.add("-P:bonsai:wildcards=" + configuration.wildcards());
    configuration.optionFold().ifPresent(fold -> options.add("-P:bonsai:option-fold=" + fold));
    options.add(tagged(ANY, mode.equals("ci") ? "e" : "i"));
    if (mode.equals("ci")) {
      for (String id : configuration.warnings()) {
        if (ids.checks().contains(id)) {
          options.add(tagged(id, "i"));
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
    for (String member : configuration.bounded()) {
      options.add("-P:bonsai:bounded=" + member);
    }
    for (String type : configuration.checked()) {
      options.add("-P:bonsai:checked=" + type);
    }
    for (String glob : configuration.generated()) {
      options.add("-P:bonsai:generated=" + glob);
    }
    for (File directory : managed) {
      options.add("-P:bonsai:generated=" + escape(directory.getAbsolutePath()) + "/**");
    }
    List<String> skipped = configuration.generatedDisable().contains("*")
      ? List.of("*")
      : configuration.generatedDisable().stream().filter(ids.checks()::contains).toList();
    if (skipped.isEmpty()) {
      // the plugin skips every check over generated code when the option is absent
      options.add("-P:bonsai:generated-disable=");
    }
    for (String id : skipped) {
      options.add("-P:bonsai:generated-disable=" + id);
    }
    return options;
  }
}
