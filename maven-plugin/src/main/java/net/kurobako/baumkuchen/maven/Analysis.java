package net.kurobako.baumkuchen.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Stream;

/**
 * The options the analysis reads, and the manifest of the build it reads the compiled output of.
 *
 * <p>The manifest names the output of every module, its classpath, its sources and its compiler options, and the text
 * rules the analysis reads the files of the build from.
 */
public final class Analysis {

  private Analysis() {
  }

  /** The options the analysis reads, for the mode; `writeBaseline` names the file the findings are recorded in. */
  public static List<String> options(
      Configuration configuration, Ids ids, String mode, Path root, String writeBaseline) {
    List<String> options = new ArrayList<>();
    options.add("--mode");
    options.add(mode);
    for (String id : sorted(configuration.disabled())) {
      if (ids.findings().contains(id)) {
        options.add("--disable");
        options.add(id);
      }
    }
    for (String id : sorted(configuration.errors())) {
      if (ids.findings().contains(id)) {
        options.add("--error");
        options.add(id);
      }
    }
    for (String id : sorted(configuration.warnings())) {
      if (ids.findings().contains(id)) {
        options.add("--warn");
        options.add(id);
      }
    }
    options.add("--baseline");
    options.add(configuration.baseline().toString());
    // The file the environment names records the findings of the run, which is how the baseline is written.
    if (writeBaseline != null && !writeBaseline.isBlank()) {
      Path file = Path.of(writeBaseline);
      options.add("--write-baseline");
      options.add((file.isAbsolute() ? file : root.resolve(file)).toString());
    }
    options.add("--purity-lock");
    options.add(configuration.purity().toString());
    for (String prefix : new TreeSet<>(configuration.effects().keySet())) {
      options.add("--effects");
      options.add(prefix + "=" + String.join(",", sorted(configuration.effects().get(prefix))));
    }
    for (String method : sorted(configuration.concurrent())) {
      options.add("--concurrent");
      options.add(method);
    }
    for (String glob : sorted(configuration.generated())) {
      options.add("--generated");
      options.add(glob);
    }
    for (String glob : sorted(configuration.reflective())) {
      options.add("--reflective");
      options.add(glob);
    }
    for (String glob : sorted(configuration.hot())) {
      options.add("--hot");
      options.add(glob);
    }
    for (Configuration.Bounded bound : configuration.bounded()) {
      options.add("--bounded");
      options.add(bound.member());
      options.add(Integer.toString(bound.max()));
      options.add(bound.reason());
    }
    if (!configuration.nesting().isEmpty()) {
      options.add("--nesting");
    }
    for (String limit : new TreeSet<>(configuration.nesting().keySet())) {
      options.add("--nesting-" + limit);
      options.add(Integer.toString(configuration.nesting().get(limit)));
    }
    return options;
  }

  /**
   * The manifest of the build, which names the output of every module, its model, its classpath, its sources and its
   * compiler options, and the text rules the analysis reads.
   */
  public static List<String> manifest(
    Path root,
    List<Module> modules,
    List<String> textExclusions,
    List<TextRule> textRules
  ) {
    List<String> manifest = new ArrayList<>();
    manifest.add("root\t" + root);
    for (Module module : modules) {
      if (module.classes() != null) {
        manifest.add("output\t" + module.id() + "\tmain\t" + module.classes());
      }
      if (module.testClasses() != null) {
        manifest.add("output\t" + module.id() + "\ttest\t" + module.testClasses());
      }
      if (module.model() != null) {
        manifest.add("model\t" + module.id() + "\tmain\t" + module.model());
      }
      if (module.testModel() != null) {
        manifest.add("model\t" + module.id() + "\ttest\t" + module.testModel());
      }
      if (module.library()) {
        manifest.add("library\t" + module.id());
      }
      for (Path source : module.mainSources()) {
        manifest.add("source\t" + module.id() + "\tmain\t" + source);
      }
      for (Path source : module.testSources()) {
        manifest.add("source\t" + module.id() + "\ttest\t" + source);
      }
      for (String option : module.options()) {
        manifest.add("option\t" + module.id() + "\t" + option);
      }
      for (String entry : module.classpath()) {
        manifest.add("classpath\t" + entry);
      }
    }
    for (String glob : textExclusions) {
      manifest.add("text-exclude\t" + glob);
    }
    for (int index = 0; index < textRules.size(); index++) {
      manifest.addAll(textRules.get(index).manifest(index));
    }
    return manifest;
  }

  private static List<String> sorted(Set<String> values) {
    return new ArrayList<>(new TreeSet<>(values));
  }

  /**
   * The manifest of the whole build, which is the fragments of every module of it.
   *
   * <p>The root of the build is the same for every fragment, so the manifest names it once, at the end.
   */
  public static List<String> merge(Path fragments, String root) throws IOException {
    List<String> manifest = new ArrayList<>();
    try (Stream<Path> files = Files.list(fragments)) {
      for (Path file : files.sorted(Comparator.comparing(Path::toString)).toList()) {
        for (String line : Files.readAllLines(file)) {
          if (!line.startsWith("root\t")) {
            manifest.add(line);
          }
        }
      }
    }
    manifest.add("root\t" + root);
    return manifest;
  }
}
