package net.kurobako.bonsai.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

/**
 * The manifest of the build the analysis reads the compiled output of.
 *
 * <p>The manifest names the output of every module, its classpath, its sources and its compiler options, and the text
 * rules the analysis reads the files of the build from.
 */
public final class Analysis {

  private Analysis() {
  }

  /**
   * The manifest of the build, which names the output of every module, its model, its classpath, its sources and its
   * compiler options, and the text rules the analysis reads.
   */
  public static List<String> manifest(
    Path root,
    List<Module> modules,
    List<String> text
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
        manifest.add("classpath\t" + module.id() + "\t" + entry);
      }
    }
    manifest.addAll(text);
    return manifest;
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
