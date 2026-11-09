package net.kurobako.bonsai.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;

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
   * compiler options, the JDK the build targets, and the text rules the analysis reads.
   */
  public static List<String> manifest(
    Path root,
    Path jdk,
    List<Module> modules,
    List<String> text
  ) {
    List<String> manifest = new ArrayList<>();
    manifest.add("root\t" + root);
    manifest.add("jdk\t" + jdk);
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
      if (module.recorded()) {
        manifest.add("recorded\t" + module.id());
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
   * The manifest of the whole build, which is the fragments of the modules of it.
   *
   * <p>Only the fragments of the modules named are read, so a fragment left in the directory by another build is not
   * part of this one. The fragments are read in the order of their names, whatever order the modules are named in.
   * The root of the build is the same for every fragment, so the manifest names it once, at the end.
   */
  public static List<String> merge(Path fragments, Collection<String> modules, String root) throws IOException {
    List<String> manifest = new ArrayList<>();
    List<Path> files = modules.stream()
      .distinct()
      .map(module -> fragments.resolve(module + ".tsv"))
      .sorted(Comparator.comparing(Path::toString))
      .toList();
    for (Path file : files) {
      for (String line : Files.readAllLines(file)) {
        if (!line.startsWith("root\t")) {
          manifest.add(line);
        }
      }
    }
    manifest.add("root\t" + root);
    return manifest;
  }
}
