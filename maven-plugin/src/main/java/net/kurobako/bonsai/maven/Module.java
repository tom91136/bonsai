package net.kurobako.bonsai.maven;

import java.nio.file.Path;
import java.util.List;

/**
 * A module of the build the analysis reads: its output, its model, its classpath, its sources and its compiler options.
 *
 * <p>The model of a module is the file the javac plugin wrote while the compiler compiled the code of the scope, which
 * holds the nodes of the code and the references between them. A module is recorded when the compiler plugin compiled
 * it in the audit mode, which keeps the findings of the compiler beside the classes for the analysis to read.
 */
public record Module(
  String id,
  Path classes,
  Path testClasses,
  Path model,
  Path testModel,
  List<Path> mainSources,
  List<Path> testSources,
  List<String> classpath,
  List<String> options,
  boolean library,
  boolean recorded
) {
}
