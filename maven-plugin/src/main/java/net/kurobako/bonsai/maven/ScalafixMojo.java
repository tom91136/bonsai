package net.kurobako.bonsai.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Runs the scalafix rules, which read the SemanticDB the compiler writes.
 *
 * <p>The rules read the SemanticDB of a source at the path the source has from the root of the build, which is where
 * the compiler writes it ({@link Scalac}), so the rules run from the root too.
 */
@Mojo(
  name = "scalafix",
  defaultPhase = LifecyclePhase.VERIFY,
  threadSafe = true,
  requiresDependencyResolution = ResolutionScope.TEST
)
public class ScalafixMojo extends BonsaiMojo {

  /** The exit codes of scalafix for a source that breaks a rule, and for one that differs from the fix of a rule. */
  private static final int LINTER_ERROR = 64;
  private static final int TEST_ERROR = 32;

  @Override
  public void execute() throws MojoExecutionException {
    Tools tools = tools();
    Configuration configuration = configuration(tools);
    File plugin = compilerPlugin(tools);
    File rules = tools.rulesJar(version);
    Versions.ScalafixVersions scalafix = scalafix(rules);

    Path work = work();
    Path file = work.resolve("scalafix.conf");
    Ids ids = ids(tools, plugin, rules);
    Versions.Support support = support(rules);
    String scala = scalaVersion(tools);
    write(file, List.of(Scalafix.configuration(configuration, ids, support, scala)));

    Path root = root();
    List<Path> sources = sources(project.getCompileSourceRoots());
    sources.addAll(sources(project.getTestCompileSourceRoots()));
    List<String> files = new ArrayList<>();
    for (Path source : sources) {
      files.add(root.relativize(source).toString());
    }
    if (files.isEmpty()) {
      getLog().info("bonsai: the module has no sources the rules read");
      return;
    }

    List<String> classpath = new ArrayList<>(classpath());
    classpath.add(new File(project.getBuild().getOutputDirectory()).getAbsolutePath());
    classpath.add(new File(project.getBuild().getTestOutputDirectory()).getAbsolutePath());

    List<String> command = new ArrayList<>();
    command.add(javaExecutable());
    command.add("-cp");
    command.add(Javac.processorPath(tools.scalafix(version, scalafix.version(), scalafix.scalaVersion())));
    command.add("scalafix.cli.Cli");
    command.add("--config");
    command.add(file.toString());
    command.add("--check");
    command.add("--scala-version");
    command.add(scala);
    List<String> given = scalacOptions();
    List<String> unused = new ArrayList<>(Scalac.ownUnused(given));
    unused.addAll(Scalac.unused(configuration, ids, support, scala, given));
    for (String option : unused) {
      command.add("--scalac-options");
      command.add(option);
    }
    command.add("--sourceroot");
    command.add(root.toString());
    command.add("--classpath");
    command.add(String.join(java.io.File.pathSeparator, classpath));
    // `--files` takes a single path, so repeating it would leave all but one source of the module unchecked
    command.addAll(files);
    boolean dev = dev();
    run("the scalafix rules", command, root.toFile(), code -> dev && (code & ~(LINTER_ERROR | TEST_ERROR)) == 0);
  }
}
