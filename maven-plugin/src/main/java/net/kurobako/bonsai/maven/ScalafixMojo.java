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
 * <p>The rules read the SemanticDB of a source at the path the source has from the directory of the module, which is
 * where the compiler writes it ({@link Scalac}), so the rules run from that directory too.
 */
@Mojo(
  name = "scalafix",
  defaultPhase = LifecyclePhase.VERIFY,
  threadSafe = true,
  requiresDependencyResolution = ResolutionScope.TEST
)
public class ScalafixMojo extends BonsaiMojo {

  @Override
  public void execute() throws MojoExecutionException {
    Configuration configuration = configuration();
    Tools tools = tools();

    Path work = work();
    Path file = work.resolve("scalafix.conf");
    write(file, List.of(Scalafix.configuration(configuration, ids(tools))));

    Path root = project.getBasedir().toPath();
    List<String> files = new ArrayList<>();
    for (Path source : sources(project.getCompileSourceRoots())) {
      files.add(root.relativize(source).toString());
    }
    for (Path source : sources(project.getTestCompileSourceRoots())) {
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
    command.add(tools.scalafix(version, scalafixVersion, scalafixScalaVersion).stream()
      .map(File::getAbsolutePath)
      .reduce((a, b) -> a + java.io.File.pathSeparator + b)
      .orElseThrow());
    command.add("scalafix.cli.Cli");
    command.add("--config");
    command.add(file.toString());
    command.add("--check");
    command.add("--sourceroot");
    command.add(root.toString());
    command.add("--classpath");
    command.add(String.join(java.io.File.pathSeparator, classpath));
    // `--files` takes a single path, so repeating it would leave all but one source of the module unchecked
    command.addAll(files);
    run("the scalafix rules", command, root.toFile());
  }

  /** The ids the scalafix rules accept, which they publish them in. */
  private Ids ids(Tools tools) throws MojoExecutionException {
    try {
      return Ids.of(
        tools.compilerPlugin(scalaVersion(), version),
        tools.analysisJar(scalaVersion(), version),
        tools.rulesJar(version));
    } catch (java.io.IOException error) {
      throw new MojoExecutionException("bonsai: cannot read the ids the tools accept", error);
    }
  }
}
