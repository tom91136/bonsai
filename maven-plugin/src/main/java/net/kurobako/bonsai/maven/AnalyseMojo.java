package net.kurobako.bonsai.maven;

import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Runs the analysis, which reads the compiled output of the build and reports what it finds in it.
 *
 * <p>The analysis reads the whole build at once, because what it reports is a judgement about the build: a class
 * nothing refers to in any module is dead, and a public API another module uses is not. So every module writes a fragment
 * of the manifest, which names its output, its model, its classpath, its sources and its compiler options, and the
 * analysis runs in the module that completes the set of the fragments.
 *
 * <p>A build that runs its modules in parallel reaches them in any order, so the module that completes the set is not
 * always the last one of the reactor. Each module records its fragment in the session once it has written it, and the
 * module whose record completes the set merges the fragments of the modules of this build and runs the analysis, which
 * it does once. A module that fails never records its fragment, so the analysis of a build with a failed module does
 * not run.
 */
@Mojo(
  name = "analyse",
  defaultPhase = LifecyclePhase.VERIFY,
  threadSafe = true,
  requiresDependencyResolution = ResolutionScope.TEST
)
public class AnalyseMojo extends BonsaiMojo {

  @Parameter(defaultValue = "${mojoExecution}", readonly = true, required = true)
  private MojoExecution mojoExecution;

  @Override
  public void execute() throws MojoExecutionException {
    Tools tools = tools();
    Configuration configuration = configuration(tools);
    if (configuration.excluded().contains(project.getArtifactId())) {
      return;
    }
    String mode = mode();

    Path work = work();
    Path model = Javac.model(work, "main");
    Path testModel = Javac.model(work, "test");

    List<Path> mainSources = sources(project.getCompileSourceRoots());
    List<Path> testSources = sources(project.getTestCompileSourceRoots());
    File classes = new File(project.getBuild().getOutputDirectory());
    File testClasses = new File(project.getBuild().getTestOutputDirectory());
    Module module = new Module(
      project.getArtifactId(),
      classes.isDirectory() ? classes.toPath() : null,
      testClasses.isDirectory() ? testClasses.toPath() : null,
      Files.exists(model) ? model : null,
      Files.exists(testModel) ? testModel : null,
      mainSources,
      testSources,
      classpath(),
      scalacOptions(),
      Libraries.detect(
        configuration.library(),
        Libraries.skipped(project, session.getUserProperties()),
        Libraries.declares(project.getModel())));

    Path root = root();
    List<String> fragment = Analysis.manifest(root, jdk.toPath(), List.of(module), configuration.text());

    Path fragments = fragments();
    Path file = fragments.resolve(project.getArtifactId() + ".tsv");
    write(file, fragment);
    getLog().info("bonsai: the analysis reads the fragment of " + project.getArtifactId());

    List<String> modules = modules(configuration.excluded());
    if (!Roster.of(repositorySystemSession).record(project.getId(), modules.size())) {
      return;
    }

    List<String> manifest;
    try {
      manifest = Analysis.merge(fragments, modules, root.toString());
    } catch (IOException error) {
      throw new MojoExecutionException("bonsai: cannot read the fragments of the modules", error);
    }
    Path merged = work.resolve("manifest.tsv");
    write(merged, manifest);
    getLog().info("bonsai: the analysis reads " + merged);

    List<File> classpath = analysis(tools);
    List<String> command = new ArrayList<>();
    command.add(javaExecutable());
    command.add("-Xmx" + analysisHeap);
    command.add("-cp");
    command.add(Javac.processorPath(classpath));
    command.add("net.kurobako.bonsai.analysis.CLI");
    command.add("--manifest");
    command.add(merged.toString());
    command.add("--mode");
    command.add(mode);
    command.add("--parallelism");
    command.add(Integer.toString(session.getRequest().getDegreeOfConcurrency()));
    command.addAll(configuration.options());
    String baseline = writeBaseline();
    if (baseline != null) {
      command.add("--write-baseline");
      command.add(root.resolve(baseline).toString());
    }
    run("the analysis", command, root.toFile());
  }

  /** The directory the fragments of the modules are kept in, which is at the root of the build. */
  private Path fragments() throws MojoExecutionException {
    return directory(root().resolve("target").resolve("bonsai").resolve("manifests"));
  }

  /** Whether the module binds the analysis goal, so that its fragments are read by the analysis. */
  static boolean analyses(MavenProject other) {
    return other.getBuildPlugins().stream()
      .filter(plugin -> plugin.getArtifactId().equals("bonsai-maven-plugin"))
      .flatMap(plugin -> plugin.getExecutions().stream())
      .anyMatch(execution -> execution.getGoals().contains("analyse"));
  }

  /**
   * The modules of this build that analyse it, which are the ones whose fragments the analysis reads: every module when
   * the goal is named on the command line, which runs it in each of them, else the modules that bind it, less the
   * modules the configuration leaves out.
   */
  private List<String> modules(Set<String> excluded) {
    boolean named = mojoExecution.getSource() == MojoExecution.Source.CLI;
    return session.getProjects().stream()
      .filter(other -> named || analyses(other))
      .filter(other -> !excluded.contains(other.getArtifactId()))
      .map(MavenProject::getArtifactId)
      .toList();
  }
}
