package net.kurobako.bonsai.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Runs the analysis, which reads the compiled output of the build and reports what it finds in it.
 *
 * <p>The analysis reads the whole build at once, because what it reports is a judgement about the build: a class
 * nothing refers to in any module is dead, and a public API another module uses is not. So every module writes a fragment
 * of the manifest, which names its output, its model, its classpath, its sources and its compiler options, and the
 * analysis runs when the last module of the reactor has written its fragment.
 *
 * <p>The last module of the reactor is the last one the build reaches, so the fragments of the others are all there by
 * the time it runs. A build that runs its modules in parallel reaches them in another order, so the analysis of a build
 * that does is not the analysis of the whole build; the build says so when it does.
 */
@Mojo(
  name = "analyse",
  defaultPhase = LifecyclePhase.VERIFY,
  threadSafe = true,
  requiresDependencyResolution = ResolutionScope.TEST
)
public class AnalyseMojo extends BonsaiMojo {

  @Override
  public void execute() throws MojoExecutionException {
    Tools tools = tools();
    Configuration configuration = configuration(tools);
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
      configuration.library());

    Path root = root();
    List<String> fragment = Analysis.manifest(root, List.of(module), configuration.text());

    // The fragments of the modules are kept apart, and the manifest of the whole build is made of all of them.
    Path fragments = fragments();
    Path file = fragments.resolve(project.getArtifactId() + ".tsv");
    write(file, fragment);
    getLog().info("bonsai: the analysis reads the fragment of " + project.getArtifactId());

    if (!lastOfTheReactor()) {
      return;
    }
    if (session.getRequest().getDegreeOfConcurrency() > 1 && analysers() > 1) {
      getLog().warn("bonsai: the build runs its modules in parallel, so the analysis may not have the fragment of " +
        "every module of the build; the analysis of the whole build needs a build that runs them one after another");
    }

    List<String> manifest;
    try {
      manifest = Analysis.merge(fragments, root.toString());
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

  /**
   * Whether this module is the last one the build reaches that analyses the build.
   *
   * <p>The modules that do not analyse the build are left out, so that a build whose analysis is bound in some of its
   * modules alone still runs the analysis of the whole build in the last of them.
   */
  private boolean lastOfTheReactor() {
    List<org.apache.maven.project.MavenProject> projects = session.getProjects();
    return projects.subList(projects.indexOf(project) + 1, projects.size()).stream().noneMatch(this::analyses);
  }

  /** Whether the module analyses the build. */
  private boolean analyses(org.apache.maven.project.MavenProject other) {
    return other.getBuildPlugins().stream()
      .anyMatch(plugin -> plugin.getArtifactId().equals("bonsai-maven-plugin"));
  }

  /** How many modules of the build analyse it, which is how many fragments the analysis reads. */
  private long analysers() {
    return session.getProjects().stream().filter(this::analyses).count();
  }
}
