package net.kurobako.baumkuchen.maven;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.repository.RemoteRepository;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The settings the baumkuchen tools take, which the goals of this plugin share.
 *
 * <p>The configuration of the build is the HOCON the sbt plugin reads, so both builds configure baumkuchen the same
 * way. The tools are found in the repository the build resolves them from, which is where their own build installs
 * them.
 */
public abstract class BaumkuchenMojo extends AbstractMojo {

  /** The project the goals run in. */
  @Parameter(defaultValue = "${project}", readonly = true, required = true)
  protected MavenProject project;

  /** The session of the build, which names the directory it started in. */
  @Parameter(defaultValue = "${session}", readonly = true, required = true)
  protected MavenSession session;

  /** The repository the tools are found in. */
  @javax.inject.Inject
  protected RepositorySystem repositorySystem;

  /** The session the resolver reads the repository with. */
  @Parameter(defaultValue = "${repositorySystemSession}", readonly = true, required = true)
  protected RepositorySystemSession repositorySystemSession;

  /** The repositories the build resolves its dependencies from, which the tools are found in. */
  @Parameter(defaultValue = "${project.remoteProjectRepositories}", readonly = true, required = true)
  protected List<RemoteRepository> remoteRepositories;

  /** The configuration of the build, which the tools take their options from. It is at the root of the build, as it is
    * in a build of sbt. */
  @Parameter(defaultValue = "${session.executionRootDirectory}/.baumkuchen.conf", property = "baumkuchen.config")
  protected File config;

  /** The version of the tools, which have no published release. */
  @Parameter(property = "baumkuchen.version", defaultValue = "0.1.0-SNAPSHOT")
  protected String version;

  /** The Scala version the tools of the build are built for. */
  @Parameter(property = "baumkuchen.scalaVersion", defaultValue = "${scala.version}")
  protected String scalaVersion;

  /** The version of Scala the scalafix rules are built for, which is the one they parse the sources with. */
  @Parameter(property = "baumkuchen.scalafixScalaVersion", defaultValue = "3.9.0")
  protected String scalafixScalaVersion;

  /** The version of scalafix the rules run with. */
  @Parameter(property = "baumkuchen.scalafixVersion", defaultValue = "0.14.9")
  protected String scalafixVersion;

  /** The Scala version the tools are used with when the build does not name one, which the tools are published for. */
  @Parameter(property = "baumkuchen.scala.version", defaultValue = "3.10.0")
  protected String defaultScalaVersion;

  /** The mode the tools run in, which the environment names when it is not set here. */
  @Parameter(property = "baumkuchen.mode")
  protected String mode;

  /** The heap the analysis may take, which it reads the output of the build with. */
  @Parameter(property = "baumkuchen.analysisHeap", defaultValue = "4g")
  protected String analysisHeap;

  /** The configuration of the build, for the module of this project. */
  protected Configuration configuration() throws MojoExecutionException {
    try {
      return Configuration.read(config.toPath(), project.getArtifactId());
    } catch (IOException error) {
      throw new MojoExecutionException(error.getMessage(), error);
    }
  }

  /** The mode the tools run in. */
  protected String mode() {
    if (mode != null && !mode.isBlank()) {
      return mode.toLowerCase(java.util.Locale.ROOT);
    }
    try {
      return Mode.of(System.getenv());
    } catch (IllegalArgumentException error) {
      getLog().warn(error.getMessage());
      return "dev";
    }
  }

  /** The file the environment names to record the findings of the run in, which rewrites the baseline. */
  protected String writeBaseline() {
    String named = System.getenv("BAUMKUCHEN_WRITE_BASELINE");
    return named != null && !named.isBlank() ? named : null;
  }

  /** The tools the build runs, which the repository holds. */
  protected Tools tools() {
    return new Tools(repositorySystem, repositorySystemSession, remoteRepositories);
  }

  /** The Scala version the tools of the build are built for, which the build names, or the one the tools are used with
    * when it does not. A build of Java code alone has no Scala version of its own, and the tools are still the ones
    * that read the model of the code. */
  protected String scalaVersion() {
    if (scalaVersion != null && !scalaVersion.isBlank() && !scalaVersion.startsWith("${")) {
      return scalaVersion;
    }
    for (org.apache.maven.model.Dependency dependency : project.getDependencies()) {
      if (dependency.getArtifactId().equals("scala3-library_3")) {
        return dependency.getVersion();
      }
    }
    String named = project.getProperties().getProperty("scala.version");
    if (named != null && !named.isBlank()) {
      return named;
    }
    return defaultScalaVersion;
  }

  /** The directory the goals write their files to. */
  protected Path work() throws MojoExecutionException {
    Path work = Path.of(project.getBuild().getDirectory()).resolve("baumkuchen");
    try {
      Files.createDirectories(work);
    } catch (IOException error) {
      throw new MojoExecutionException("baumkuchen: cannot create " + work, error);
    }
    return work;
  }

  /** The files of the classpath of the project, which the analysis and the rules read. The output of the project is
    * not among them, because the manifest names it apart. */
  protected List<String> classpath() {
    Set<String> outputs = Set.of(
      project.getBuild().getOutputDirectory(),
      project.getBuild().getTestOutputDirectory());
    return project.getArtifacts().stream()
      .map(artifact -> artifact.getFile())
      .filter(java.util.Objects::nonNull)
      .map(File::getAbsolutePath)
      .filter(element -> !outputs.contains(element))
      .collect(java.util.stream.Collectors.toList());
  }

  /** The source files of the scope, which the analysis and the rules read. */
  protected List<Path> sources(List<String> roots) {
    return sources(roots, ".scala");
  }

  /** The source files of the scope with the given extension, which the tools read. */
  protected List<Path> sources(List<String> roots, String extension) {
    List<Path> sources = new ArrayList<>();
    for (String root : roots) {
      Path directory = Path.of(root);
      if (!Files.isDirectory(directory)) {
        continue;
      }
      try (var files = Files.walk(directory)) {
        files.filter(file -> file.getFileName().toString().endsWith(extension)).forEach(sources::add);
      } catch (IOException error) {
        getLog().warn("baumkuchen: cannot read the sources of " + directory + ": " + error.getMessage());
      }
    }
    sources.sort(Path::compareTo);
    return sources;
  }

  /** The compiler options of the project, which the analysis reads to know whether its warnings fail the build. */
  protected List<String> scalacOptions() {
    List<String> options = new ArrayList<>();
    for (org.apache.maven.model.Plugin plugin : project.getBuildPlugins()) {
      if (!plugin.getArtifactId().equals("scala-maven-plugin")) {
        continue;
      }
      Object configuration = plugin.getConfiguration();
      if (!(configuration instanceof org.codehaus.plexus.util.xml.Xpp3Dom dom)) {
        continue;
      }
      org.codehaus.plexus.util.xml.Xpp3Dom args = dom.getChild("args");
      if (args == null) {
        continue;
      }
      for (org.codehaus.plexus.util.xml.Xpp3Dom arg : args.getChildren("arg")) {
        options.add(arg.getValue());
      }
    }
    return options;
  }

  /** Writes the lines to the file. */
  protected void write(Path file, List<String> lines) throws MojoExecutionException {
    try {
      Files.createDirectories(file.getParent());
      Files.writeString(file, String.join("\n", lines) + (lines.isEmpty() ? "" : "\n"), StandardCharsets.UTF_8);
    } catch (IOException error) {
      throw new MojoExecutionException("baumkuchen: cannot write " + file, error);
    }
  }

  /** The command that runs a JVM of its own, which the tools run in. */
  protected String javaExecutable() {
    return Path.of(System.getProperty("java.home"), "bin", "java").toString();
  }

  /** Runs the command, and fails the build when it does. */
  protected void run(String tool, List<String> command, File directory) throws MojoExecutionException {
    getLog().info("baumkuchen: " + String.join(" ", command));
    ProcessBuilder builder = new ProcessBuilder(command);
    if (directory != null) {
      builder.directory(directory);
    }
    builder.redirectErrorStream(true);
    try {
      Process process = builder.start();
      String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
      int code = process.waitFor();
      for (String line : output.split("\n")) {
        getLog().info(line);
      }
      if (code != 0) {
        throw new MojoExecutionException("baumkuchen: " + tool + " failed with " + code);
      }
    } catch (IOException error) {
      throw new MojoExecutionException("baumkuchen: cannot run " + command.get(0), error);
    } catch (InterruptedException error) {
      Thread.currentThread().interrupt();
      throw new MojoExecutionException("baumkuchen: " + command.get(0) + " was interrupted", error);
    }
  }
}
