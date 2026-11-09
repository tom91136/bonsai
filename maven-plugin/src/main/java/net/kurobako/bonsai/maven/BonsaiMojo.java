package net.kurobako.bonsai.maven;

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
import java.util.Optional;
import java.util.Set;
import java.util.function.IntPredicate;

/**
 * The settings the bonsai tools take, which the goals of this plugin share.
 *
 * <p>The configuration of the build is the HOCON the sbt plugin reads, which the analysis reads for this plugin, so
 * both builds configure bonsai the same way. The tools are found in the repository the build resolves them from, which is where their own build installs
 * them.
 */
public abstract class BonsaiMojo extends AbstractMojo {

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
  @Parameter(defaultValue = "${session.executionRootDirectory}/.bonsai.conf", property = "bonsai.config")
  protected File config;

  /** The version of the tools, which have no published release. */
  @Parameter(property = "bonsai.version", defaultValue = "0.1.0-SNAPSHOT")
  protected String version;

  /** The Scala version the tools of the build are built for. */
  @Parameter(property = "bonsai.scalaVersion", defaultValue = "${scala.version}")
  protected String scalaVersion;

  /** The version of Scala the scalafix rules are built for, which is the one they parse the sources with. */
  @Parameter(property = "bonsai.scalafixScalaVersion")
  protected String scalafixScalaVersion;

  /** The version of scalafix the rules run with. */
  @Parameter(property = "bonsai.scalafixVersion")
  protected String scalafixVersion;

  /** The mode the tools run in, which the environment names when it is not set here. */
  @Parameter(property = "bonsai.mode")
  protected String mode;

  /** The heap the analysis may take, which it reads the output of the build with. */
  @Parameter(property = "bonsai.analysisHeap", defaultValue = "4g")
  protected String analysisHeap;

  /** The home of the JDK the build targets, whose classes the analysis reads before those of the build. */
  @Parameter(property = "bonsai.jdk", defaultValue = "${java.home}")
  protected File jdk;

  /** The configuration of the build for the module of this project, which the analysis jar reads. */
  protected Configuration configuration(Tools tools) throws MojoExecutionException {
    try {
      Path entries = Files.createTempFile("bonsai-configuration", ".tsv");
      try {
        Configuration configuration = read(
          List.of(
            javaExecutable(),
            "-cp",
            Javac.processorPath(tools.analysis(version)),
            "net.kurobako.bonsai.analysis.Configure",
            "--config",
            config.toString(),
            "--module",
            project.getArtifactId(),
            "--out",
            entries.toString()),
          entries);
        List<String> strangers = configuration.excluded().stream()
          .filter(id -> session.getAllProjects().stream().noneMatch(other -> other.getArtifactId().equals(id)))
          .toList();
        if (!strangers.isEmpty()) {
          throw new IOException("bonsai: excluded-modules names modules the build does not have: " + strangers);
        }
        return configuration;
      } finally {
        Files.deleteIfExists(entries);
      }
    } catch (IOException error) {
      throw new MojoExecutionException("bonsai: cannot read " + config, error);
    } catch (InterruptedException error) {
      Thread.currentThread().interrupt();
      throw new MojoExecutionException("bonsai: reading " + config + " was interrupted", error);
    }
  }

  /** Runs the reader and parses the entries it writes to the file, leaving its stdout to the console. */
  static Configuration read(List<String> command, Path entries)
    throws IOException, InterruptedException, MojoExecutionException {
    Process process = new ProcessBuilder(command).redirectOutput(ProcessBuilder.Redirect.INHERIT).start();
    String error = new String(process.getErrorStream().readAllBytes(), StandardCharsets.UTF_8).strip();
    int code = process.waitFor();
    if (code != 0) {
      throw new MojoExecutionException(
        error.isEmpty() ? "bonsai: the configuration reader failed with " + code : error);
    }
    return Configuration.parse(Files.readAllLines(entries, StandardCharsets.UTF_8));
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
    String named = System.getenv("BONSAI_WRITE_BASELINE");
    return named != null && !named.isBlank() ? named : null;
  }

  /** The tools the build runs, which the repository holds. */
  protected Tools tools() {
    return new Tools(repositorySystem, repositorySystemSession, remoteRepositories);
  }

  /** The Scala version of the build, or the one the analysis is built with when the build names none. A build of Java
    * code alone has no Scala version of its own, and the tools are still the ones that read the model of the code. */
  protected String scalaVersion(Tools tools) throws MojoExecutionException {
    Optional<String> named = namedScalaVersion();
    return named.isPresent() ? named.get() : analysisScala(tools);
  }

  /** The Scala version the build names, if it names one. */
  protected Optional<String> namedScalaVersion() {
    if (scalaVersion != null && !scalaVersion.isBlank() && !scalaVersion.startsWith("${")) {
      return Optional.of(scalaVersion);
    }
    for (org.apache.maven.model.Dependency dependency : project.getDependencies()) {
      if (dependency.getArtifactId().equals("scala3-library_3")) {
        return Optional.of(dependency.getVersion());
      }
    }
    String named = project.getProperties().getProperty("scala.version");
    return named != null && !named.isBlank() ? Optional.of(named) : Optional.empty();
  }

  /** The Scala version the analysis is built with, which is the newest TASTy it reads. */
  protected String analysisScala(Tools tools) throws MojoExecutionException {
    try {
      return Versions.analysisScala(tools.analysisJar(version));
    } catch (IOException error) {
      throw new MojoExecutionException(error.getMessage(), error);
    }
  }

  /** The versions of scalafix the rules run with, which the rules jar publishes unless the build names them. */
  protected Versions.ScalafixVersions scalafix(File rules) throws MojoExecutionException {
    Versions.ScalafixVersions published;
    try {
      published = Versions.scalafix(rules);
    } catch (IOException error) {
      throw new MojoExecutionException(error.getMessage(), error);
    }
    return new Versions.ScalafixVersions(
      named(scalafixVersion, published.version()), named(scalafixScalaVersion, published.scalaVersion()));
  }

  /** The releases the scalafix rules that need the compiler of the build run in, which the rules jar publishes. */
  protected Versions.Support support(File rules) throws MojoExecutionException {
    try {
      return Versions.support(rules);
    } catch (IOException error) {
      throw new MojoExecutionException(error.getMessage(), error);
    }
  }

  private static String named(String named, String published) {
    return named != null && !named.isBlank() ? named : published;
  }

  /** The compiler plugin for the Scala version of the build, which every patch of its minor line shares. */
  protected File compilerPlugin(Tools tools) throws MojoExecutionException {
    String scala = scalaVersion(tools);
    String line = Versions.line(scala).orElseThrow(() ->
      new MojoExecutionException("bonsai: no compiler plugin for Scala " + scala + "; it is built for the final " +
        "releases of Scala 3"));
    File plugin;
    try {
      plugin = tools.compilerPlugin(line, version);
    } catch (MojoExecutionException error) {
      throw new MojoExecutionException(
        "bonsai: no compiler plugin for Scala " + scala + "; " + error.getMessage(), error);
    }
    try {
      String oldest = Versions.oldest(plugin);
      if (!Versions.runsIn(scala, oldest)) {
        throw new MojoExecutionException("bonsai: no compiler plugin for Scala " + scala + "; the plugin of its " +
          "line runs in " + oldest + " and later patches");
      }
    } catch (IOException error) {
      throw new MojoExecutionException(error.getMessage(), error);
    }
    return plugin;
  }

  /** The analysis, when it reads the TASTy of the Scala version of the build. */
  protected List<File> analysis(Tools tools) throws MojoExecutionException {
    String newest = analysisScala(tools);
    String scala = namedScalaVersion().orElse(newest);
    if (!Versions.readable(scala, newest)) {
      throw new MojoExecutionException("bonsai: no analysis for Scala " + scala + "; it reads the TASTy of Scala " +
        newest + " and the minor lines before it");
    }
    return tools.analysis(version);
  }

  /** The directory the build was started in. */
  protected Path root() {
    return Path.of(session.getExecutionRootDirectory());
  }

  /** Whether the tools leave a finding of their rewriting rules unfailed, as the dev and audit modes do. */
  protected boolean dev() {
    return mode().equals("dev") || mode().equals("audit");
  }

  /** The directory the goals write their files to. */
  protected Path work() throws MojoExecutionException {
    return directory(Path.of(project.getBuild().getDirectory()).resolve("bonsai"));
  }

  /** Creates the directory, and its parents. */
  protected Path directory(Path path) throws MojoExecutionException {
    try {
      Files.createDirectories(path);
    } catch (IOException error) {
      throw new MojoExecutionException("bonsai: cannot create " + path, error);
    }
    return path;
  }

  /** The ids the tools accept, which the tools publish them in. */
  protected Ids ids(Tools tools, File plugin, File rules) throws MojoExecutionException {
    try {
      return Ids.of(plugin, tools.analysisJar(version), rules);
    } catch (IOException error) {
      throw new MojoExecutionException("bonsai: cannot read the ids the tools accept", error);
    }
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
        getLog().warn("bonsai: cannot read the sources of " + directory + ": " + error.getMessage());
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
      throw new MojoExecutionException("bonsai: cannot write " + file, error);
    }
  }

  /** The command that runs a JVM of its own, which the tools run in. */
  protected String javaExecutable() {
    return Path.of(System.getProperty("java.home"), "bin", "java").toString();
  }

  /** Runs the command, and fails the build when it does. */
  protected void run(String tool, List<String> command, File directory) throws MojoExecutionException {
    run(tool, command, directory, code -> false);
  }

  /** Runs the command, and fails the build when it fails with a code other than the ones that only report findings. */
  protected void run(String tool, List<String> command, File directory, IntPredicate findings)
    throws MojoExecutionException {
    getLog().info("bonsai: " + String.join(" ", command));
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
      if (code != 0 && findings.test(code)) {
        getLog().warn("bonsai: " + tool + " report findings");
      } else if (code != 0) {
        throw new MojoExecutionException("bonsai: " + tool + " failed with " + code);
      }
    } catch (IOException error) {
      throw new MojoExecutionException("bonsai: cannot run " + command.get(0), error);
    } catch (InterruptedException error) {
      Thread.currentThread().interrupt();
      throw new MojoExecutionException("bonsai: " + command.get(0) + " was interrupted", error);
    }
  }
}
