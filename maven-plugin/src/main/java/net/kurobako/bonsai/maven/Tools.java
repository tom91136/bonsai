package net.kurobako.bonsai.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.collection.CollectRequest;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResolutionException;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.resolution.DependencyResult;

import java.io.File;
import java.util.List;

/**
 * The tools the build runs, which the repository the build resolves them from holds.
 *
 * <p>The compiler plugin and the analysis are built for one Scala version each, so the build names the one it uses, and
 * the scalafix rules are built for Scala 3 whatever its version.
 */
public final class Tools {

  private final RepositorySystem repositorySystem;
  private final RepositorySystemSession session;
  private final List<RemoteRepository> repositories;

  public Tools(RepositorySystem repositorySystem, RepositorySystemSession session, List<RemoteRepository> repositories) {
    this.repositorySystem = repositorySystem;
    this.session = session;
    this.repositories = repositories;
  }

  /** The compiler plugin of the Scala version, which the compiler reads. */
  public File compilerPlugin(String scalaVersion, String version) throws MojoExecutionException {
    return artifact("net.kurobako", "bonsai-plugin_" + scalaVersion, version);
  }

  /** The analysis of the Scala version, which the analysis reads the output of the build with. */
  public File analysisJar(String scalaVersion, String version) throws MojoExecutionException {
    return artifact("net.kurobako", "bonsai-analysis_" + scalaVersion, version);
  }

  /** The scalafix rules, which the command that runs them reads. */
  public File rulesJar(String version) throws MojoExecutionException {
    return artifact("net.kurobako", "bonsai-scalafix_3", version);
  }

  /** The javac plugin, with the library it runs on, which the compiler of the build reads. */
  public List<File> javaPlugin(String version) throws MojoExecutionException {
    // The plugin runs on the library of Scala it is written in, which the processor path of the compiler holds alike.
    return resolve(new DefaultArtifact(
      "net.kurobako", Javac.ARTIFACT, null, "jar", version));
  }

  /** The jar of the artifact, which the build resolves without the artifacts it depends on. */
  public File artifact(String groupId, String artifactId, String version) throws MojoExecutionException {
    Artifact artifact = new DefaultArtifact(groupId, artifactId, null, "jar", version);
    try {
      return repositorySystem.resolveArtifact(session, new ArtifactRequest(artifact, repositories, null))
        .getArtifact().getFile();
    } catch (ArtifactResolutionException error) {
      throw new MojoExecutionException(
        "bonsai: cannot resolve " + artifact + ": " + error.getMessage(), error);
    }
  }

  /** The analysis of the Scala version, with the compiler it reads the output with. */
  public List<File> analysis(String scalaVersion, String version) throws MojoExecutionException {
    return resolve(new DefaultArtifact(
      "net.kurobako", "bonsai-analysis_" + scalaVersion, null, "jar", version));
  }

  /** The scalafix rules and the command that runs them. */
  public List<File> scalafix(String version, String scalafixVersion, String scalafixScalaVersion)
    throws MojoExecutionException {
    List<File> classpath = new java.util.ArrayList<>(resolve(new DefaultArtifact(
      "net.kurobako", "bonsai-scalafix_3", null, "jar", version)));
    classpath.addAll(resolve(new DefaultArtifact(
      "ch.epfl.scala", "scalafix-cli_" + scalafixScalaVersion, null, "jar", scalafixVersion)));
    return classpath;
  }

  /** The artifact and the artifacts it depends on, which the tools run beside. */
  private List<File> resolve(Artifact artifact) throws MojoExecutionException {
    try {
      CollectRequest collect = new CollectRequest(new Dependency(artifact, "compile"), repositories);
      DependencyRequest request = new DependencyRequest(collect, null);
      DependencyResult result = repositorySystem.resolveDependencies(session, request);
      return result.getArtifactResults().stream()
        .map(resolved -> resolved.getArtifact().getFile())
        .filter(file -> file != null)
        .toList();
    } catch (DependencyResolutionException error) {
      throw new MojoExecutionException(
        "bonsai: cannot resolve " + artifact + ": " + error.getMessage(), error);
    }
  }
}
