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
 * <p>The compiler plugin is built for each minor line of Scala 3, so the build names the line it uses. The analysis is
 * built once, with the newest compiler, which reads the TASTy of every older line, and the scalafix rules are built for
 * Scala 3 whatever its version.
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

  /** The compiler plugin of the minor line of Scala, which the compiler reads. */
  public File compilerPlugin(String line, String version) throws MojoExecutionException {
    return artifact("net.kurobako", "bonsai-plugin_" + line, version);
  }

  /** The analysis, which reads the output of the build. */
  public File analysisJar(String version) throws MojoExecutionException {
    return artifact("net.kurobako", "bonsai-analysis_3", version);
  }

  /** The scalafix rules, which the command that runs them reads. */
  public File rulesJar(String version) throws MojoExecutionException {
    return artifact("net.kurobako", "bonsai-scalafix_3", version);
  }

  /** The javac plugin, with the library it runs on, which the compiler of the build reads. */
  public List<File> javaPlugin(String version) throws MojoExecutionException {
    // The plugin runs on the library of Scala it is written in, which the processor path of the compiler holds alike.
    return bonsai(Javac.ARTIFACT, version);
  }

  /** The jar of the artifact, which the build resolves without the artifacts it depends on. */
  private File artifact(String groupId, String artifactId, String version) throws MojoExecutionException {
    Artifact artifact = new DefaultArtifact(groupId, artifactId, null, "jar", version);
    try {
      return repositorySystem.resolveArtifact(session, new ArtifactRequest(artifact, repositories, null))
        .getArtifact().getFile();
    } catch (ArtifactResolutionException error) {
      throw new MojoExecutionException(
        "bonsai: cannot resolve " + artifact + ": " + error.getMessage(), error);
    }
  }

  /** The analysis, with the compiler it reads the output with. */
  public List<File> analysis(String version) throws MojoExecutionException {
    return bonsai("bonsai-analysis_3", version);
  }

  /** The scalafix rules and the command that runs them. */
  public List<File> scalafix(String version, String scalafixVersion, String scalafixScalaVersion)
    throws MojoExecutionException {
    List<File> classpath = new java.util.ArrayList<>(bonsai("bonsai-scalafix_3", version));
    classpath.addAll(resolve(new DefaultArtifact(
      "ch.epfl.scala", "scalafix-cli_" + scalafixScalaVersion, null, "jar", scalafixVersion)));
    return classpath;
  }

  /** The bonsai artifact and the artifacts it depends on. */
  private List<File> bonsai(String artifactId, String version) throws MojoExecutionException {
    return resolve(new DefaultArtifact("net.kurobako", artifactId, null, "jar", version));
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
