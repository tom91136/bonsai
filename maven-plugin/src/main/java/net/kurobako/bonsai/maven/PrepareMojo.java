package net.kurobako.bonsai.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.plugins.annotations.Parameter;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Writes the options the compiler plugin reads, which the compiler of the build takes from a response file.
 *
 * <p>The response file holds the plugin itself and the options the configuration gives it, and its path is the
 * {@code bonsai.scalac.args} property, which the build passes to the compiler:
 *
 * <pre>{@code
 * <plugin>
 *   <groupId>net.alchim31.maven</groupId>
 *   <artifactId>scala-maven-plugin</artifactId>
 *   <configuration>
 *     <args>
 *       <arg>@${bonsai.scalac.args}</arg>
 *     </args>
 *   </configuration>
 * </plugin>
 * }</pre>
 */
@Mojo(
  name = "prepare",
  defaultPhase = LifecyclePhase.GENERATE_SOURCES,
  threadSafe = true,
  requiresDependencyResolution = ResolutionScope.TEST
)
public class PrepareMojo extends BonsaiMojo {

  /** The property the path of the response file is set to, which the compiler takes it from. */
  @Parameter(property = "bonsai.scalac.args")
  private String scalacArgs;

  /** Whether the compiler writes the SemanticDB the scalafix rules read. */
  @Parameter(property = "bonsai.semanticdb", defaultValue = "true")
  private boolean semanticdb;

  @Override
  public void execute() throws MojoExecutionException {
    Tools tools = tools();
    Configuration configuration = configuration(tools);
    String mode = mode();
    File plugin = compilerPlugin(tools);
    File rules = tools.rulesJar(version);
    Ids ids = ids(tools, plugin, rules);
    try {
      ids.validate(configuration.disabled(), configuration.enabled(), configuration.errors(), configuration.nonfatal());
    } catch (java.io.IOException error) {
      throw new MojoExecutionException("bonsai: cannot read the ids the tools accept", error);
    }

    List<String> options = new ArrayList<>();
    if (!configuration.excluded().contains(project.getArtifactId())) {
      File output = new File(project.getBuild().getDirectory());
      options.add("-Xplugin:" + plugin.getAbsolutePath());
      options.addAll(Scalac.options(configuration, ids, mode,
        List.of(new File(output, "generated-sources"), new File(output, "generated-test-sources"))));
      options.addAll(Scalac.sourceroot(root().toFile()));
      if (semanticdb) {
        // The rules read the SemanticDB the compiler writes, which the build keeps out of its published artifacts.
        options.addAll(Scalac.semanticdb(new File(project.getBuild().getOutputDirectory())));
        options.addAll(Scalac.unused(configuration, ids, support(rules), scalaVersion(tools), scalacOptions()));
      }
    }

    Path work = work();
    Path file = work.resolve("scalac-args.txt");
    write(file, options);
    getLog().info("bonsai: the compiler reads " + file);

    project.getProperties().setProperty(
      "bonsai.scalac.args", scalacArgs != null && !scalacArgs.isBlank() ? scalacArgs : file.toString());
  }
}
