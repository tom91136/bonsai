package net.kurobako.baumkuchen.maven;

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
 * {@code baumkuchen.scalac.args} property, which the build passes to the compiler:
 *
 * <pre>{@code
 * <plugin>
 *   <groupId>net.alchim31.maven</groupId>
 *   <artifactId>scala-maven-plugin</artifactId>
 *   <configuration>
 *     <args>
 *       <arg>@${baumkuchen.scalac.args}</arg>
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
public class PrepareMojo extends BaumkuchenMojo {

  /** The property the path of the response file is set to, which the compiler takes it from. */
  @Parameter(property = "baumkuchen.scalac.args")
  private String scalacArgs;

  /** Whether the compiler writes the SemanticDB the scalafix rules read. */
  @Parameter(property = "baumkuchen.semanticdb", defaultValue = "true")
  private boolean semanticdb;

  @Override
  public void execute() throws MojoExecutionException {
    Configuration configuration = configuration();
    String mode = mode();
    Tools tools = tools();
    File plugin = tools.compilerPlugin(scalaVersion(), version);
    Ids ids;
    try {
      ids = Ids.of(plugin, tools.analysisJar(scalaVersion(), version), tools.rulesJar(version));
      ids.validate(configuration.disabled(), configuration.errors(), configuration.warnings());
    } catch (java.io.IOException error) {
      throw new MojoExecutionException("baumkuchen: cannot read the ids the tools accept", error);
    }

    List<String> options = new ArrayList<>();
    options.add("-Xplugin:" + plugin.getAbsolutePath());
    options.addAll(Scalac.options(configuration, ids, mode));
    if (semanticdb) {
      // The rules read the SemanticDB the compiler writes, which the build keeps out of its published artifacts.
      options.addAll(Scalac.semanticdb(project.getBasedir(), new File(project.getBuild().getOutputDirectory())));
    }

    Path work = work();
    Path file = work.resolve("scalac-args.txt");
    write(file, options);
    getLog().info("baumkuchen: the compiler reads " + file);

    project.getProperties().setProperty("baumkuchen.scalac.args", file.toString());
    if (scalacArgs != null && !scalacArgs.isBlank()) {
      project.getProperties().setProperty("baumkuchen.scalac.args", scalacArgs);
    }
  }
}
