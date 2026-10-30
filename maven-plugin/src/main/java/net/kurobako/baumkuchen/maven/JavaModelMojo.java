package net.kurobako.baumkuchen.maven;

import org.apache.maven.plugin.MojoExecutionException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Collects the model of the Java code of the module, which the javac plugin writes while the compiler compiles it.
 *
 * <p>The goal runs before the compile of its scope, so that the property the compiler reads names the file the model of
 * that scope is written to. The plugin is found on the processor path of the compiler, which the build takes from the
 * property the goal sets, and the file of the model is an argument of the plugin, so the compile of a module names its
 * own file even when the modules of the build are compiled in parallel:
 *
 * <pre>{@code
 * <plugin>
 *   <groupId>org.apache.maven.plugins</groupId>
 *   <artifactId>maven-compiler-plugin</artifactId>
 *   <configuration>
 *     <compilerArgs>
 *       <arg>-processorpath</arg>
 *       <arg>${baumkuchen.java.plugin}</arg>
 *       <arg>-Xplugin:baumkuchen ${baumkuchen.model}</arg>
 *     </compilerArgs>
 *   </configuration>
 * </plugin>
 * }</pre>
 *
 * <p>The build binds the goal of the main code and the goal of the test code, so that the two compiles of a module
 * write their models to files of their own. The path of the file is a property of the project, so each compile of a
 * module reads the path of its own scope, and not the path of a compile that runs beside it.
 */
public abstract class JavaModelMojo extends BaumkuchenMojo {

  /** The scope the model is collected for, which is the scope of the compile the plugin runs in. */
  protected abstract String scope();

  /** The source roots the code of the scope is in, which the model is collected from. */
  protected abstract List<String> roots();

  @Override
  public void execute() throws MojoExecutionException {
    Path model = Javac.model(work(), scope());
    // The property of the project names the file the compiler passes to the plugin, so that the compile of a module
    // names its own file even when the modules of the build are compiled in parallel.
    project.getProperties().setProperty(Javac.MODEL, model.toAbsolutePath().toString());
    // The compiler takes the javac plugin from the path the property names, which is named even when the module has no
    // Java code, as the compiler of a module without it still reads the property.
    project.getProperties().setProperty(Javac.PLUGIN, "");

    if (sources(roots(), ".java").isEmpty()) {
      getLog().info("baumkuchen: the module has no " + scope() + " Java code, so it has no model");
      // A model of code that is not there any more is of no use to the analysis.
      try {
        Files.deleteIfExists(model);
      } catch (IOException error) {
        getLog().warn("baumkuchen: cannot remove the model of the " + scope() + " code: " + error.getMessage());
      }
      return;
    }

    List<File> plugin;
    try {
      plugin = tools().javaPlugin(version);
    } catch (MojoExecutionException error) {
      getLog().warn("baumkuchen: the javac plugin cannot be resolved, so the model of the Java code is not " +
        "collected: " + error.getMessage());
      return;
    }

    // The compiler takes the plugin from the processor path, which the build reads from the property of the project.
    project.getProperties().setProperty(Javac.PLUGIN, Javac.processorPath(plugin));

    if (!processorPathConfigured()) {
      getLog().warn("baumkuchen: the build does not put the javac plugin on the processor path of the compiler, so " +
        "the compiler does not run it and the model of the Java code is not collected; the compiler plugin of the " +
        "build has to take the processor path from the " + Javac.PLUGIN + " property");
    }

    getLog().info("baumkuchen: the compiler writes the model of the " + scope() + " code to " + model);
  }

  /** Whether the build names the javac plugin in the configuration of the compiler plugin. */
  private boolean processorPathConfigured() {
    for (org.apache.maven.model.Plugin plugin : project.getBuildPlugins()) {
      if (!plugin.getArtifactId().equals("maven-compiler-plugin")) {
        continue;
      }
      if (names(plugin.getConfiguration())) {
        return true;
      }
      for (org.apache.maven.model.PluginExecution execution : plugin.getExecutions()) {
        if (names(execution.getConfiguration())) {
          return true;
        }
      }
    }
    return false;
  }

  /** Whether the configuration names the javac plugin or the property of its processor path. */
  private boolean names(Object configuration) {
    if (!(configuration instanceof org.codehaus.plexus.util.xml.Xpp3Dom dom)) {
      return false;
    }
    String xml = dom.toString();
    return xml.contains(Javac.PLUGIN) || xml.contains(Javac.ARTIFACT);
  }
}
