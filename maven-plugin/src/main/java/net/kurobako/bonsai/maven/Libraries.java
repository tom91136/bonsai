package net.kurobako.bonsai.maven;

import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import java.util.Optional;
import java.util.Properties;

/**
 * Whether other builds use the public API of a module, as the sbt plugin decides it.
 *
 * <p>The configuration decides when it names the module. Otherwise a module skipped from deploying is no library, and
 * a deployed one is a library when it declares licences, developers or a url, as an artifact for others does. The
 * sbt plugin also takes a deployed module without a main class for a library, which needs the classes read and is left
 * out here.
 *
 * <p>The declarations are those of the effective model, so a module that inherits them from its parent is the same as
 * one that spells them out.
 */
public final class Libraries {

  private static final String SKIP = "maven.deploy.skip";

  private Libraries() {
  }

  /** Whether the module is a library. */
  public static boolean detect(Optional<Boolean> configured, boolean skipped, boolean declared) {
    return configured.orElse(!skipped && declared);
  }

  /** Whether the model declares licences, developers or a url. */
  public static boolean declares(Model model) {
    return !model.getLicenses().isEmpty()
      || !model.getDevelopers().isEmpty()
      || model.getUrl() != null && !model.getUrl().isBlank();
  }

  /** Whether the deploy plugin skips the module, by its own configuration or else the property the command line or the model sets. */
  public static boolean skipped(MavenProject project, Properties user) {
    String named = project.getBuildPlugins().stream()
      .filter(plugin -> plugin.getArtifactId().equals("maven-deploy-plugin"))
      .map(Libraries::skip)
      .flatMap(Optional::stream)
      .findFirst()
      .orElseGet(() -> user.getProperty(SKIP, project.getProperties().getProperty(SKIP)));
    return Boolean.parseBoolean(named);
  }

  private static Optional<String> skip(Plugin plugin) {
    if (plugin.getConfiguration() instanceof Xpp3Dom configuration) {
      Xpp3Dom skip = configuration.getChild("skip");
      if (skip != null && skip.getValue() != null && !skip.getValue().startsWith("${")) {
        return Optional.of(skip.getValue().strip());
      }
    }
    return Optional.empty();
  }
}
