package net.kurobako.bonsai.maven;

import org.apache.maven.model.Build;
import org.apache.maven.model.Developer;
import org.apache.maven.model.License;
import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LibrariesTest {

  @Test
  void theConfigurationDecidesWhateverTheModuleLooksLike() {
    assertTrue(Libraries.detect(Optional.of(true), true, false));
    assertFalse(Libraries.detect(Optional.of(false), false, true));
  }

  @Test
  void aModuleSkippedFromDeployingIsNoLibraryWhateverItDeclares() {
    assertFalse(Libraries.detect(Optional.empty(), true, true));
  }

  @Test
  void aDeployedModuleIsALibraryWhenItDeclaresItsLicencesDevelopersOrHomepage() {
    assertTrue(Libraries.detect(Optional.empty(), false, true));
    assertFalse(Libraries.detect(Optional.empty(), false, false));
  }

  @Test
  void aModuleThatDeclaresNothingIsNoLibrary() {
    assertFalse(Libraries.declares(new Model()));
  }

  @Test
  void aLicenceADeveloperOrAUrlIsADeclaration() {
    Model licensed = new Model();
    licensed.addLicense(new License());
    Model developed = new Model();
    developed.addDeveloper(new Developer());
    Model homepaged = new Model();
    homepaged.setUrl("https://example.org/demo");

    assertTrue(Libraries.declares(licensed));
    assertTrue(Libraries.declares(developed));
    assertTrue(Libraries.declares(homepaged));
  }

  @Test
  void aBlankUrlIsNoDeclaration() {
    Model model = new Model();
    model.setUrl("  ");

    assertFalse(Libraries.declares(model));
  }

  @Test
  void aModuleIsSkippedByTheDeployPropertyInItsModelOrOnTheCommandLine() {
    MavenProject modelled = project();
    modelled.getModel().getProperties().setProperty("maven.deploy.skip", "true");
    Properties user = new Properties();
    user.setProperty("maven.deploy.skip", "true");

    assertTrue(Libraries.skipped(modelled, new Properties()));
    assertTrue(Libraries.skipped(project(), user));
    assertFalse(Libraries.skipped(project(), new Properties()));
  }

  @Test
  void theCommandLineOverridesTheDeployPropertyOfTheModel() {
    MavenProject modelled = project();
    modelled.getModel().getProperties().setProperty("maven.deploy.skip", "true");
    Properties user = new Properties();
    user.setProperty("maven.deploy.skip", "false");

    assertFalse(Libraries.skipped(modelled, user));
  }

  @Test
  void aModuleIsSkippedByTheConfigurationOfTheDeployPlugin() {
    MavenProject project = project();
    project.getModel().getBuild().addPlugin(deploy("true"));
    MavenProject other = project();
    other.getModel().getBuild().addPlugin(deploy("false"));

    assertTrue(Libraries.skipped(project, new Properties()));
    assertFalse(Libraries.skipped(other, new Properties()));
  }

  private static MavenProject project() {
    MavenProject project = new MavenProject();
    project.getModel().setBuild(new Build());
    return project;
  }

  private static Plugin deploy(String skip) {
    Xpp3Dom configuration = new Xpp3Dom("configuration");
    Xpp3Dom flag = new Xpp3Dom("skip");
    flag.setValue(skip);
    configuration.addChild(flag);
    Plugin plugin = new Plugin();
    plugin.setGroupId("org.apache.maven.plugins");
    plugin.setArtifactId("maven-deploy-plugin");
    plugin.setConfiguration(configuration);
    return plugin;
  }
}
