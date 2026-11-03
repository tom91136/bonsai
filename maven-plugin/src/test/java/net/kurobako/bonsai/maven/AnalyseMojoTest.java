package net.kurobako.bonsai.maven;

import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AnalyseMojoTest {

  @Test
  void aModuleBindingTheAnalysisGoalAnalyses() {
    assertTrue(AnalyseMojo.analyses(project(plugin("bonsai-maven-plugin", "analyse", "analyse"))));
  }

  @Test
  void aModuleDeclaringThePluginForOtherGoalsOnlyDoesNotAnalyse() {
    assertFalse(AnalyseMojo.analyses(project(plugin("bonsai-maven-plugin", "prepare", "prepare"))));
  }

  @Test
  void aModuleBindingTheAnalysisGoalOfAnotherPluginDoesNotAnalyse() {
    assertFalse(AnalyseMojo.analyses(project(plugin("maven-jar-plugin", "analyse", "analyse"))));
  }

  private static MavenProject project(Plugin... plugins) {
    Build build = new Build();
    for (Plugin plugin : plugins) {
      build.addPlugin(plugin);
    }
    MavenProject project = new MavenProject();
    project.getModel().setBuild(build);
    return project;
  }

  private static Plugin plugin(String artifactId, String execution, String goal) {
    PluginExecution binding = new PluginExecution();
    binding.setId(execution);
    binding.addGoal(goal);
    Plugin plugin = new Plugin();
    plugin.setArtifactId(artifactId);
    plugin.addExecution(binding);
    return plugin;
  }
}
