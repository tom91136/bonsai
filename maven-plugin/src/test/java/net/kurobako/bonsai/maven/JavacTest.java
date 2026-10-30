package net.kurobako.bonsai.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

class JavacTest {

  @TempDir
  Path root;

  @Test
  void theModelOfEachScopeHasItsOwnFile() {
    Path main = Javac.model(root, "main");
    Path test = Javac.model(root, "test");

    assertEquals(root.resolve("model/main.tsv"), main);
    assertEquals(root.resolve("model/test.tsv"), test);
    assertNotEquals(main, test);
  }

  @Test
  void theProcessorPathHoldsThePluginAndTheLibraryItRunsOn() {
    String path = Javac.processorPath(List.of(new File("/repo/plugin.jar"), new File("/repo/library.jar")));

    assertEquals("/repo/plugin.jar" + File.pathSeparator + "/repo/library.jar", path);
  }

  @Test
  void theCompilerTakesTheModelFileFromTheProperty() {
    assertEquals("bonsai.model", Javac.MODEL);
  }

  @Test
  void theCompilerTakesTheProcessorPathFromTheProperty() {
    assertEquals("bonsai.java.plugin", Javac.PLUGIN);
  }

  @Test
  void thePluginIsPublishedAsAnArtifactOfItsOwn() {
    assertEquals("bonsai-java-plugin_3", Javac.ARTIFACT);
  }
}
