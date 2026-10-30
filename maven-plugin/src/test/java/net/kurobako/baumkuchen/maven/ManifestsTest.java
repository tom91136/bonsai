package net.kurobako.baumkuchen.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManifestsTest {

  @TempDir
  Path root;

  private Path fragment(String name, String... lines) throws IOException {
    Path file = root.resolve(name);
    Files.writeString(file, String.join("\n", lines) + "\n");
    return file;
  }

  @Test
  void theManifestOfTheBuildIsTheFragmentsOfEveryModule() throws IOException {
    fragment("a.tsv", "root\t/build", "output\ta\tmain\t/a/classes", "classpath\t/a/library.jar");
    fragment("b.tsv", "root\t/build", "output\tb\tmain\t/b/classes", "classpath\t/b/library.jar");

    List<String> manifest = Analysis.merge(root, "/build");

    assertTrue(manifest.contains("output\ta\tmain\t/a/classes"), manifest.toString());
    assertTrue(manifest.contains("output\tb\tmain\t/b/classes"), manifest.toString());
    assertTrue(manifest.contains("classpath\t/a/library.jar"), manifest.toString());
    assertTrue(manifest.contains("classpath\t/b/library.jar"), manifest.toString());
  }

  @Test
  void theRootOfTheBuildIsNamedOnce() throws IOException {
    fragment("a.tsv", "root\t/build", "output\ta\tmain\t/a/classes");
    fragment("b.tsv", "root\t/build", "output\tb\tmain\t/b/classes");

    List<String> manifest = Analysis.merge(root, "/build");

    assertEquals(1, manifest.stream().filter(line -> line.startsWith("root\t")).count(), manifest.toString());
    assertTrue(manifest.contains("root\t/build"), manifest.toString());
  }

  @Test
  void theFragmentsAreReadInTheOrderOfTheirNames() throws IOException {
    fragment("b.tsv", "output\tb\tmain\t/b/classes");
    fragment("a.tsv", "output\ta\tmain\t/a/classes");

    List<String> manifest = Analysis.merge(root, "/build");

    assertEquals("output\ta\tmain\t/a/classes", manifest.get(0));
    assertEquals("output\tb\tmain\t/b/classes", manifest.get(1));
  }

  @Test
  void aBuildWithoutModulesHasAManifestThatNamesTheRoot() throws IOException {
    List<String> manifest = Analysis.merge(root, "/build");

    assertEquals(List.of("root\t/build"), manifest);
    assertFalse(manifest.isEmpty());
  }
}
