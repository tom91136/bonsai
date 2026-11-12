package net.kurobako.bonsai.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManifestsTest {

  private static final List<String> MODULES = List.of("a", "b");

  @TempDir
  Path root;

  private Path fragment(String name, String... lines) throws IOException {
    Path file = root.resolve(name);
    Files.writeString(file, String.join("\n", lines) + "\n");
    return file;
  }

  @Test
  void theManifestOfTheBuildIsTheFragmentsOfEveryModule() throws IOException {
    fragment("a.tsv", "root\t/build", "output\ta\tmain\t/a/classes", "classpath\ta\tmain\t/a/library.jar");
    fragment("b.tsv", "root\t/build", "output\tb\tmain\t/b/classes", "classpath\tb\tmain\t/b/library.jar");

    List<String> manifest = Analysis.merge(root, MODULES, "/build");

    assertTrue(manifest.contains("output\ta\tmain\t/a/classes"), manifest.toString());
    assertTrue(manifest.contains("output\tb\tmain\t/b/classes"), manifest.toString());
    assertTrue(manifest.contains("classpath\ta\tmain\t/a/library.jar"), manifest.toString());
    assertTrue(manifest.contains("classpath\tb\tmain\t/b/library.jar"), manifest.toString());
  }

  @Test
  void theRootOfTheBuildIsNamedOnce() throws IOException {
    fragment("a.tsv", "root\t/build", "output\ta\tmain\t/a/classes");
    fragment("b.tsv", "root\t/build", "output\tb\tmain\t/b/classes");

    List<String> manifest = Analysis.merge(root, MODULES, "/build");

    assertEquals(1, manifest.stream().filter(line -> line.startsWith("root\t")).count(), manifest.toString());
    assertTrue(manifest.contains("root\t/build"), manifest.toString());
  }

  @Test
  void theFragmentsAreReadInTheOrderOfTheirNames() throws IOException {
    fragment("b.tsv", "output\tb\tmain\t/b/classes");
    fragment("a.tsv", "output\ta\tmain\t/a/classes");

    List<String> manifest = Analysis.merge(root, List.of("b", "a"), "/build");

    assertEquals("output\ta\tmain\t/a/classes", manifest.get(0));
    assertEquals("output\tb\tmain\t/b/classes", manifest.get(1));
  }

  @Test
  void aBuildWithoutModulesHasAManifestThatNamesTheRoot() throws IOException {
    List<String> manifest = Analysis.merge(root, List.of(), "/build");

    assertEquals(List.of("root\t/build"), manifest);
    assertFalse(manifest.isEmpty());
  }

  @Test
  void aFragmentOfAModuleThatIsNotInTheBuildIsNotRead() throws IOException {
    fragment("a.tsv", "output\ta\tmain\t/a/classes");
    fragment("removed.tsv", "output\tremoved\tmain\t/removed/classes");

    List<String> manifest = Analysis.merge(root, List.of("a"), "/build");

    assertEquals(List.of("output\ta\tmain\t/a/classes", "root\t/build"), manifest);
  }

  @Test
  void theManifestIsTheSameWhicheverModuleIsListedFirst() throws IOException {
    fragment("a.tsv", "output\ta\tmain\t/a/classes");
    fragment("a-b.tsv", "output\ta-b\tmain\t/a-b/classes");
    fragment("b.tsv", "output\tb\tmain\t/b/classes");

    assertEquals(
      Analysis.merge(root, List.of("a", "a-b", "b"), "/build"),
      Analysis.merge(root, List.of("b", "a-b", "a"), "/build"));
  }

  @Test
  void aModuleWithoutAFragmentFailsTheMerge() throws IOException {
    fragment("a.tsv", "output\ta\tmain\t/a/classes");

    assertThrows(IOException.class, () -> Analysis.merge(root, MODULES, "/build"));
  }
}
