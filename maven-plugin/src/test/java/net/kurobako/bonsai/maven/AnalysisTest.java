package net.kurobako.bonsai.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AnalysisTest {

  @TempDir
  Path root;

  @Test
  void writesTheManifestOfTheBuild() throws IOException {
    Path classes = root.resolve("target/classes");
    Path testClasses = root.resolve("target/test-classes");
    Path model = root.resolve("target/bonsai/model/main.tsv");
    Path testModel = root.resolve("target/bonsai/model/test.tsv");
    Path source = root.resolve("src/main/scala/demo/Main.scala");
    Path testSource = root.resolve("src/test/scala/demo/MainSpec.scala");
    Files.createDirectories(classes);
    Files.createDirectories(testClasses);
    Files.createDirectories(source.getParent());
    Files.createDirectories(testSource.getParent());
    Files.writeString(source, "object Main");
    Files.writeString(testSource, "object MainSpec");

    List<String> manifest = Analysis.manifest(
      root,
      List.of(new Module("demo", classes, testClasses, model, testModel, List.of(source), List.of(testSource),
        List.of("/repo/library.jar"), List.of("-Werror"), true)),
      List.of());

    assertTrue(manifest.contains("root\t" + root), manifest.toString());
    assertTrue(manifest.contains("output\tdemo\tmain\t" + classes), manifest.toString());
    assertTrue(manifest.contains("output\tdemo\ttest\t" + testClasses), manifest.toString());
    assertTrue(manifest.contains("model\tdemo\tmain\t" + model), manifest.toString());
    assertTrue(manifest.contains("model\tdemo\ttest\t" + testModel), manifest.toString());
    assertTrue(manifest.contains("library\tdemo"), manifest.toString());
    assertTrue(manifest.contains("source\tdemo\tmain\t" + source), manifest.toString());
    assertTrue(manifest.contains("source\tdemo\ttest\t" + testSource), manifest.toString());
    assertTrue(manifest.contains("option\tdemo\t-Werror"), manifest.toString());
    assertTrue(manifest.contains("classpath\tdemo\t/repo/library.jar"), manifest.toString());
  }

  @Test
  void aModuleOtherBuildsUseIsNotReportedAsUnused() throws IOException {
    List<String> manifest = Analysis.manifest(
      root,
      List.of(new Module("demo", root.resolve("classes"), root.resolve("test-classes"),
        null, null, List.of(), List.of(), List.of(), List.of(), false)),
      List.of());

    assertTrue(!manifest.contains("library\tdemo"), manifest.toString());
  }

  @Test
  void aModuleWithoutAModelHasNoModelLine() throws IOException {
    List<String> manifest = Analysis.manifest(
      root,
      List.of(new Module("demo", root.resolve("classes"), null, null, null,
        List.of(), List.of(), List.of(), List.of(), false)),
      List.of());

    assertTrue(!manifest.contains("model\tdemo\tmain\t" + root.resolve("classes")), manifest.toString());
    assertTrue(manifest.stream().noneMatch(line -> line.startsWith("model\t")), manifest.toString());
  }
}
