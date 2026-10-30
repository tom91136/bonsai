package net.kurobako.baumkuchen.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AnalysisTest {

  @TempDir
  Path root;

  private Configuration configuration(String contents) throws IOException {
    Path file = root.resolve(".baumkuchen.conf");
    Files.writeString(file, contents);
    return Configuration.read(file, "demo");
  }

  private Ids ids() throws IOException {
    Path checks = root.resolve("checks.id");
    Path findings = root.resolve("findings.id");
    Path rules = root.resolve("rules.id");
    Files.writeString(checks, "acyclic\n");
    Files.writeString(findings, "duplicate-code\nunused-member\n");
    Files.writeString(rules, "BannedSyntax\n");
    return Ids.of(checks, findings, rules);
  }

  @Test
  void writesTheOptionsTheAnalysisReads() throws IOException {
    Configuration configuration = configuration("""
        disabled = [duplicate-code]
        errors = [unused-member]
        warnings = [acyclic]
        effects { "demo.io" = [input, output] }
        concurrent = [demo.WorkerPool.run]
        generated = ["**/generated/**"]
        reflective = ["demo.rendered.*"]
        hot = ["demo.kernel.**"]
        """);

    List<String> options = Analysis.options(configuration, ids(), "dev", root, null);

    assertTrue(options.contains("--mode"), options.toString());
    assertTrue(options.contains("dev"), options.toString());
    assertTrue(options.contains("--disable"), options.toString());
    assertTrue(options.contains("duplicate-code"), options.toString());
    assertTrue(options.contains("--error"), options.toString());
    assertTrue(options.contains("unused-member"), options.toString());
    assertTrue(options.contains("--effects"), options.toString());
    assertTrue(options.contains("demo.io=input,output"), options.toString());
    assertTrue(options.contains("--concurrent"), options.toString());
    assertTrue(options.contains("demo.WorkerPool.run"), options.toString());
    assertTrue(options.contains("--generated"), options.toString());
    assertTrue(options.contains("**/generated/**"), options.toString());
    assertTrue(options.contains("--reflective"), options.toString());
    assertTrue(options.contains("--hot"), options.toString());
  }

  @Test
  void passesTheLimitsOfTheNestingRuleItNames() throws IOException {
    List<String> options = Analysis.options(
      configuration("nesting { depth = 3, size = 120 }\n"), ids(), "dev", root, null);

    assertTrue(options.contains("--nesting"), options.toString());
    int depth = options.indexOf("--nesting-depth");
    assertEquals("3", options.get(depth + 1), options.toString());
    int size = options.indexOf("--nesting-size");
    assertEquals("120", options.get(size + 1), options.toString());
    assertTrue(!options.contains("--nesting-damping"), options.toString());
  }

  @Test
  void recordsTheFindingsInTheFileTheEnvironmentNames() throws IOException {
    Configuration configuration = configuration("");

    List<String> options = Analysis.options(configuration, ids(), "dev", root, "recorded-baseline.tsv");

    assertTrue(options.contains("--write-baseline"), options.toString());
    assertTrue(options.contains(root.resolve("recorded-baseline.tsv").toString()), options.toString());
  }

  @Test
  void aRuleOfAnotherToolIsNotPassedToTheAnalysis() throws IOException {
    Configuration configuration = configuration("disabled = [acyclic, duplicate-code]\n");

    List<String> options = Analysis.options(configuration, ids(), "dev", root, null);

    assertTrue(options.contains("duplicate-code"), options.toString());
    assertTrue(!options.contains("acyclic"), options.toString());
  }

  @Test
  void writesTheManifestOfTheBuild() throws IOException {
    Path classes = root.resolve("target/classes");
    Path testClasses = root.resolve("target/test-classes");
    Path model = root.resolve("target/baumkuchen/model/main.tsv");
    Path testModel = root.resolve("target/baumkuchen/model/test.tsv");
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
      List.of(),
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
    assertTrue(manifest.contains("classpath\t/repo/library.jar"), manifest.toString());
  }

  @Test
  void aModuleOtherBuildsUseIsNotReportedAsUnused() throws IOException {
    List<String> manifest = Analysis.manifest(
      root,
      List.of(new Module("demo", root.resolve("classes"), root.resolve("test-classes"),
        null, null, List.of(), List.of(), List.of(), List.of(), false)),
      List.of(),
      List.of());

    assertTrue(!manifest.contains("library\tdemo"), manifest.toString());
  }

  @Test
  void aModuleWithoutAModelHasNoModelLine() throws IOException {
    List<String> manifest = Analysis.manifest(
      root,
      List.of(new Module("demo", root.resolve("classes"), null, null, null,
        List.of(), List.of(), List.of(), List.of(), false)),
      List.of(),
      List.of());

    assertTrue(!manifest.contains("model\tdemo\tmain\t" + root.resolve("classes")), manifest.toString());
    assertTrue(manifest.stream().noneMatch(line -> line.startsWith("model\t")), manifest.toString());
  }
}
