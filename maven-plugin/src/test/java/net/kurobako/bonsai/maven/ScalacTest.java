package net.kurobako.bonsai.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ScalacTest {

  private static final Path BUILTIN =
    Path.of("..", "scalafix", "rules", "src", "main", "resources", "META-INF", "bonsai", "builtin.id");
  private static final Versions.Support SUPPORT = new Versions.Support("3.3.4", List.of("3.4"));

  @TempDir
  Path root;

  private Configuration configuration(String... lines) throws IOException {
    return Configuration.parse(List.of(lines));
  }

  private Ids ids() throws IOException {
    Path checks = root.resolve("checks.id");
    Path findings = root.resolve("findings.id");
    Path rules = root.resolve("rules.id");
    Files.writeString(checks, "acyclic\nlocal-throw\nbroad-catch\nidentity-token\n");
    Files.writeString(findings, "duplicate-code\n");
    Files.writeString(rules, "BannedSyntax\n");
    return Ids.of(checks, findings, rules, BUILTIN);
  }

  @Test
  void writesTheOptionsTheCompilerPluginReads() throws IOException {
    Configuration configuration = configuration(
      "disabled\tacyclic", "errors\tbroad-catch", "warnings\tidentity-token", "wildcards\t3",
      "hot\tdemo.kernel.**", "bounded\tdemo.Encoding.operands:4", "checked\tdemo.where");

    List<String> options = Scalac.options(configuration, ids(), "dev");

    assertTrue(options.contains("-P:bonsai:mode=dev"), options.toString());
    assertTrue(options.contains("-P:bonsai:wildcards=3"), options.toString());
    assertTrue(options.contains("-P:bonsai:disable=acyclic"), options.toString());
    assertTrue(options.contains("-P:bonsai:error=broad-catch"), options.toString());
    assertTrue(options.contains("-P:bonsai:warn=identity-token"), options.toString());
    assertFalse(options.stream().anyMatch(option -> option.contains("generated")), options.toString());
    assertTrue(options.contains("-P:bonsai:hot=demo.kernel.**"), options.toString());
    assertTrue(options.contains("-P:bonsai:bounded=demo.Encoding.operands:4"), options.toString());
    assertTrue(options.contains("-P:bonsai:checked=demo.where"), options.toString());
  }

  @Test
  void theCompilerIsAskedForTheUnusedWarningsTheRuleReads() throws IOException {
    List<String> options = Scalac.unused(configuration(), ids(), SUPPORT, "3.9.0", List.of("-Werror"));

    assertTrue(options.contains("-Wunused:all"), options.toString());
    assertTrue(options.stream().anyMatch(option -> option.startsWith("-Wconf:msg=^unused")), options.toString());
  }

  @Test
  void noUnusedWarningsAreAskedForWhenTheRuleIsLeftOut() throws IOException {
    List<String> options = Scalac.unused(configuration("disabled\tRemoveUnused"), ids(), SUPPORT, "3.9.0", List.of());

    assertTrue(options.isEmpty(), options.toString());
  }

  @Test
  void noUnusedWarningsAreAskedForOfACompilerThatLacksThem() throws IOException {
    assertTrue(Scalac.unused(configuration(), ids(), SUPPORT, "3.3.3", List.of()).isEmpty());
  }

  @Test
  void aBuildThatAsksForUnusedWarningsItselfKeepsItsOwn() throws IOException {
    for (String own : List.of("-Wunused:imports", "-Wunused:all", "-Wunused", "-Wall", "-Ywarn-unused")) {
      List<String> options = Scalac.unused(configuration(), ids(), SUPPORT, "3.9.0", List.of("-Werror", own));

      assertTrue(options.isEmpty(), own + " " + options);
    }
  }

  @Test
  void theRulesAreHandedTheUnusedOptionsTheCompilerIsRunWith() throws IOException {
    List<String> own = Scalac.ownUnused(java.util.Arrays.asList("-Werror", null, "@args.txt", "-Wunused:imports"));
    List<String> added = Scalac.unused(configuration(), ids(), SUPPORT, "3.9.0", List.of("-Werror"));

    assertEquals(List.of("-Wunused:imports"), own);
    assertTrue(added.contains("-Wunused:all"), added.toString());
    assertTrue(Scalac.ownUnused(List.of("-Werror")).isEmpty());
  }

  @Test
  void theOptionFoldIsPassedOnlyWhenTheConfigurationNamesIt() throws IOException {
    List<String> named = Scalac.options(configuration("option-fold\tmap-get-or-else"), ids(), "dev");
    List<String> unnamed = Scalac.options(configuration("wildcards\t3"), ids(), "dev");

    assertTrue(named.contains("-P:bonsai:option-fold=map-get-or-else"), named.toString());
    assertFalse(unnamed.stream().anyMatch(option -> option.contains("option-fold")), unnamed.toString());
  }

  @Test
  void semanticdbIsWrittenUnderTheOutput() {
    File output = root.resolve("target/classes").toFile();

    assertEquals(List.of("-Xsemanticdb", "-semanticdb-target", output.getAbsolutePath()), Scalac.semanticdb(output));
  }

  @Test
  void sourcesArePathedFromTheRootOfTheBuild() {
    assertEquals(List.of("-sourceroot", root.toFile().getAbsolutePath()), Scalac.sourceroot(root.toFile()));
  }

  @Test
  void aCheckOfAnotherToolIsNotPassedToTheCompilerPlugin() throws IOException {
    Configuration configuration = configuration("disabled\tduplicate-code", "disabled\tacyclic");

    List<String> options = Scalac.options(configuration, ids(), "dev");

    assertTrue(options.contains("-P:bonsai:disable=acyclic"), options.toString());
    assertTrue(!options.contains("-P:bonsai:disable=duplicate-code"), options.toString());
  }

  @Test
  void theFindingsDoNotFailTheBuildWhileDeveloping() throws IOException {
    Configuration configuration = configuration("disabled\tacyclic");

    List<String> options = Scalac.options(configuration, ids(), "dev");

    assertTrue(options.contains("-Wconf:msg=\\[bonsai.[a-z0-9-]+\\]:i"), options.toString());
  }

  @Test
  void theFindingsFailTheBuildOnACiServer() throws IOException {
    Configuration configuration = configuration("disabled\tacyclic");

    List<String> options = Scalac.options(configuration, ids(), "ci");

    assertTrue(options.contains("-Wconf:msg=\\[bonsai.[a-z0-9-]+\\]:e"), options.toString());
  }

  @Test
  void aCheckKeptAsAWarningDoesNotFailTheBuildOnACiServer() throws IOException {
    Configuration configuration = configuration("warnings\tacyclic");

    List<String> options = Scalac.options(configuration, ids(), "ci");

    String failing = "-Wconf:msg=\\[bonsai.[a-z0-9-]+\\]:e";
    String kept = "-Wconf:msg=\\[bonsai.acyclic\\]:i";
    assertTrue(options.contains(kept), options.toString());
    assertTrue(options.indexOf(failing) < options.indexOf(kept), options.toString());
  }
}
