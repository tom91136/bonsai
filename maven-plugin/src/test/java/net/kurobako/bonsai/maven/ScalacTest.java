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
    return Ids.of(checks, findings, rules);
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

    assertTrue(options.contains("-Wconf:msg=.*\\[bonsai.*\\]:i"), options.toString());
  }

  @Test
  void theFindingsFailTheBuildOnACiServer() throws IOException {
    Configuration configuration = configuration("disabled\tacyclic");

    List<String> options = Scalac.options(configuration, ids(), "ci");

    assertTrue(options.contains("-Wconf:msg=.*\\[bonsai.*\\]:e"), options.toString());
  }

  @Test
  void aCheckKeptAsAWarningDoesNotFailTheBuildOnACiServer() throws IOException {
    Configuration configuration = configuration("warnings\tacyclic");

    List<String> options = Scalac.options(configuration, ids(), "ci");

    assertTrue(options.contains("-Wconf:msg=.*\\[bonsai[^]]*acyclic\\]:i"), options.toString());
  }
}
