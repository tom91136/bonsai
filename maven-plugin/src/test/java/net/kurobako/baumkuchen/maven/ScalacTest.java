package net.kurobako.baumkuchen.maven;

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

  private Configuration configuration(String contents) throws IOException {
    Path file = root.resolve(".baumkuchen.conf");
    Files.writeString(file, contents);
    return Configuration.read(file, "demo");
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
    Configuration configuration = configuration("""
        disabled = [acyclic]
        errors = [broad-catch]
        warnings = [identity-token]
        wildcards = 3
        generated = ["**/generated/**"]
        hot = ["demo.kernel.**"]
        checked = [demo.where]
        """);

    List<String> options = Scalac.options(configuration, ids(), "dev");

    assertTrue(options.contains("-P:baumkuchen:mode=dev"), options.toString());
    assertTrue(options.contains("-P:baumkuchen:wildcards=3"), options.toString());
    assertTrue(options.contains("-P:baumkuchen:disable=acyclic"), options.toString());
    assertTrue(options.contains("-P:baumkuchen:error=broad-catch"), options.toString());
    assertTrue(options.contains("-P:baumkuchen:warn=identity-token"), options.toString());
    assertFalse(options.stream().anyMatch(option -> option.contains("generated")), options.toString());
    assertTrue(options.contains("-P:baumkuchen:hot=demo.kernel.**"), options.toString());
    assertTrue(options.contains("-P:baumkuchen:checked=demo.where"), options.toString());
  }

  @Test
  void semanticdbIsWrittenUnderTheOutputWithPathsRelativeToTheRoot() {
    File output = root.resolve("target/classes").toFile();

    assertEquals(
      List.of(
        "-Xsemanticdb",
        "-sourceroot",
        root.toFile().getAbsolutePath(),
        "-semanticdb-target",
        output.getAbsolutePath()
      ),
      Scalac.semanticdb(root.toFile(), output)
    );
  }

  @Test
  void aCheckOfAnotherToolIsNotPassedToTheCompilerPlugin() throws IOException {
    Configuration configuration = configuration("disabled = [duplicate-code, acyclic]\n");

    List<String> options = Scalac.options(configuration, ids(), "dev");

    assertTrue(options.contains("-P:baumkuchen:disable=acyclic"), options.toString());
    assertTrue(!options.contains("-P:baumkuchen:disable=duplicate-code"), options.toString());
  }

  @Test
  void theFindingsDoNotFailTheBuildWhileDeveloping() throws IOException {
    Configuration configuration = configuration("disabled = [acyclic]\n");

    List<String> options = Scalac.options(configuration, ids(), "dev");

    assertTrue(options.contains("-Wconf:msg=.*\\[baumkuchen.*\\]:i"), options.toString());
  }

  @Test
  void theFindingsFailTheBuildOnACiServer() throws IOException {
    Configuration configuration = configuration("disabled = [acyclic]\n");

    List<String> options = Scalac.options(configuration, ids(), "ci");

    assertTrue(options.contains("-Wconf:msg=.*\\[baumkuchen.*\\]:e"), options.toString());
  }

  @Test
  void aCheckKeptAsAWarningDoesNotFailTheBuildOnACiServer() throws IOException {
    Configuration configuration = configuration("warnings = [acyclic]\n");

    List<String> options = Scalac.options(configuration, ids(), "ci");

    assertTrue(options.contains("-Wconf:msg=.*\\[baumkuchen[^]]*acyclic\\]:i"), options.toString());
  }
}
