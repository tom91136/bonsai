package net.kurobako.bonsai.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ScalafixTest {

  @TempDir
  Path root;

  private Configuration configuration(String contents) throws IOException {
    Path file = root.resolve(".bonsai.conf");
    Files.writeString(file, contents);
    return Configuration.read(file, "demo");
  }

  private Ids ids() throws IOException {
    Path checks = root.resolve("checks.id");
    Path findings = root.resolve("findings.id");
    Path rules = root.resolve("rules.id");
    Files.writeString(checks, "acyclic\n");
    Files.writeString(findings, "duplicate-code\n");
    Files.writeString(rules, "BannedSyntax\nOptionIdioms\n");
    return Ids.of(checks, findings, rules);
  }

  @Test
  void writesTheRulesThatRun() throws IOException {
    Configuration configuration = configuration("disabled = [OptionIdioms]\n");

    String scalafix = Scalafix.configuration(configuration, ids());

    assertTrue(scalafix.contains("\"BannedSyntax\""), scalafix);
    assertTrue(!scalafix.contains("\"OptionIdioms\""), scalafix);
    assertTrue(scalafix.contains("\"DisableSyntax\""), scalafix);
    assertTrue(scalafix.contains("\"RedundantSyntax\""), scalafix);
  }

  @Test
  void theBansOfTheDisableSyntaxRuleAreOn() throws IOException {
    Configuration configuration = configuration("");

    String scalafix = Scalafix.configuration(configuration, ids());

    assertTrue(scalafix.contains("DisableSyntax.noVars = true"), scalafix);
    assertTrue(scalafix.contains("DisableSyntax.noFinalize = true"), scalafix);
  }

  @Test
  void theBansAreOffWhenTheRuleIsLeftOut() throws IOException {
    Configuration configuration = configuration("disabled = [DisableSyntax]\n");

    String scalafix = Scalafix.configuration(configuration, ids());

    assertTrue(!scalafix.contains("DisableSyntax.noVars = true"), scalafix);
    assertTrue(!scalafix.contains("\"DisableSyntax\""), scalafix);
  }

  @Test
  void aBanLeftOutOnItsOwnIsOff() throws IOException {
    Configuration configuration = configuration("disabled = [DisableSyntax.noVars]\n");

    String scalafix = Scalafix.configuration(configuration, ids());

    assertTrue(scalafix.contains("DisableSyntax.noVars = false"), scalafix);
    assertTrue(scalafix.contains("DisableSyntax.noReturns = true"), scalafix);
  }

  @Test
  void theModeIsTheOneTheEnvironmentNames() {
    assertEquals("ci", Mode.of(Map.of("BONSAI_MODE", "ci")));
    assertEquals("dev", Mode.of(Map.of("BONSAI_MODE", "DEV")));
    assertEquals("ci", Mode.of(Map.of("CI", "true")));
    assertEquals("dev", Mode.of(Map.of()));
  }

  @Test
  void aModeTheToolsDoNotKnowFails() {
    assertThrows(IllegalArgumentException.class, () -> Mode.of(Map.of("BONSAI_MODE", "strict")));
  }
}
