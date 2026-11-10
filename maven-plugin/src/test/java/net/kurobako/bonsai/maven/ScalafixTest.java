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

  private static final Path BUILTIN =
    Path.of("..", "scalafix", "rules", "src", "main", "resources", "META-INF", "bonsai", "builtin.id");

  @TempDir
  Path root;

  private Configuration configuration(String... lines) throws IOException {
    return Configuration.parse(List.of(lines));
  }

  private Ids ids() throws IOException {
    return ids(BUILTIN);
  }

  private Ids ids(Path builtin) throws IOException {
    Path checks = root.resolve("checks.id");
    Path findings = root.resolve("findings.id");
    Path rules = root.resolve("rules.id");
    Files.writeString(checks, "acyclic\n");
    Files.writeString(findings, "duplicate-code\n");
    Files.writeString(rules, "BannedSyntax\nOptionIdioms\n");
    return Ids.of(checks, findings, rules, builtin);
  }

  private static final Versions.Support SUPPORT = new Versions.Support("3.3.4", List.of("3.4"));
  private static final String SCALA = "3.9.0";

  @Test
  void aNonfatalBanWarnsUnderTheLintsScalafixReports() throws IOException {
    Configuration configuration = configuration("nonfatal\tDisableSyntax.var");

    String scalafix = Scalafix.configuration(configuration, ids(), SUPPORT, SCALA);

    assertTrue(scalafix.contains("\"DisableSyntax.var\""), scalafix);
    assertTrue(!scalafix.contains("\"DisableSyntax.noVars\""), scalafix);
    assertTrue(!scalafix.contains("\"DisableSyntax.null\""), scalafix);
  }

  @Test
  void eitherLintOfABanNamesTheWholeBan() throws IOException {
    Path builtin = root.resolve("builtin.id");
    Files.writeString(builtin, "DisableSyntax\nDisableSyntax.noVars|var,variable\nDisableSyntax.noNulls|null\n");
    Ids ids = ids(builtin);

    String nonfatal = Scalafix.configuration(configuration("nonfatal\tDisableSyntax.variable"), ids, SUPPORT, SCALA);
    String disabled = Scalafix.configuration(configuration("disabled\tDisableSyntax.variable"), ids, SUPPORT, SCALA);

    assertTrue(nonfatal.contains("\"DisableSyntax.var\""), nonfatal);
    assertTrue(disabled.contains("DisableSyntax.noVars = false"), disabled);
    assertTrue(disabled.contains("DisableSyntax.noNulls = true"), disabled);
  }

  @Test
  void writesTheRulesThatRun() throws IOException {
    Configuration configuration = configuration("disabled\tOptionIdioms");

    String scalafix = Scalafix.configuration(configuration, ids(), SUPPORT, SCALA);

    assertTrue(scalafix.contains("\"BannedSyntax\""), scalafix);
    assertTrue(!scalafix.contains("\"OptionIdioms\""), scalafix);
    assertTrue(scalafix.contains("\"DisableSyntax\""), scalafix);
    assertTrue(scalafix.contains("\"RedundantSyntax\""), scalafix);
  }

  @Test
  void unusedCodeIsCheckedByDefaultAndInferredTypesAreNot() throws IOException {
    String scalafix = Scalafix.configuration(configuration(), ids(), SUPPORT, SCALA);

    assertTrue(scalafix.contains("\"RemoveUnused\""), scalafix);
    assertTrue(!scalafix.contains("\"ExplicitResultTypes\""), scalafix);
  }

  @Test
  void unusedCodeCanBeLeftOut() throws IOException {
    String scalafix = Scalafix.configuration(configuration("disabled\tRemoveUnused"), ids(), SUPPORT, SCALA);

    assertTrue(!scalafix.contains("\"RemoveUnused\""), scalafix);
  }

  @Test
  void inferredTypesAreCheckedWhenEnabled() throws IOException {
    String scalafix = Scalafix.configuration(configuration("enabled\tExplicitResultTypes"), ids(), SUPPORT, SCALA);

    assertTrue(scalafix.contains("\"ExplicitResultTypes\""), scalafix);
    assertTrue(scalafix.contains("ExplicitResultTypes.fetchScala3CompilerArtifactsOnVersionMismatch = true"), scalafix);
  }

  @Test
  void disabledWinsOverEnabled() throws IOException {
    String scalafix = Scalafix.configuration(
      configuration("enabled\tExplicitResultTypes", "disabled\tExplicitResultTypes"), ids(), SUPPORT, SCALA);

    assertTrue(!scalafix.contains("\"ExplicitResultTypes\""), scalafix);
  }

  @Test
  void theResultTypesRuleFetchesTheCompilerOfAMismatchedLine() throws IOException {
    String scalafix = Scalafix.configuration(configuration(), ids(), SUPPORT, SCALA);

    assertTrue(scalafix.contains("ExplicitResultTypes.fetchScala3CompilerArtifactsOnVersionMismatch = true"), scalafix);
  }

  @Test
  void aLineScalafixCannotLoadACompilerForKeepsTheOtherRules() throws IOException {
    String scalafix = Scalafix.configuration(configuration("enabled\tExplicitResultTypes"), ids(), SUPPORT, "3.4.3");

    assertTrue(!scalafix.contains("\"ExplicitResultTypes\""), scalafix);
    assertTrue(scalafix.contains("\"RemoveUnused\""), scalafix);
    assertTrue(scalafix.contains("\"BannedSyntax\""), scalafix);
  }

  @Test
  void aReleaseBeforeTheUnusedWarningsLeavesUnusedCodeOut() throws IOException {
    String scalafix = Scalafix.configuration(configuration("enabled\tExplicitResultTypes"), ids(), SUPPORT, "3.3.3");

    assertTrue(!scalafix.contains("\"RemoveUnused\""), scalafix);
    assertTrue(scalafix.contains("\"ExplicitResultTypes\""), scalafix);
  }

  @Test
  void theBansOfTheDisableSyntaxRuleAreOn() throws IOException {
    Configuration configuration = configuration();

    String scalafix = Scalafix.configuration(configuration, ids(), SUPPORT, SCALA);

    assertTrue(scalafix.contains("DisableSyntax.noVars = true"), scalafix);
    assertTrue(scalafix.contains("DisableSyntax.noFinalize = true"), scalafix);
    assertTrue(!scalafix.contains("noAsInstanceOf"), scalafix);
    assertTrue(scalafix.contains("DisableSyntax.noIsInstanceOf = true"), scalafix);
  }

  @Test
  void theBansAreOffWhenTheRuleIsLeftOut() throws IOException {
    Configuration configuration = configuration("disabled\tDisableSyntax");

    String scalafix = Scalafix.configuration(configuration, ids(), SUPPORT, SCALA);

    assertTrue(!scalafix.contains("DisableSyntax.noVars = true"), scalafix);
    assertTrue(!scalafix.contains("\"DisableSyntax\""), scalafix);
  }

  @Test
  void aBanLeftOutOnItsOwnIsOff() throws IOException {
    Configuration configuration = configuration("disabled\tDisableSyntax.var");

    String scalafix = Scalafix.configuration(configuration, ids(), SUPPORT, SCALA);

    assertTrue(scalafix.contains("DisableSyntax.noVars = false"), scalafix);
    assertTrue(scalafix.contains("DisableSyntax.noReturns = true"), scalafix);
  }

  @Test
  void theModeIsTheOneTheEnvironmentNames() {
    assertEquals("strict", Mode.of(Map.of("BONSAI_MODE", "strict")));
    assertEquals("relaxed", Mode.of(Map.of("BONSAI_MODE", "RELAXED")));
    assertEquals("audit", Mode.of(Map.of("BONSAI_MODE", "Audit")));
    assertEquals("audit", Mode.of(Map.of("BONSAI_MODE", "audit", "CI", "true")));
    assertEquals("strict", Mode.of(Map.of("CI", "true")));
    assertEquals("relaxed", Mode.of(Map.of()));
  }

  @Test
  void aModeTheToolsDoNotKnowFails() {
    assertThrows(IllegalArgumentException.class, () -> Mode.of(Map.of("BONSAI_MODE", "dev")));
  }
}
