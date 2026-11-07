package net.kurobako.bonsai.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class IdsTest {

  private static final Path BUILTIN =
    Path.of("..", "scalafix", "rules", "src", "main", "resources", "META-INF", "bonsai", "builtin.id");

  @TempDir
  Path root;

  private Path write(String name, String contents) throws IOException {
    Path file = root.resolve(name);
    Files.writeString(file, contents);
    return file;
  }

  @Test
  void readsTheIdsEachToolKnows() throws IOException {
    Ids ids = Ids.of(
      write("checks.id", "acyclic\nlocal-throw\n"),
      write("findings.id", "duplicate-code\nunused-member\n"),
      write("rules.id", "BannedSyntax\nOptionIdioms\n"), BUILTIN);

    assertEquals(Set.of("acyclic", "local-throw"), ids.checks());
    assertEquals(Set.of("duplicate-code", "unused-member"), ids.findings());
    // The syntax scalafix itself provides is known too, and the bans of its DisableSyntax rule.
    assertTrue(ids.rules().containsAll(Set.of("BannedSyntax", "OptionIdioms", "DisableSyntax", "RedundantSyntax")),
      ids.rules().toString());
    assertTrue(ids.rules().containsAll(Set.of("RemoveUnused", "ExplicitResultTypes")), ids.rules().toString());
    assertFalse(ids.rules().contains("DisableSyntax.noVars"), ids.rules().toString());
    assertTrue(ids.bans().contains("DisableSyntax.noVars"), ids.bans().toString());
    assertFalse(ids.settings().isEmpty());
    assertTrue(ids.unusedOptions().contains("-Wunused:all"), ids.unusedOptions().toString());
  }

  @Test
  void theBuiltInRulesAndTheirBansAreKnownToTheCheck() throws IOException {
    Ids ids = Ids.of(
      write("checks.id", "acyclic\n"),
      write("findings.id", "duplicate-code\n"),
      write("rules.id", "BannedSyntax\n"), BUILTIN);

    ids.validate(Set.of("RemoveUnused", "DisableSyntax.noXml"), Set.of("ExplicitResultTypes"), Set.of(), Set.of());
  }

  @Test
  void theOptInRulesAreTheOnesTheBuiltInFileMarks() throws IOException {
    Ids ids = Ids.of(
      write("checks.id", "acyclic\n"),
      write("findings.id", "duplicate-code\n"),
      write("rules.id", "BannedSyntax\n"), BUILTIN);

    assertEquals(Set.of("ExplicitResultTypes"), ids.optIn());
    assertTrue(ids.rules().contains("ExplicitResultTypes"), ids.rules().toString());
    assertFalse(ids.rules().contains("?ExplicitResultTypes"), ids.rules().toString());
  }

  @Test
  void anIdNoToolKnowsFailsTheCheck() throws IOException {
    Ids ids = Ids.of(
      write("checks.id", "acyclic\n"),
      write("findings.id", "duplicate-code\n"),
      write("rules.id", "BannedSyntax\n"), BUILTIN);

    IOException error = assertThrows(IOException.class,
      () -> ids.validate(Set.of("acyclic", "typo"), Set.of(), Set.of(), Set.of()));
    assertTrue(error.getMessage().contains("typo"), error.getMessage());
  }

  @Test
  void aRuleThatIsNotOptInCannotBeEnabled() throws IOException {
    Ids ids = Ids.of(
      write("checks.id", "acyclic\n"),
      write("findings.id", "duplicate-code\n"),
      write("rules.id", "BannedSyntax\n"), BUILTIN);

    IOException error = assertThrows(IOException.class,
      () -> ids.validate(Set.of(), Set.of("RemoveUnused", "acyclic"), Set.of(), Set.of()));
    assertTrue(error.getMessage().contains("RemoveUnused"), error.getMessage());
    assertTrue(error.getMessage().contains("ExplicitResultTypes"), error.getMessage());
  }

  @Test
  void anIdOfAnotherToolIsNotAnUnknownOne() throws IOException {
    Ids ids = Ids.of(
      write("checks.id", "acyclic\n"),
      write("findings.id", "duplicate-code\n"),
      write("rules.id", "BannedSyntax\n"), BUILTIN);

    ids.validate(Set.of("acyclic", "duplicate-code", "BannedSyntax"), Set.of(), Set.of(), Set.of());
  }
}
