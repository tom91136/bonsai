package net.kurobako.baumkuchen.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class IdsTest {

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
      write("rules.id", "BannedSyntax\nOptionIdioms\n"));

    assertEquals(Set.of("acyclic", "local-throw"), ids.checks());
    assertEquals(Set.of("duplicate-code", "unused-member"), ids.findings());
    // The syntax scalafix itself provides is known too, and the bans of its DisableSyntax rule.
    assertTrue(ids.rules().containsAll(Set.of("BannedSyntax", "OptionIdioms", "DisableSyntax", "RedundantSyntax")),
      ids.rules().toString());
  }

  @Test
  void anIdNoToolKnowsFailsTheCheck() throws IOException {
    Ids ids = Ids.of(
      write("checks.id", "acyclic\n"),
      write("findings.id", "duplicate-code\n"),
      write("rules.id", "BannedSyntax\n"));

    IOException error = assertThrows(IOException.class,
      () -> ids.validate(Set.of("acyclic", "typo"), Set.of(), Set.of()));
    assertTrue(error.getMessage().contains("typo"), error.getMessage());
  }

  @Test
  void anIdOfAnotherToolIsNotAnUnknownOne() throws IOException {
    Ids ids = Ids.of(
      write("checks.id", "acyclic\n"),
      write("findings.id", "duplicate-code\n"),
      write("rules.id", "BannedSyntax\n"));

    ids.validate(Set.of("acyclic", "duplicate-code", "BannedSyntax"), Set.of(), Set.of());
  }
}
