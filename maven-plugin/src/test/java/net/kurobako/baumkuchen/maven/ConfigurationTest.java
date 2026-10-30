package net.kurobako.baumkuchen.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ConfigurationTest {

  @TempDir
  Path root;

  private Path write(String contents) throws IOException {
    Path file = root.resolve(".baumkuchen.conf");
    Files.writeString(file, contents);
    return file;
  }

  @Test
  void readsTheSettingsOfTheBuild() throws IOException {
    Path file = write("""
        disabled = [acyclic, local-throw]
        errors = [broad-catch]
        warnings = [identity-token]
        wildcards = 3
        baseline = "findings.tsv"
        purity = "pure.tsv"
        effects { "demo.io" = [input, output] }
        concurrent = [demo.WorkerPool.run]
        generated = ["**/target/generated/**"]
        hot = ["demo.kernel.**"]
        reflective = ["demo.rendered.*"]
        checked = [demo.where]
        """);

    Configuration configuration = Configuration.read(file, "demo");

    assertEquals(Set.of("acyclic", "local-throw"), configuration.disabled());
    assertEquals(Set.of("broad-catch"), configuration.errors());
    assertEquals(Set.of("identity-token"), configuration.warnings());
    assertEquals(3, configuration.wildcards());
    assertEquals(root.resolve("findings.tsv"), configuration.baseline());
    assertEquals(root.resolve("pure.tsv"), configuration.purity());
    assertEquals(Set.of("demo.io"), configuration.effects().keySet());
    assertEquals(Set.of("input", "output"), configuration.effects().get("demo.io"));
    assertEquals(Set.of("demo.WorkerPool.run"), configuration.concurrent());
    assertEquals(Set.of("**/target/generated/**"), configuration.generated());
    assertEquals(Set.of("demo.kernel.**"), configuration.hot());
    assertEquals(Set.of("demo.rendered.*"), configuration.reflective());
    assertEquals(Set.of("demo.where"), configuration.checked());
    assertTrue(!configuration.library());
  }

  @Test
  void readsTheBoundedMembersInTheOrderTheyAreNamed() throws IOException {
    Path file = write("""
        bounded = [
          { member = "demo.Encoding.operands", max = 4, reason = "an instruction has at most four operands" },
          { member = "demo.Encoding.modifiers", max = 2, reason = "at most two modifiers" }
        ]
        """);

    assertEquals(
      List.of(
        new Configuration.Bounded("demo.Encoding.operands", 4, "an instruction has at most four operands"),
        new Configuration.Bounded("demo.Encoding.modifiers", 2, "at most two modifiers")
      ),
      Configuration.read(file, "demo").bounded()
    );
  }

  @Test
  void aProjectsOwnSettingsOverrideTheBuilds() throws IOException {
    Path file = write("""
        disabled = [acyclic]
        projects.demo {
          disabled = [local-throw]
          library = true
        }
        """);

    Configuration configuration = Configuration.read(file, "demo");

    assertEquals(Set.of("local-throw"), configuration.disabled());
    assertTrue(configuration.library());
  }

  @Test
  void aProjectWithoutSettingsTakesTheBuilds() throws IOException {
    Path file = write("""
        disabled = [acyclic]
        projects.other {
          disabled = [local-throw]
        }
        """);

    assertEquals(Set.of("acyclic"), Configuration.read(file, "demo").disabled());
  }

  @Test
  void aKeyNoSettingReadsFailsTheRead() throws IOException {
    Path file = write("unknown = 1\n");

    IOException error = assertThrows(IOException.class, () -> Configuration.read(file, "demo"));
    assertTrue(error.getMessage().contains("unknown"), error.getMessage());
  }

  @Test
  void anEffectKindNoPolicyKnowsFailsTheRead() throws IOException {
    Path file = write("effects { \"demo.io\" = [sound] }\n");

    IOException error = assertThrows(IOException.class, () -> Configuration.read(file, "demo"));
    assertTrue(error.getMessage().contains("sound"), error.getMessage());
  }

  @Test
  void aConfigurationThatDoesNotExistIsAnEmptyOne() throws IOException {
    Configuration configuration = Configuration.read(root.resolve("absent.conf"), "demo");

    assertTrue(configuration.disabled().isEmpty());
    assertEquals(root.resolve("baumkuchen-baseline.tsv"), configuration.baseline());
  }
}
