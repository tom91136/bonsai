package net.kurobako.bonsai.maven;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ConfigurationTest {

  @Test
  void readsTheEntriesOfTheReader() throws IOException {
    Configuration configuration = Configuration.parse(List.of(
      "disabled\tlocal-throw",
      "disabled\tacyclic",
      "errors\tbroad-catch",
      "warnings\tidentity-token",
      "wildcards\t3",
      "library\ttrue",
      "option-fold\tmap-get-or-else",
      "hot\tdemo.kernel.**",
      "bounded\tdemo.Encoding.operands:4",
      "checked\tdemo.where",
      "option\t--baseline",
      "option\t/build/bonsai-baseline.tsv",
      "option\t--bounded",
      "option\tdemo.Encoding.operands",
      "option\t4",
      "option\ta reason\twith a tab"));

    assertEquals(Set.of("acyclic", "local-throw"), configuration.disabled());
    assertEquals(Set.of("broad-catch"), configuration.errors());
    assertEquals(Set.of("identity-token"), configuration.warnings());
    assertEquals(3, configuration.wildcards());
    assertTrue(configuration.library());
    assertEquals(Optional.of("map-get-or-else"), configuration.optionFold());
    assertEquals(Set.of("demo.kernel.**"), configuration.hot());
    assertEquals(List.of("demo.Encoding.operands:4"), configuration.bounded());
    assertEquals(Set.of("demo.where"), configuration.checked());
    assertEquals(
      List.of("--baseline", "/build/bonsai-baseline.tsv", "--bounded", "demo.Encoding.operands", "4", "a reason\twith a tab"),
      configuration.options());
  }

  @Test
  void keepsTheTextLinesWhole() throws IOException {
    List<String> text = List.of("text-exclude\tdocs/**", "text\t0\tkind\tterms", "text\t0\twords\talpha");

    assertEquals(text, Configuration.parse(text).text());
  }

  @Test
  void anEmptyReadIsTheDefaults() throws IOException {
    Configuration configuration = Configuration.parse(List.of());

    assertTrue(configuration.disabled().isEmpty());
    assertEquals(2, configuration.wildcards());
    assertFalse(configuration.library());
    assertEquals(Optional.empty(), configuration.optionFold());
    assertTrue(configuration.options().isEmpty());
    assertTrue(configuration.text().isEmpty());
  }

  @Test
  void anEntryNobodyKnowsFailsTheRead() {
    IOException error = assertThrows(IOException.class, () -> Configuration.parse(List.of("surprise\t1")));
    assertTrue(error.getMessage().contains("surprise"), error.getMessage());
  }
}
