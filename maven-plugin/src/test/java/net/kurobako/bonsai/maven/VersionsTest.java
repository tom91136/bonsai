package net.kurobako.bonsai.maven;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class VersionsTest {

  @TempDir
  Path root;

  @Test
  void aFinalReleaseIsOfItsMinorLine() {
    assertEquals(Optional.of("3.3"), Versions.line("3.3.0"));
    assertEquals(Optional.of("3.7"), Versions.line("3.7.4"));
    assertEquals(Optional.of("3.10"), Versions.line("3.10.0"));
  }

  @Test
  void aReleaseWithoutFinalTastyHasNoLine() {
    assertEquals(Optional.empty(), Versions.line("3.7.0-RC1"));
    assertEquals(Optional.empty(), Versions.line("3.8.0-RC1-bin-20250101-abc-NIGHTLY"));
    assertEquals(Optional.empty(), Versions.line("2.13.16"));
  }

  @Test
  void aPluginRunsFromTheOldestPatchItPublishesOfItsLine() {
    assertTrue(Versions.runsIn("3.7.4", "3.7.4"));
    assertTrue(Versions.runsIn("3.7.5", "3.7.4"));
    assertFalse(Versions.runsIn("3.7.3", "3.7.4"));
    assertFalse(Versions.runsIn("3.8.9", "3.7.4"));
    assertFalse(Versions.runsIn("3.7.4-RC1", "3.7.4"));
    assertTrue(Versions.runsIn("3.6.0", "3.6.0"));
  }

  @Test
  void theAnalysisReadsItsOwnMinorLineAndTheOlderOnes() {
    assertTrue(Versions.readable("3.10.0", "3.10.0"));
    assertTrue(Versions.readable("3.10.7", "3.10.0"));
    assertTrue(Versions.readable("3.3.5", "3.10.0"));
    assertFalse(Versions.readable("3.11.0", "3.10.0"));
    assertFalse(Versions.readable("3.10.0-RC1", "3.10.0"));
    assertFalse(Versions.readable("2.13.16", "3.10.0"));
  }

  @Test
  void theAnalysisPublishesTheScalaVersionItIsBuiltWith() throws IOException {
    Path jar = root.resolve("analysis.jar");
    try (OutputStream out = Files.newOutputStream(jar); ZipOutputStream zip = new ZipOutputStream(out)) {
      zip.putNextEntry(new ZipEntry("META-INF/bonsai/scala.id"));
      zip.write("3.10.0\n".getBytes());
      zip.closeEntry();
    }
    assertEquals("3.10.0", Versions.analysisScala(jar.toFile()));
  }

  @Test
  void anAnalysisThatDoesNotPublishItsScalaVersionIsNotOneThePluginKnows() throws IOException {
    Path jar = root.resolve("other.jar");
    try (OutputStream out = Files.newOutputStream(jar); ZipOutputStream zip = new ZipOutputStream(out)) {
      zip.putNextEntry(new ZipEntry("other"));
      zip.closeEntry();
    }
    assertThrows(IOException.class, () -> Versions.analysisScala(jar.toFile()));
  }

  @Test
  void theRulesPublishTheScalafixVersionAndTheScalaVersionTheyAreBuiltWith() throws IOException {
    Path jar = root.resolve("rules.jar");
    try (OutputStream out = Files.newOutputStream(jar); ZipOutputStream zip = new ZipOutputStream(out)) {
      zip.putNextEntry(new ZipEntry("META-INF/bonsai/versions.properties"));
      zip.write("scalafix.version=0.14.9\nscalafix.scalaVersion=3.9.0\n".getBytes());
      zip.closeEntry();
    }
    Versions.ScalafixVersions scalafix = Versions.scalafix(jar.toFile());
    assertEquals("0.14.9", scalafix.version());
    assertEquals("3.9.0", scalafix.scalaVersion());
  }

  @Test
  void theRulesPublishWhichReleasesTheirRulesRunIn() throws IOException {
    Path jar = root.resolve("support.jar");
    try (OutputStream out = Files.newOutputStream(jar); ZipOutputStream zip = new ZipOutputStream(out)) {
      zip.putNextEntry(new ZipEntry("META-INF/bonsai/versions.properties"));
      zip.write("removeUnused.from=3.3.4\nexplicitResultTypes.excluded=3.4,3.6\n".getBytes());
      zip.closeEntry();
    }
    Versions.Support support = Versions.support(jar.toFile());

    assertFalse(support.removeUnused("3.3.3"));
    assertTrue(support.removeUnused("3.3.4"));
    assertTrue(support.removeUnused("3.4.0"));
    assertTrue(support.removeUnused("3.8.0-RC1"));
    assertFalse(support.removeUnused("2.13.16"));
    assertFalse(support.explicitResultTypes("3.4.3"));
    assertFalse(support.explicitResultTypes("3.6.0"));
    assertTrue(support.explicitResultTypes("3.3.9"));
    assertTrue(support.explicitResultTypes("3.10.0"));
    assertFalse(support.explicitResultTypes("2.13.16"));
  }

  @Test
  void anEmptyExclusionExcludesNoLine() throws IOException {
    Path jar = root.resolve("none.jar");
    try (OutputStream out = Files.newOutputStream(jar); ZipOutputStream zip = new ZipOutputStream(out)) {
      zip.putNextEntry(new ZipEntry("META-INF/bonsai/versions.properties"));
      zip.write("removeUnused.from=3.3.4\nexplicitResultTypes.excluded=\n".getBytes());
      zip.closeEntry();
    }
    assertTrue(Versions.support(jar.toFile()).explicitResultTypes("3.4.3"));
  }
}
