package net.kurobako.bonsai.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BonsaiMojoTest {

  @Test
  void entriesAreReadWhenTheReaderAlsoPrintsToStdout(@TempDir Path directory)
    throws IOException, InterruptedException, MojoExecutionException {
    Path entries = directory.resolve("entries.tsv");
    Configuration configuration = BonsaiMojo.read(child(Noisy.class, entries), entries);
    assertTrue(configuration.disabled().contains("acyclic"));
  }

  @Test
  void aFailingReaderReportsItsStderr(@TempDir Path directory) {
    Path entries = directory.resolve("entries.tsv");
    MojoExecutionException error = assertThrows(
      MojoExecutionException.class,
      () -> BonsaiMojo.read(child(Failing.class, entries), entries));
    assertTrue(error.getMessage().contains("the reader rejected the file"));
  }

  private static List<String> child(Class<?> main, Path entries) {
    return List.of(
      Path.of(System.getProperty("java.home"), "bin", "java").toString(),
      "-cp",
      System.getProperty("java.class.path"),
      main.getName(),
      entries.toString());
  }

  public static final class Noisy {
    public static void main(String[] args) throws IOException {
      System.out.println("[0.011s][info][jfr,startup] Started recording 1.");
      Files.writeString(Path.of(args[0]), "disabled\tacyclic\n", StandardCharsets.UTF_8);
    }
  }

  public static final class Failing {
    public static void main(String[] args) {
      System.err.println("bonsai: the reader rejected the file");
      System.exit(1);
    }
  }
}
