package net.kurobako.bonsai.maven;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.Optional;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * The Scala releases the tools serve: the compiler plugin, by the minor line it is built for, and the analysis, by the
 * TASTy its compiler reads.
 */
public final class Versions {

  private static final Pattern RELEASE = Pattern.compile("3\\.(\\d+)\\.(\\d+)");

  private Versions() {}

  /** The minor line of a final release, as {@code 3.7}; a release candidate or a nightly has TASTy of its own. */
  public static Optional<String> line(String version) {
    return minor(version).map(minor -> "3." + minor);
  }

  /**
   * Whether the compiler plugin of the line runs in the release, which it does from the oldest patch the plugin
   * publishes: it is built against the latest patch of the line, and calls compiler methods that earlier patches lack.
   */
  public static boolean runsIn(String version, String oldest) {
    Matcher match = RELEASE.matcher(version);
    Matcher first = RELEASE.matcher(oldest);
    return match.matches() && first.matches() && match.group(1).equals(first.group(1)) &&
      Integer.parseInt(match.group(2)) >= Integer.parseInt(first.group(2));
  }

  /** The oldest release the compiler plugin runs in, which the jar publishes. */
  public static String oldest(File plugin) throws IOException {
    return resource(plugin, "oldest.id");
  }

  /**
   * Whether the compiler of the analysis reads the TASTy of the release, which a compiler does for its own minor line
   * and the older ones.
   */
  public static boolean readable(String version, String analysis) {
    return minor(version).flatMap(project -> minor(analysis).map(newest -> project <= newest)).orElse(false);
  }

  /** The Scala version the analysis is built with, which the jar publishes. */
  public static String analysisScala(File analysis) throws IOException {
    return resource(analysis, "scala.id");
  }

  /** The versions the scalafix rules run with, which the rules jar publishes. */
  public record ScalafixVersions(String version, String scalaVersion) {}

  /** The scalafix version and the Scala version its rules are built for, which the rules jar publishes. */
  public static ScalafixVersions scalafix(File rules) throws IOException {
    Properties properties = new Properties();
    properties.load(new StringReader(resource(rules, "versions.properties")));
    return new ScalafixVersions(
      property(rules, properties, "scalafix.version"),
      property(rules, properties, "scalafix.scalaVersion"));
  }

  private static String property(File jar, Properties properties, String key) throws IOException {
    String value = properties.getProperty(key);
    if (value == null) {
      throw new IOException("bonsai: " + jar + " does not publish " + key);
    }
    return value.trim();
  }

  static String resource(File jar, String name) throws IOException {
    try (ZipFile file = new ZipFile(jar)) {
      ZipEntry entry = file.getEntry("META-INF/bonsai/" + name);
      if (entry == null) {
        throw new IOException("bonsai: " + jar + " does not publish " + name + "; the tools are not the ones this " +
          "plugin knows");
      }
      try (InputStream stream = file.getInputStream(entry)) {
        return new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim();
      }
    }
  }

  private static Optional<Integer> minor(String version) {
    Matcher match = RELEASE.matcher(version);
    return match.matches() ? Optional.of(Integer.parseInt(match.group(1))) : Optional.empty();
  }
}
