package net.kurobako.bonsai.maven;

import java.util.Locale;
import java.util.Map;

/**
 * The mode the tools run in: the relaxed mode reports the findings without failing the build, the strict mode fails on
 * them, and the audit mode compiles whatever the compiler plugin finds, so that the analysis reports every finding of
 * the build at once and fails as the strict mode would.
 *
 * <p>The environment names the mode, and the build runs in the strict mode when it runs on a CI server.
 */
public final class Mode {

  private Mode() {
  }

  /** The mode the environment names. */
  public static String of(Map<String, String> environment) {
    String named = environment.get("BONSAI_MODE");
    if (named != null && !named.isBlank()) {
      String mode = named.toLowerCase(Locale.ROOT);
      if (!mode.equals("relaxed") && !mode.equals("strict") && !mode.equals("audit")) {
        throw new IllegalArgumentException("bonsai: BONSAI_MODE must be one of relaxed, strict, audit, not " + named);
      }
      return mode;
    }
    return environment.containsKey("CI") ? "strict" : "relaxed";
  }
}
