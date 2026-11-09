package net.kurobako.bonsai.maven;

import java.util.Locale;
import java.util.Map;

/**
 * The mode the tools run in: the dev mode reports the findings without failing the build, the ci mode fails on them,
 * and the audit mode compiles whatever the compiler plugin finds, so that the analysis reports every finding of the
 * build at once and fails as the ci mode would.
 *
 * <p>The environment names the mode, and the build runs in the ci mode when it runs on a CI server.
 */
public final class Mode {

  private Mode() {
  }

  /** The mode the environment names. */
  public static String of(Map<String, String> environment) {
    String named = environment.get("BONSAI_MODE");
    if (named != null && !named.isBlank()) {
      String mode = named.toLowerCase(Locale.ROOT);
      if (!mode.equals("dev") && !mode.equals("ci") && !mode.equals("audit")) {
        throw new IllegalArgumentException("bonsai: BONSAI_MODE must be one of dev, ci, audit, not " + named);
      }
      return mode;
    }
    return environment.containsKey("CI") ? "ci" : "dev";
  }
}
