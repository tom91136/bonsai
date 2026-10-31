package net.kurobako.bonsai.maven;

import java.util.TreeSet;

/**
 * The configuration the scalafix rules read, which names the rules that run.
 *
 * <p>A rule the bonsai configuration leaves out does not run, and the bans of the DisableSyntax rule are turned on
 * unless that rule is left out.
 */
public final class Scalafix {

  private Scalafix() {
  }

  /** The configuration the rules read. */
  public static String configuration(Configuration configuration, Ids ids) {
    StringBuilder scalafix = new StringBuilder("rules = [\n");
    for (String rule : new TreeSet<>(ids.rules())) {
      if (!configuration.disabled().contains(rule)) {
        scalafix.append("  \"").append(rule).append("\"\n");
      }
    }
    scalafix.append("]\n");
    if (!configuration.disabled().contains("DisableSyntax")) {
      for (String ban : Ids.bans()) {
        scalafix.append(ban).append(" = ").append(!configuration.disabled().contains(ban)).append("\n");
      }
    }
    return scalafix.toString();
  }
}
