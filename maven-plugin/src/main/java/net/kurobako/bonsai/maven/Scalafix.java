package net.kurobako.bonsai.maven;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * The configuration the scalafix rules read, which names the rules that run.
 *
 * <p>A rule the bonsai configuration leaves out does not run, and the bans of the DisableSyntax rule are turned on
 * unless that rule is left out.
 */
public final class Scalafix {

  /** The syntax scalafix itself provides, which bonsai turns on and off as a whole. */
  private static final List<String> BUILT_INS = List.of("DisableSyntax", "RedundantSyntax");

  private Scalafix() {
  }

  /** The configuration the rules read. */
  public static String configuration(Configuration configuration, Ids ids) {
    List<String> rules = new ArrayList<>();
    for (String rule : new TreeSet<>(ids.rules())) {
      if (!configuration.disabled().contains(rule)) {
        rules.add(rule);
      }
    }
    StringBuilder scalafix = new StringBuilder("rules = [\n");
    for (String rule : rules) {
      scalafix.append("  \"").append(rule).append("\"\n");
    }
    scalafix.append("]\n");
    for (String ban : Ids.bans()) {
      if (!configuration.disabled().contains("DisableSyntax")) {
        scalafix.append(ban).append(" = ").append(!configuration.disabled().contains(ban)).append("\n");
      }
    }
    return scalafix.toString();
  }
}
