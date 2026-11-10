package net.kurobako.bonsai.maven;

import java.util.TreeSet;

/**
 * The configuration the scalafix rules read, which names the rules that run.
 *
 * <p>A rule the bonsai configuration leaves out does not run, and an opt-in rule runs only when the configuration
 * enables it. The bans of the DisableSyntax rule are turned on unless that rule is left out. A rule the Scala version
 * of the build cannot run with, because its compiler lacks the unused warnings or scalafix cannot load a presentation
 * compiler for it, is left out too.
 */
public final class Scalafix {

  private Scalafix() {
  }

  /** The configuration the rules read. */
  public static String configuration(Configuration configuration, Ids ids, Versions.Support support, String scala) {
    StringBuilder scalafix = new StringBuilder("rules = [\n");
    for (String rule : new TreeSet<>(ids.rules())) {
      boolean enabled = !ids.optIn().contains(rule) || configuration.enabled().contains(rule);
      if (enabled && !configuration.disabled().contains(rule) && supported(rule, support, scala)) {
        scalafix.append("  \"").append(rule).append("\"\n");
      }
    }
    scalafix.append("]\n");
    for (String setting : ids.settings()) {
      scalafix.append(setting).append("\n");
    }
    if (!configuration.disabled().contains("DisableSyntax")) {
      ids.banSettings().forEach((setting, lints) -> {
        boolean disabled = lints.stream().anyMatch(configuration.disabled()::contains);
        scalafix.append(setting).append(" = ").append(!disabled).append("\n");
      });
    }
    scalafix.append("lint.warning = [\n");
    for (String id : new TreeSet<>(configuration.nonfatal())) {
      if (ids.rules().contains(id) || ids.bans().contains(id)) {
        for (String lint : ids.lints(id)) {
          scalafix.append("  \"").append(lint).append("\"\n");
        }
      }
    }
    scalafix.append("]\n");
    return scalafix.toString();
  }

  /** Whether the rule can run on the Scala version of the build. */
  static boolean supported(String rule, Versions.Support support, String scala) {
    return switch (rule) {
      case "RemoveUnused" -> support.removeUnused(scala);
      case "ExplicitResultTypes" -> support.explicitResultTypes(scala);
      default -> true;
    };
  }
}
