package net.kurobako.bonsai.maven;

import org.eclipse.aether.RepositorySystemSession;

import java.util.HashSet;
import java.util.Set;

/**
 * The modules of a build that have written their fragment of the manifest.
 *
 * <p>The record belongs to the session of the build, so a build never sees the modules of another one. Modules finish
 * in any order when the build runs them in parallel, so the module that completes the set is the one that runs the
 * analysis, whichever module that is.
 */
final class Roster {

  private static final String KEY = Roster.class.getName();

  private final Set<String> modules = new HashSet<>();

  /** The record of the build the session belongs to, which every module of the build shares. */
  static Roster of(RepositorySystemSession session) {
    return (Roster) session.getData().computeIfAbsent(KEY, Roster::new);
  }

  /**
   * Records that the module has written its fragment.
   *
   * @return whether this module is the one that completes the set of the modules, which is true for one module alone
   */
  synchronized boolean record(String module, int expected) {
    return modules.add(module) && modules.size() == expected;
  }
}
