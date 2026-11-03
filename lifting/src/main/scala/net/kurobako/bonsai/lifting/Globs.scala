package net.kurobako.bonsai.lifting

import java.nio.file.FileSystems
import java.nio.file.PathMatcher
import java.util.concurrent.ConcurrentHashMap
import scala.util.matching.Regex

object Globs {

  /** A glob over qualified names, matched against a name with a dot appended: `*` stands for one name segment or part
    * of one, and `**` for any number of segments.
    */
  def qualifiedNames(glob: String): Regex = glob.split('.').map {
    case "**"    => "(?:[^.]+\\.)*"
    case segment => segment.split("\\*", -1).map(Regex.quote).mkString("[^.]*") + "\\."
  }.mkString.r

  /** Whether `glob` selects `name`. */
  def matches(glob: Regex, name: String): Boolean = glob.matches(s"$name.")

  /** Whether `name` is selected by a `!`-prefixed glob, which leaves it out of the hot names. */
  def cold(globs: Seq[String]): String => Boolean = {
    val excluded = globs.collect { case glob if glob.startsWith("!") => qualifiedNames(glob.drop(1)) }
    memoised(name => excluded.exists(matches(_, name)))
  }

  /** Whether `name` is selected by one of the `globs` and by none of the `!`-prefixed ones. */
  def heat(globs: Seq[String]): String => Boolean = heat(cold(globs), globs)

  /** Whether `name` is selected by one of the `globs` and by none of the `!`-prefixed ones, where `isCold` is
    * `cold(globs)`.
    */
  def heat(isCold: String => Boolean, globs: Seq[String]): String => Boolean = {
    val hot = globs.collect { case glob if !glob.startsWith("!") => qualifiedNames(glob) }
    name => !isCold(name) && hot.exists(matches(_, name))
  }

  private def memoised(test: String => Boolean): String => Boolean = {
    val known = ConcurrentHashMap[String, java.lang.Boolean]()
    name => known.computeIfAbsent(name, key => test(key))
  }

  def matcher(glob: String): PathMatcher = FileSystems.getDefault.getPathMatcher(s"glob:$glob")
}
