package net.kurobako.bonsai.plugin

import java.util.concurrent.ConcurrentHashMap
import scala.util.matching.Regex

/** The patterns of the methods that throw, with the patterns each qualified name matches kept once found. */
private class ThrowingTable(patterns: List[(Regex, List[String])]) {
  private val matched = ConcurrentHashMap[String, List[(Regex, List[String])]]()

  def matching(name: String): List[(Regex, List[String])] =
    matched.computeIfAbsent(name, _ => patterns.filter((pattern, _) => pattern.matches(name)))
}
