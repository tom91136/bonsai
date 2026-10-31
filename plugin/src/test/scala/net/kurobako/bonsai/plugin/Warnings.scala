package net.kurobako.bonsai.plugin

/** Compiles a source with the plugin and returns what it reports. */
object Warnings {

  /** The messages of the warnings, each ending in the id of the check that reported it. */
  def of(source: String, options: String*): List[String] =
    Compilations.run(Seq("Lints.scala" -> source), options) { (result, _) =>
      val errors = result.allErrors.map(_.message).toList
      assertNoErrors(errors)
      result.allWarnings.map(_.message).toList
    }

  private def assertNoErrors(errors: List[String]): Unit =
    if (errors.nonEmpty) throw AssertionError(errors.mkString("\n"))

  /** The reports of one check, sorted, without anything after the first semicolon. */
  def tagged(found: List[String], check: String): List[String] =
    found.collect { case message if message.endsWith(s"[bonsai:$check]") => message.takeWhile(_ != ';') }.sorted
}
