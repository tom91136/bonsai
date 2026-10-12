package net.kurobako.baumkuchen.plugin

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.plugins.StandardPlugin
import dotty.tools.dotc.report
import java.nio.file.FileSystems

class BaumkuchenPlugin extends StandardPlugin {
  val name        = "baumkuchen"
  val description = "Check source cycles and lint typed expressions"

  private val analysis         = Analysis()
  private val DefaultWildcards = 2
  private val DefaultGenerated = List("**/src/generated/**", "src/generated/**")
  // Mutation testing compiles rewritten copies of the sources from a temporary directory, which no check judges.
  private val Instrumented = List("**/stryker4s-*/**", "stryker4s-*/**")
  private val Modes        = List("dev", "ci")
  private val Keys         = Set("mode", "disable", "error", "warn", "wildcards", "generated", "partial", "throws")

  override def initialize(options: List[String])(using Context): List[PluginPhase] = {
    options.filterNot {
      case s"$key=$_" => Keys(key)
      case _          => false
    }.foreach(option => report.error(s"unknown baumkuchen option: $option"))
    def values(key: String) = options.collect { case s"$option=$value" if option == key => value }
    def checks(key: String) = values(key).flatMap(named(_)).toSet
    val wildcards           = values("wildcards").lastOption.fold(DefaultWildcards)(count =>
      count.toIntOption.filter(_ >= 0).getOrElse {
        report.error(s"baumkuchen option wildcards must be a non-negative count, not $count")
        DefaultWildcards
      }
    )
    val generated = values("generated") match {
      case Nil   => DefaultGenerated
      case globs => globs
    }
    val mode = values("mode").lastOption.getOrElse("dev")
    if (!Modes.contains(mode)) report.error(s"baumkuchen option mode must be one of ${Modes.mkString(", ")}, not $mode")
    val disabled = checks("disable")
    val errors   = checks("error")
    val warnings = checks("warn")
    val strict   = if (mode == "ci") Check.values.filter(_.strict).toSet else Set.empty
    // `partial=<method>:<alternative>` and `throws=<method pattern>:<exception>|...` extend the defaults, and an empty
    // right-hand side removes one.
    val partials = values("partial").foldLeft(Exceptions.Partials) {
      case (table, s"$method:")             => table - method
      case (table, s"$method:$alternative") =>
        table + (method -> Exceptions.Partial(alternative, alternative.matches(".*(Option|Opt)")))
      case (table, option) =>
        report.error(s"baumkuchen option partial must be <method>:<alternative>, not $option")
        table
    }
    // Scala.js compiles with -scalajs, and Scala Native puts its nativelib on the classpath.
    val platform =
      if (ctx.settings.scalajs.value) Exceptions.Platform.JS
      else if (ctx.settings.classpath.value.contains("nativelib_native")) Exceptions.Platform.Native
      else Exceptions.Platform.JVM
    val throwing = values("throws").foldLeft(Exceptions.throws(platform)) {
      case (table, s"$pattern:")           => table - pattern
      case (table, s"$pattern:$exception") => table + (pattern -> exception.split('|').toList)
      case (table, option)                 =>
        report.error(s"baumkuchen option throws must be <method pattern>:<exception>, not $option")
        table
    }
    val escalated                     = strict ++ errors -- warnings
    def matchers(globs: List[String]) = globs.map(glob => FileSystems.getDefault.getPathMatcher(s"glob:$glob"))
    List(
      // A cycle through a generated source still ties together the handwritten sources on it.
      AcyclicPhase(analysis, Checks(disabled, escalated, matchers(Instrumented))),
      LintPhase(
        wildcards,
        Checks(disabled, escalated, matchers(generated ++ Instrumented)),
        partials,
        throwing.toList.map((pattern, exceptions) => pattern.r -> exceptions)
      )
    )
  }

  /** The checks an option names: one by its id, or every check with `*`. */
  private def named(id: String)(using Context): List[Check] =
    if (id == "*") Check.values.toList
    else {
      val found = Check.values.find(_.id == id)
      if (found.isEmpty) report.error(s"unknown baumkuchen check: $id")
      found.toList
    }
}
