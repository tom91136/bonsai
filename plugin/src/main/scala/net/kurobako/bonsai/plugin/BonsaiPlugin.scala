package net.kurobako.bonsai.plugin

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.plugins.StandardPlugin
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.effects.*
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

class BonsaiPlugin extends StandardPlugin {
  val name        = "bonsai"
  val description = "Check source cycles and lint typed expressions"

  private val analysis         = Analysis()
  private val DefaultWildcards = 2
  // Mutation testing compiles rewritten copies of the sources from a temporary directory, which no check judges.
  private val Instrumented = List("**/stryker4s-*/**", "stryker4s-*/**")
  private val Modes        = List("relaxed", "strict", "audit")
  private val Keys         =
    Set(
      "mode",
      "disable",
      "error",
      "warn",
      "wildcards",
      "partial",
      "throws",
      "hot",
      "bounded",
      "checked",
      "option-fold",
      "generated",
      "generated-disable"
    )

  // The compilers before 3.5 give no context to initialise a plugin with, so the problems with the options wait for the
  // first phase to report them.
  override def init(options: List[String]): List[PluginPhase] = {
    val problems = mutable.ListBuffer.empty[String]
    options.filterNot {
      case s"$key=$_" => Keys(key)
      case _          => false
    }.foreach(option => problems += s"unknown bonsai option: $option")
    def values(key: String) = options.collect { case s"$option=$value" if option == key => value }
    def checks(key: String) = values(key).flatMap(named(_, problems)).toSet
    val wildcards           = values("wildcards").lastOption.fold(DefaultWildcards)(count =>
      count.toIntOption.filter(_ >= 0).getOrElse {
        problems += s"bonsai option wildcards must be a non-negative count, not $count"
        DefaultWildcards
      }
    )
    val mode = values("mode").lastOption.getOrElse("relaxed")
    if (!Modes.contains(mode)) problems += s"bonsai option mode must be one of ${Modes.mkString(", ")}, not $mode"
    val optionFold = values("option-fold").lastOption.fold(OptionFold.Default)(key =>
      OptionFold.named(key).getOrElse {
        problems += s"bonsai option option-fold must be one of ${OptionFold.values.map(_.key).mkString(", ")}, not $key"
        OptionFold.Default
      }
    )
    val disabled = checks("disable")
    val errors   = checks("error")
    val warnings = checks("warn")
    val fatal    = if (mode == "strict" || mode == "audit") Check.all.filter(_.fatal).toSet else Set.empty
    val records  = Option.when(mode == "audit")(Records())
    // `partial=<method>:<alternative>` and `throws=<method pattern>:<exception>|...` extend the defaults, and an empty
    // right-hand side removes one.
    val partials = values("partial").foldLeft(Partials.Alternatives) {
      case (table, s"$method:")             => table - method
      case (table, s"$method:$alternative") =>
        table + (method -> Partials.Partial(alternative, alternative.matches(".*(Option|Opt)")))
      case (table, option) =>
        problems += s"bonsai option partial must be <method>:<alternative>, not $option"
        table
    }
    val parsed = values("throws").map {
      case s"$pattern:"           => Right((table: Map[String, List[String]]) => table - pattern)
      case s"$pattern:$exception" =>
        Right((table: Map[String, List[String]]) => table + (pattern -> exception.split('|').toList))
      case option => Left(s"bonsai option throws must be <method pattern>:<exception>, not $option")
    }
    problems ++= parsed.collect { case Left(problem) => problem }
    val edits    = parsed.collect { case Right(edit) => edit }
    val throwing = Deferred(
      ThrowingTable(
        edits.foldLeft(Throwing.throws(platform))((table, edit) => edit(table)).toList.map((pattern, exceptions) =>
          pattern.r -> exceptions
        )
      )
    )
    // `checked=<type>` names, by its qualified name, a refinement type whose checked operations the others bypass.
    // `hot=<glob>` marks the classes whose code runs hot, by the qualified name of the class or object, and
    // `hot=!<glob>` leaves out classes the others mark, such as the suites of a hot package.
    val isCold = Globs.cold(values("hot"))
    val hot    = Globs.heat(isCold, values("hot"))
    // `bounded=<member>:<max>` declares, by the qualified name of the member or of its owner, that the collections of a
    // member hold at most `max` elements, so walking them costs constant time.
    val bounds = values("bounded").map {
      case s"$member:$max" if max.toIntOption.exists(_ >= 0) => Right(Globs.qualifiedNames(member))
      case option => Left(s"bonsai option bounded must be <member>:<max>, not $option")
    }
    problems ++= bounds.collect { case Left(problem) => problem }
    val bounded       = bounds.collect { case Right(glob) => glob }
    val escalated     = fatal ++ errors -- warnings
    val matchers      = Instrumented.map(Globs.matcher)
    val generated     = values("generated").map(Globs.matcher)
    val overGenerated =
      if (values("generated-disable").isEmpty) Check.all.toSet
      else values("generated-disable").filter(_.nonEmpty).flatMap(named(_, problems)).toSet
    val phaseChecks = Checks(
      disabled,
      escalated,
      warnings,
      path => matchers.exists(_.matches(path)),
      path => generated.exists(_.matches(path)),
      overGenerated,
      analysis.suppressions,
      records = records
    )
    List(
      // A cycle through a mutation-testing copy still ties together the sources on it.
      AcyclicPhase(analysis, phaseChecks, problems.toList),
      LintPhase(
        wildcards,
        phaseChecks,
        partials,
        throwing,
        hot,
        values("hot").exists(!_.startsWith("!")),
        isCold,
        (name, owner) => bounded.exists(Globs.reaches(_, name, owner)),
        values("checked").toSet,
        optionFold
      )
    ) ++ records.map(RecordPhase(_))
  }

  /** The checks an option names: one by its id, or every check with `*`. */
  private def named(id: String, problems: mutable.ListBuffer[String]): List[Check] =
    if (id == "*") Check.all
    else {
      val found = Check.all.find(_.id == id)
      if (found.isEmpty) problems += s"unknown bonsai check: $id"
      found.toList
    }

  // Scala.js compiles with -scalajs, and Scala Native puts its nativelib on the classpath.
  private def platform(using Context): Throwing.Platform =
    if (ctx.settings.scalajs.value) Throwing.Platform.JS
    else if (ctx.settings.classpath.value.contains("nativelib_native")) Throwing.Platform.Native
    else Throwing.Platform.JVM
}
