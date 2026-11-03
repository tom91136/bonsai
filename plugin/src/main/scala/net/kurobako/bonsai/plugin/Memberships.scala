package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.lifting.Calls
import net.kurobako.bonsai.lifting.Classes
import scala.collection.mutable

private object Memberships {

  /** Whether a tree runs once each time the code around it runs, or once per iteration of a loop. */
  enum Repetition {
    case Once, Looped
  }
}

/** Tests of a value against a collection of constants written in place, as `Set("Pg", "UPg")(binding)`, which name a
  * category the code never names. A guard that tests what its pattern binds wants pattern alternatives, a category of
  * enum cases wants a member of the enum or a match, which checks exhaustiveness, and a set repeated in a file or
  * rebuilt on every iteration wants a name; a one-off test in code that runs once is left alone.
  */
private class Memberships(checks: Checks)(using Context) {
  private case class Site(set: tpd.Tree, key: List[String], immediate: Option[String], fallback: Option[String])

  private val sites = mutable.ArrayBuffer.empty[Site]

  private val Iterable = Classes.required("scala.collection.Iterable")
  private val Set      = Classes.required("scala.collection.Set")

  /** Records the constant collections `application` tests membership in, given the symbols the pattern of an enclosing
    * guard binds, and returns their spans.
    */
  def observe(application: tpd.Apply, repetition: Memberships.Repetition, guarded: Symbol => Boolean): List[Span] = {
    val observed = tested(application).map { (set, elements, value, data) =>
      val shown     = Trees.text(set)
      val immediate =
        if (bound(value, guarded))
          Some(s"$shown in a guard tests what the pattern binds; match it with pattern alternatives instead")
        else if (elements.forall(Trees.enumerated(_))) {
          val category = set.tpe.widen.baseType(Iterable).argInfos.headOption.fold("")(_.show)
          Some(
            s"$shown names a category of $category; make it a member of $category or a match, which checks " +
              "exhaustiveness"
          )
        } else None
      val fallback = Option.when(repetition == Memberships.Repetition.Looped)(
        if (data) s"$shown is a table rebuilt on every iteration; make it a val named for what it holds"
        else s"what does $shown stand for? it is rebuilt and hashed on every iteration; name it as a member or a match"
      )
      Site(set, elements.map(Trees.text(_)).sorted, immediate, fallback)
    }
    sites ++= observed
    observed.map(_.set.span)
  }

  /** Reports the recorded tests, naming the sets a file repeats. */
  def report(): Unit = {
    val counts = sites.groupMapReduce(_.key)(_ => 1)(_ + _)
    sites.foreach { site =>
      val repeated = Option.when(counts(site.key) > 1)(
        s"${Trees.text(site.set)} appears ${counts(site.key)} times in this file; name the concept it tests"
      )
      site.immediate.orElse(repeated).orElse(site.fallback).foreach(checks(
        Check.InlineSetMembership,
        _,
        site.set.srcPos
      ))
    }
  }

  // A set passed where a predicate is expected tests every element it is applied to, as data rather than a condition.
  private def tested(application: tpd.Apply): List[(tpd.Tree, List[tpd.Tree], tpd.Tree, Boolean)] =
    application match {
      case tpd.Apply(tpd.Select(receiver, name), List(value))
          if name.toString == "contains" || name == nme.apply && receiver.tpe.widen.derivesFrom(Set) =>
        constants(receiver).map((receiver, _, value, false)).toList
      case tpd.Apply(function, arguments) =>
        val receiver   = Calls.selected(function).map(_.qualifier)
        val parameters = Trees.parameters(application)
        arguments.zipWithIndex.flatMap((argument, index) =>
          Option.when(parameters.lift(index).exists(parameter => defn.isFunctionType(parameter.info)))(argument)
            .flatMap(set => constants(set).map((set, _, receiver.getOrElse(set), true)))
        )
    }

  private def constants(tree: tpd.Tree): Option[List[tpd.Tree]] =
    Trees.built(tree).filter(elements => elements.nonEmpty && elements.forall(Trees.constant(_)))

  private def bound(value: tpd.Tree, guarded: Symbol => Boolean): Boolean = Calls.peelInlined(value) match {
    case reference: tpd.Ident => guarded(reference.symbol)
    case _                    => false
  }
}
