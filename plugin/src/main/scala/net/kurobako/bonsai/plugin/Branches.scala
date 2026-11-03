package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

private object Branches {
  val Checks: Set[Check] = Set(Check.IdenticalBranches, Check.RepeatedBranchCall)
}

/** Reports branches of an `if` chain, a `match` or the handlers of a `try` that run the same code: adjacent branches
  * with equal bodies, whose conditions or patterns join, a branch equal to the default one, which decides nothing, and
  * three or more branches that each call the same method with a few arguments set apart, which one call can make. The
  * bodies are equal when they resolve to the same symbols, up to the names of the values they bind; a body that is only
  * a constant, or that has a comment before the next branch, is left alone.
  */
private class Branches(checks: Checks)(using Context) {
  private val MinimumBranches = 3
  private val MinimumShared   = 2
  private val SharedShare     = 3
  private val text            = Edits.source

  private enum Shape {
    case Bound(binder: tpd.Bind, tpt: tpd.Tree)
    case Free(pieces: List[tpd.Tree])
  }

  private case class Invocation(
      method: Symbol,
      receiver: Option[tpd.Tree],
      types: List[Type],
      lists: List[Int],
      arguments: Vector[tpd.Tree],
      parameters: Vector[Symbol]
  )

  def run(tree: tpd.Tree): Unit = new tpd.TreeTraverser {
    private val chained = mutable.Set.empty[tpd.Tree]

    def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case definition: tpd.DefDef if definition.symbol.is(Flags.Synthetic) && !definition.symbol.isAnonymousFunction =>
        ()
      case branch: tpd.If =>
        if (!branch.isInline && !chained(branch) && Trees.real(branch)) conditional(branch, chained)
        traverseChildren(tree)
      case branch: tpd.Match =>
        if (!branch.isInline && Trees.real(branch)) {
          handlers(branch.cases)
          repeated(branch, branch.cases.map(_.body))
        }
        traverseChildren(tree)
      case branch: tpd.Try =>
        if (Trees.real(branch)) handlers(branch.cases)
        traverseChildren(tree)
      case _ => traverseChildren(tree)
    }
  }.traverse(tree)

  private def trivial(tree: tpd.Tree): Boolean = Trees.constant(Calls.peelInlined(tree))

  private def plain(from: Int, to: Int): Boolean = {
    val gap = text.slice(from, to)
    !gap.contains("//") && !gap.contains("/*")
  }

  private def conditional(head: tpd.If, chained: mutable.Set[tpd.Tree]): Unit = {
    @annotation.tailrec
    def walk(node: tpd.If, arms: Vector[tpd.If]): (Vector[tpd.If], Option[tpd.Tree]) = {
      val all = arms :+ node
      node.elsep match {
        case next: tpd.If if !next.isInline && Trees.real(next) =>
          chained += next
          walk(next, all)
        case other if Trees.real(other) => (all, Some(other))
        case _                          => (all, None)
      }
    }
    val (arms, otherwise) = walk(head, Vector.empty)
    identical(arms)
    otherwise.foreach(redundant(arms.last, _))
    otherwise.foreach(last => repeated(head, arms.map(_.thenp) :+ last))
  }

  private def same(left: tpd.Tree, right: tpd.Tree, prepare: Sameness => Unit = _ => ()): Boolean = {
    val sameness = Sameness()
    prepare(sameness)
    !trivial(left) && sameness(Calls.peelInlined(left), Calls.peelInlined(right))
  }

  private def identical(arms: Vector[tpd.If]): Unit = {
    @annotation.tailrec
    def extent(from: Int, to: Int): Int =
      if (
        to + 1 < arms.size && same(arms(from).thenp, arms(to + 1).thenp) &&
        plain(arms(to).thenp.span.end, arms(to + 1).cond.span.start)
      ) extent(from, to + 1)
      else to
    var from = 0
    while (from < arms.size - 1) {
      val to = extent(from, from)
      if (to > from) {
        val run = arms.slice(from, to + 1)
        checks(
          Check.IdenticalBranches,
          s"the branches for ${run.size} consecutive conditions are the same; join the conditions with ||",
          arms(from).cond.srcPos,
          joined(run)
        )
      }
      from = to + 1
    }
  }

  private def joined(run: Vector[tpd.If]): List[ActionPatch] = {
    val first    = run.head
    val last     = run.last
    val removed  = text.slice(first.thenp.span.end, last.thenp.span.end)
    val balanced = removed.count(_ == '{') == removed.count(_ == '}')
    if (!balanced) Nil
    else {
      val parenthesised = text.slice(first.span.start, first.cond.span.start).trim.endsWith("if")
      val conditions    = run.map(arm => operand(arm.cond)).mkString(" || ")
      List(
        Edits.replace(
          first.cond.span.start,
          first.cond.span.end,
          if (parenthesised) s"($conditions)" else conditions
        ),
        Edits.replace(first.thenp.span.end, last.thenp.span.end, "")
      )
    }
  }

  private def operand(condition: tpd.Tree): String = {
    val shown = Trees.text(condition)
    Calls.peelInlined(condition) match {
      case _: (tpd.Ident | tpd.Select | tpd.Apply | tpd.TypeApply | tpd.Literal) => shown
      case _                                                                     => s"($shown)"
    }
  }

  private def redundant(last: tpd.If, otherwise: tpd.Tree): Unit =
    if (same(last.thenp, otherwise) && plain(last.thenp.span.end, otherwise.span.start)) {
      val fix =
        if (Pures(last.cond)) List(Edits.replace(last.span.start, last.span.end, Trees.text(otherwise))) else Nil
      checks(
        Check.IdenticalBranches,
        "both branches are the same, so the condition decides nothing",
        last.cond.srcPos,
        fix
      )
    }

  private def wildcard(pattern: tpd.Tree): Boolean = pattern match {
    case tpd.Ident(nme.WILDCARD)              => true
    case tpd.Bind(_, tpd.Ident(nme.WILDCARD)) => true
    case _                                    => false
  }

  private def stable(piece: tpd.Tree)(using Context): Boolean = piece match {
    case _: (tpd.Ident | tpd.Select) => piece.symbol.isTerm && piece.symbol.isStableMember
    case _                           => false
  }

  private def testable(tpt: tpd.Tree): Boolean =
    tpt.span.exists && !Trees.text(tpt).matches("(?s).*\\[\\s*[a-z_].*")

  private def pieces(pattern: tpd.Tree): Option[List[tpd.Tree]] = pattern match {
    case tpd.Alternative(trees) =>
      trees.foldLeft(Option(List.empty[tpd.Tree]))((found, tree) => found.zip(pieces(tree)).map(_ ++ _))
    case piece @ tpd.Typed(tpd.Ident(nme.WILDCARD), tpt) if testable(tpt)       => Some(List(piece))
    case piece if (Trees.constant(piece) || stable(piece)) && piece.span.exists => Some(List(piece))
    case _                                                                      => None
  }

  private def shape(pattern: tpd.Tree): Option[Shape] = pattern match {
    case binder @ tpd.Bind(_, tpd.Typed(tpd.Ident(nme.WILDCARD), tpt)) if testable(tpt) && binder.span.exists =>
      Some(Shape.Bound(binder, tpt))
    case other => pieces(other).map(Shape.Free(_))
  }

  private def guarded(left: tpd.CaseDef, right: tpd.CaseDef, sameness: Sameness): Boolean =
    (left.guard.isEmpty && right.guard.isEmpty) ||
      (!left.guard.isEmpty && !right.guard.isEmpty && Pures(left.guard) && sameness(left.guard, right.guard))

  private def alike(left: tpd.CaseDef, right: tpd.CaseDef): Boolean =
    (shape(left.pat), shape(right.pat)) match {
      case (Some(Shape.Bound(a, _)), Some(Shape.Bound(b, _))) =>
        sameBody(left, right, Sameness().rename(a.symbol, b.symbol))
      case (Some(_: Shape.Free), Some(_: Shape.Free)) => sameBody(left, right, Sameness())
      case _                                          => false
    }

  private def sameBody(left: tpd.CaseDef, right: tpd.CaseDef, sameness: Sameness): Boolean =
    guarded(left, right, sameness) && !trivial(left.body) &&
      sameness(Calls.peelInlined(left.body), Calls.peelInlined(right.body))

  private def handlers(cases: List[tpd.CaseDef]): Unit =
    if (cases.nonEmpty && cases.forall(Trees.real)) merge(cases.take(absorbed(cases)))

  private def absorbed(cases: List[tpd.CaseDef]): Int = {
    val fallback = cases.last
    if (cases.size < 2 || !wildcard(fallback.pat) || !fallback.guard.isEmpty) cases.size
    else {
      val before = cases.init.toVector
      val first  = before.indices.reverse.takeWhile { index =>
        val arm   = before(index)
        val bound = arm.pat match {
          case binder: tpd.Bind => Some(binder.symbol)
          case _                => None
        }
        val sameness = Sameness()
        fallback.pat match {
          case other: tpd.Bind => bound.foreach(sameness.rename(_, other.symbol))
          case _               => ()
        }
        shape(arm.pat).isDefined && (arm.guard.isEmpty || Pures(arm.guard)) && !trivial(arm.body) &&
        sameness(Calls.peelInlined(arm.body), Calls.peelInlined(fallback.body)) &&
        plain(arm.span.end, before.lift(index + 1).getOrElse(fallback).span.start)
      }.lastOption
      first.fold(cases.size) { index =>
        val arm = before(index)
        checks(
          Check.IdenticalBranches,
          "the case has the same body as the default case, so it decides nothing",
          arm.pat.srcPos,
          List(Edits.replace(arm.span.start, fallback.span.start, ""))
        )
        index
      }
    }
  }

  private def merge(cases: List[tpd.CaseDef]): Unit = {
    val arms = cases.toVector
    var from = 0
    while (from < arms.size - 1) {
      var to = from
      while (
        to + 1 < arms.size && alike(arms(from), arms(to + 1)) &&
        plain(arms(to).span.end, arms(to + 1).span.start)
      ) to += 1
      if (to > from) {
        val run = arms.slice(from, to + 1)
        checks(
          Check.IdenticalBranches,
          s"${run.size} consecutive cases have the same body; match them with one pattern",
          run.head.pat.srcPos,
          alternatives(run)
        )
      }
      from = to + 1
    }
  }

  private def alternatives(run: Vector[tpd.CaseDef]): List[ActionPatch] = {
    val shapes = run.flatMap(arm => shape(arm.pat))
    val merged = shapes.head match {
      case Shape.Bound(binder, _) =>
        val types = shapes.collect { case Shape.Bound(_, tpt) => Trees.text(tpt) }
        Some(s"${binder.name}: (${types.mkString(" | ")})")
      case Shape.Free(_) =>
        val all   = shapes.collect { case Shape.Free(found) => found }.flatten
        val typed = all.collect { case tpd.Typed(_, tpt) => Trees.text(tpt) }
        Some(if (typed.size == all.size) s"_: (${typed.mkString(" | ")})" else all.map(Trees.text).mkString(" | "))
    }
    merged.toList.flatMap { pattern =>
      List(
        Edits.replace(run.head.pat.span.start, run.head.pat.span.end, pattern),
        Edits.replace(run.head.span.end, run.last.span.end, "")
      )
    }
  }

  private def invocation(body: tpd.Tree)(using Context): Option[Invocation] = {
    @annotation.tailrec
    def peel(
        tree: tpd.Tree,
        lists: List[List[tpd.Tree]],
        types: List[Type]
    ): (tpd.Tree, List[List[tpd.Tree]], List[Type]) =
      tree match {
        case tpd.Apply(function, arguments)     => peel(function, arguments :: lists, types)
        case tpd.TypeApply(function, arguments) => peel(function, lists, arguments.map(_.tpe) ++ types)
        case other                              => (other, lists, types)
      }
    Calls.peelInlined(body) match {
      case application: tpd.Apply =>
        val (head, lists, types) = peel(application, Nil, Nil)
        val method               = head.symbol
        val parameters           = Trees.termParameters(method).flatten.toVector
        val arguments            = lists.flatten.toVector
        Option.when(
          method.exists && method.is(Flags.Method) && !Trees.constructs(method) && parameters.size == arguments.size
        )(
          Invocation(
            method,
            head match {
              case tpd.Select(qualifier, _) => Some(qualifier)
              case _                        => None
            },
            types,
            lists.map(_.size),
            arguments,
            parameters
          )
        )
      case _ => None
    }
  }

  private def repeated(owner: tpd.Tree, bodies: Seq[tpd.Tree]): Unit =
    if (bodies.size >= MinimumBranches && bodies.forall(Trees.real)) {
      val calls = bodies.flatMap(invocation(_))
      if (calls.size == bodies.size) {
        val first = calls.head
        val alike = calls.tail.forall(call =>
          call.method == first.method && call.lists == first.lists && call.types.size == first.types.size &&
            call.types.lazyZip(first.types).forall(_ =:= _) &&
            call.receiver.isDefined == first.receiver.isDefined &&
            call.receiver.zip(first.receiver).forall((left, right) => Sameness()(left, right))
        )
        if (alike) {
          val positions = first.arguments.indices
          val shared    = positions.filter(index =>
            calls.tail.forall(call => Sameness()(first.arguments(index), call.arguments(index)))
          ).toSet
          val differing = positions.filterNot(shared.contains)
          val varying   = differing.map(index => calls.map(_.arguments(index)))
          val callee    =
            first.receiver.filter(_ => first.method.name == nme.apply).fold(first.method.name.toString)(Trees.text)
          if (
            shared.size >= MinimumShared && differing.nonEmpty && shared.size * SharedShare >= positions.size &&
            differing.forall(index => !Trees.byName(first.parameters(index))) &&
            varying.forall(_.forall(argument => Calls.lambda(argument).isEmpty)) &&
            !varying.forall(_.forall(trivial(_)))
          )
            checks(
              Check.RepeatedBranchCall,
              s"every branch calls $callee, differing in " +
                s"${differing.map(first.parameters(_).name).mkString(", ")}; choose those per branch and call once",
              owner.srcPos
            )
        }
      }
    }
}
