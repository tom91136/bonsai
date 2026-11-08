package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.MethodType
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

@SuppressWarnings(Array("bonsai:test-only", "bonsai:test-only-member"))
object Dictionaries {

  /** The instances that reach the dictionary parameter `index`, counted over all the term parameters of `method`, from
    * the calls of production code. `open` holds when some caller passes a value no instance names, or the parameter has
    * no caller to name one, as an exported method has none.
    */
  case class Parameter(method: String, index: Int, instances: Vector[String], open: Boolean)

  private case class Slot(method: Symbol, index: Int)

  private enum Term {
    case Instance(name: String, arguments: Vector[Term])
    case Forward(slot: Slot)
    case Open
  }

  private case class Reaching(instances: Set[String], open: Boolean)
}

/** The given arguments of the calls of the build, which a call writes out as trees, followed from the parameters they
  * fill to the parameters their callers forward them from.
  */
private[analysis] class Dictionaries(symbols: Symbols) {
  import Dictionaries.*
  import symbols.*

  private val declared = mutable.LinkedHashMap.empty[Symbol, Vector[Symbol]]
  private val passed   = mutable.Map.empty[Slot, mutable.ArrayBuffer[Term]]

  private def clauses(method: Symbol)(using Context): List[List[Symbol]] =
    method.paramSymss.filter(clause => clause.isEmpty || clause.head.isTerm)

  /** Notes the dictionary parameters of a method, which are those its `using` clauses declare. */
  def declare(method: Symbol)(using Context): Unit =
    if (method.is(Flags.Method) && !method.is(Flags.Synthetic)) {
      val parameters = clauses(method).flatten
      if (parameters.exists(_.is(Flags.Given))) declared(method) = parameters.toVector
    }

  /** Notes the givens `application` passes, when it fills a `using` clause. */
  def observe(application: tpd.Apply, from: Option[String])(using Context): Unit =
    application.fun.tpe.widen match {
      case filled: MethodType if filled.isContextualMethod && !from.flatMap(nodes.get).exists(_.test) =>
        val callee = application.symbol
        val before = clause(application.fun)
        val offset = clauses(callee).take(before).map(_.size).sum
        application.args.zipWithIndex.foreach((argument, position) =>
          passed.getOrElseUpdate(Slot(callee, offset + position), mutable.ArrayBuffer.empty) += term(argument)
        )
      case _ => ()
    }

  @annotation.tailrec
  private def clause(function: tpd.Tree, depth: Int = 0): Int = function match {
    case tpd.Apply(inner, _)     => clause(inner, depth + 1)
    case tpd.TypeApply(inner, _) => clause(inner, depth)
    case _                       => depth
  }

  private def term(argument: tpd.Tree)(using Context): Term = Calls.peelInlined(argument) match {
    case tpd.Apply(function, arguments) if function.symbol.is(Flags.Given) =>
      Term.Instance(Calls.qualified(function.symbol), arguments.map(term(_)).toVector)
    case other => reference(other.symbol)
  }

  private def reference(symbol: Symbol)(using Context): Term =
    if (symbol.is(Flags.Param) && symbol.owner.is(Flags.Method))
      Term.Forward(Slot(symbol.owner, clauses(symbol.owner).flatten.indexOf(symbol)))
    else if (symbol.is(Flags.ParamAccessor) && symbol.owner.isClass) {
      val constructor = symbol.owner.primaryConstructor
      Term.Forward(Slot(constructor, clauses(constructor).flatten.indexWhere(_.name == symbol.name)))
    } else if (symbol.is(Flags.Given) || symbol.is(Flags.Module)) Term.Instance(Calls.qualified(symbol), Vector.empty)
    else Term.Open

  /** The instances reaching each dictionary parameter of the build. A parameter takes what its callers pass, and what
    * the callers of the methods it overrides pass; one that forwards a parameter of its caller takes what that one
    * does.
    */
  def parameters()(using Context): Vector[Parameter] = {
    val inputs = declared.view.flatMap { (method, parameters) =>
      val overridden = method.allOverriddenSymbols.toVector
      parameters.indices.filter(parameters(_).is(Flags.Given)).map { index =>
        Slot(method, index) -> (method +: overridden).flatMap(owner =>
          passed.getOrElse(Slot(owner, index), mutable.ArrayBuffer.empty)
        )
      }
    }.toMap
    val reached = mutable.Map.from(inputs.keys.map(_ -> Reaching(Set.empty, open = false)))
    var changed = true // scalafix:ok DisableSyntax.var
    while (changed) {
      changed = false
      inputs.foreach { (slot, terms) =>
        val next = if (terms.isEmpty) Reaching(Set.empty, open = true) else join(terms.map(follow(_, reached)))
        if (next != reached(slot)) {
          reached(slot) = next
          changed = true
        }
      }
    }
    inputs.keys.toVector.flatMap { slot =>
      Keys.of(slot.method).map { key =>
        val found = reached(slot)
        Parameter(
          key,
          slot.index,
          found.instances.toVector.sorted,
          found.open
        )
      }
    }.sortBy(parameter => (parameter.method, parameter.index))
  }

  private def join(reaches: Iterable[Reaching]): Reaching =
    Reaching(reaches.flatMap(_.instances).toSet, reaches.exists(_.open))

  private def follow(term: Term, reached: collection.Map[Slot, Reaching]): Reaching = term match {
    case Term.Open                      => Reaching(Set.empty, open = true)
    case Term.Forward(slot)             => reached.getOrElse(slot, Reaching(Set.empty, open = true))
    case Term.Instance(name, arguments) =>
      val parts = arguments.map(follow(_, reached))
      val named = parts.foldLeft(Vector(Vector.empty[String])) { (rendered, part) =>
        val options = part.instances.toVector.sorted
        for {
          prefix   <- rendered
          instance <- options
        } yield prefix :+ instance
      }
      Reaching(
        named.map(rendered => if (arguments.isEmpty) name else rendered.mkString(s"$name(", ", ", ")")).toSet,
        parts.exists(_.open)
      )
  }
}
