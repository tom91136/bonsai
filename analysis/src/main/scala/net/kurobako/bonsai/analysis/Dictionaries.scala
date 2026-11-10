package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.MethodType
import net.kurobako.bonsai.lifting.*
import scala.annotation.targetName
import scala.collection.mutable

object Dictionaries {

  /** The instances that reach the dictionary parameter `index`, counted over all the term parameters of `method`, from
    * the calls of production code. `open` holds when some caller passes a value no instance names, or the parameter has
    * no caller to name one, as an exported method has none.
    */
  case class Parameter(
      method: String,
      index: Int,
      instances: Vector[String],
      open: Boolean,
      generic: Option[Generic]
  )

  /** Where a dictionary parameter that source can make concrete is declared: a method, or the constructor of a class,
    * with the type class of the parameter, which is over a type constructor. A method that returns the type class it
    * takes, such as a summoner, passes the dictionary on and is left out.
    */
  case class Generic(name: String, node: String, typeClass: String, constructor: Boolean, suppressed: Set[String])

  def findings(parameters: Vector[Parameter], nodes: Map[String, Node]): Vector[Finding] = for {
    parameter <- parameters
    generic   <- parameter.generic
    instance  <- parameter.instances.headOption
    if parameter.instances.size == 1 && !parameter.open
    found <- Finding.at(
      nodes,
      generic.node,
      Finding.Rule.FixedDictionary,
      s"${Finding.short(generic.name)} takes a ${Finding.simple(generic.typeClass)} dictionary that only " +
        s"${Finding.short(instance)} reaches in the build; make the ${if (generic.constructor) "class" else "method"} " +
        "concrete",
      level = Finding.Level.Info,
      suppressed = generic.suppressed
    )
  } yield found

  /** A parameter of a method, by the key of the method. */
  private[analysis] case class Slot(method: String, index: Int)

  private[analysis] enum Term {
    case Instance(name: String, arguments: Vector[Term])
    case Forward(slot: Slot)
    case Open
  }

  /** The slots a declared parameter takes the terms of: its own and those of the methods it overrides. */
  private[analysis] case class Declared(terms: Vector[Term], via: Vector[Slot])

  private[analysis] object Evidence {
    val empty: Evidence = Evidence(Map.empty, Map.empty)
  }

  /** What a run records of the dictionary parameters of the build: the parameters it declares with the terms its calls
    * pass them, and the terms its calls pass the parameters it did not read.
    */
  private[analysis] case class Evidence(declared: Map[Slot, Declared], apart: Map[Slot, Vector[Term]]) {
    @targetName("merge")
    def ++(other: Evidence): Evidence = Evidence(declared ++ other.declared, Maps.join(apart, other.apart)(_ ++ _))
  }

  private case class Reaching(instances: Set[String], open: Boolean)

  private val runs = java.util.concurrent.atomic.AtomicInteger()
}

/** The given arguments of the calls of the build, which a call writes out as trees, followed from the parameters they
  * fill to the parameters their callers forward them from, in this run and in the runs beside it.
  */
private[analysis] class Dictionaries(symbols: Symbols, foreign: Set[Output]) {
  import Dictionaries.*
  import symbols.*

  private val tag      = runs.incrementAndGet()
  private val declared = mutable.LinkedHashMap.empty[Symbol, Vector[Symbol]]
  private val passed   = mutable.Map.empty[Slot, mutable.ArrayBuffer[Term]]
  private val apart    = mutable.Map.empty[Slot, mutable.ArrayBuffer[Term]]

  private def clauses(method: Symbol)(using Context): List[List[Symbol]] =
    method.paramSymss.filter(clause => clause.isEmpty || clause.head.isTerm)

  private def slot(method: Symbol, index: Int)(using Context): Slot =
    Slot(
      if (method.exists && method.owner.isClass) Keys.unique(method).getOrElse(qualifiedName(method))
      else s"$tag#${method.id}",
      index
    )

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
        val target = if (foreign.nonEmpty && output(callee).exists(foreign)) apart else passed
        application.args.zipWithIndex.foreach((argument, position) =>
          target.getOrElseUpdate(slot(callee, offset + position), mutable.ArrayBuffer.empty) += term(argument)
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
      Term.Forward(slot(symbol.owner, clauses(symbol.owner).flatten.indexOf(symbol)))
    else if (symbol.is(Flags.ParamAccessor) && symbol.owner.isClass) {
      val constructor = symbol.owner.primaryConstructor
      Term.Forward(slot(constructor, clauses(constructor).flatten.indexWhere(_.name == symbol.name)))
    } else if (symbol.is(Flags.Given) || symbol.is(Flags.Module)) Term.Instance(Calls.qualified(symbol), Vector.empty)
    else Term.Open

  private def entries(using Context): Vector[(Slot, Symbol, Declared)] =
    declared.toVector.flatMap { (method, parameters) =>
      val owners = method +: method.allOverriddenSymbols.toVector
      parameters.indices.filter(parameters(_).is(Flags.Given)).map { index =>
        val via = owners.map(slot(_, index))
        (slot(method, index), method, Declared(via.flatMap(passed.get(_).fold(Vector.empty)(_.toVector)), via))
      }
    }

  /** What this run records for the runs beside it. */
  def evidence(using Context): Evidence =
    if (foreign.isEmpty) Evidence.empty
    else
      Evidence(
        entries.map((slot, _, found) => slot -> found).toMap,
        apart.view.mapValues(_.toVector).toMap
      )

  /** The instances reaching each dictionary parameter of the build. A parameter takes what its callers pass, and what
    * the callers of the methods it overrides pass; one that forwards a parameter of its caller takes what that one
    * does.
    */
  def parameters(others: Evidence)(using Context): Vector[Parameter] = {
    val local    = entries
    val slots    = others.declared ++ local.map((slot, _, found) => slot -> found)
    val supplied = Maps.join(others.apart, apart.view.mapValues(_.toVector).toMap)(_ ++ _)
    val inputs   = slots.view.mapValues(found => found.terms ++ found.via.flatMap(supplied.getOrElse(_, Vector.empty)))
      .toMap
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
    local.flatMap { (slot, method, _) =>
      Keys.of(method).map { key =>
        val found = reached(slot)
        Parameter(key, slot.index, found.instances.toVector.sorted, found.open, generic(method, slot.index))
      }
    }.sortBy(parameter => (parameter.method, parameter.index))
  }

  private def generic(method: Symbol, index: Int)(using Context): Option[Generic] = {
    val typeClass = declared(method)(index).info.widenDealias.typeSymbol
    for {
      node <- key(method)
      if !method.is(Flags.Deferred) && method.allOverriddenSymbols.isEmpty && overTypeConstructor(typeClass) &&
        method.info.finalResultType.widenDealias.typeSymbol != typeClass
    } yield Generic(
      qualifiedName(if (method.isConstructor) method.owner else method),
      node,
      Calls.qualified(typeClass),
      method.isConstructor,
      suppressions(method) ++ (if (method.isConstructor) suppressions(method.owner) else Set.empty)
    )
  }

  private def overTypeConstructor(cls: Symbol)(using Context): Boolean =
    cls.isClass && cls.typeParams.exists(_.paramInfo.hkTypeParams.nonEmpty)

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
