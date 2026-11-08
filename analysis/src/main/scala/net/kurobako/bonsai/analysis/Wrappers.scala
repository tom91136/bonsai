package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable
import scala.util.control.NonFatal

private[analysis] object Wrappers {

  /** What a wrapper passes on: one of its parameters, a value it fixes, varargs it leaves empty, or the result of
    * another call that is itself made of these.
    */
  enum Spec {
    case Parameter(index: Int, tpe: Type, repeated: Boolean)
    case Constant(value: String)
    case Empty
    case Nested(target: Symbol, specs: List[Spec])

    /** Whether the wrapper fixes a value, here or in a call it makes. */
    def fixes: Boolean = this match {
      case Constant(_)      => true
      case Nested(_, specs) => specs.exists(_.fixes)
      case _                => false
    }

    /** Whether the wrapper passes its parameter `index` on, here or in a call it makes. */
    def passes(index: Int): Boolean = this match {
      case Parameter(at, _, _) => at == index
      case Nested(_, specs)    => specs.exists(_.passes(index))
      case _                   => false
    }
  }

  /** A method of a library whose body is a call to `target` that passes its parameters on and fixes the rest. */
  case class Wrapper(symbol: Symbol, target: Symbol, specs: List[Spec], name: String, label: String)

  /** Calls in `method` to `target` that `wrapper` stands for. */
  case class Bypass(method: String, node: String, line: Int, target: String, wrapper: String, calls: Int)

  def findings(bypasses: Vector[Bypass], nodes: Map[String, Node]): Vector[Finding] =
    bypasses.flatMap { bypass =>
      val times = if (bypass.calls == 1) "" else s" ${bypass.calls} times"
      Finding.at(
        nodes,
        bypass.node,
        Finding.Rule.BypassedWrapper,
        s"${Finding.short(bypass.method)} calls ${bypass.target}$times, which ${bypass.wrapper} wraps exactly",
        Finding.Level.Info,
        line = bypass.line
      )
    }
}

/** Finds raw calls that a method of a library the build imports wraps, by the body of that method as the classpath
  * holds it.
  */
private[analysis] class Wrappers(symbols: Symbols, lines: Lines) {
  import Wrappers.*
  import symbols.*

  private val libraries = mutable.Map.empty[Symbol, Vector[Wrapper]]
  private val imported  = mutable.Map.empty[Symbol, Vector[Wrapper]]
  private val found     = mutable.ArrayBuffer.empty[Bypass]

  def bypasses: Vector[Bypass] = found.toVector

  /** Makes the wrappers a compilation unit imports the ones its methods are checked against. */
  def enter(unit: tpd.Tree)(using Context): Unit = {
    imported.clear()
    unit.foreachSubTree {
      case imports: tpd.Import =>
        val names = imports.selectors.filterNot(_.isWildcard).map(_.name.toString).toSet
        val all   = imports.selectors.exists(_.isWildcard)
        library(imports.expr.symbol).filter(wrapper => all || names(wrapper.name)).foreach(wrapper =>
          imported(wrapper.target) = imported.getOrElse(wrapper.target, Vector.empty) :+ wrapper
        )
      case _ => ()
    }
  }

  /** Records the calls in the body of a method of the build that a wrapper in scope stands for. */
  def method(method: Symbol, body: tpd.Tree, info: Node)(using Context): Unit =
    if (imported.nonEmpty && !info.test && !info.generated) {
      val calls = mutable.LinkedHashMap.empty[Wrapper, (Int, Int)]
      Scanner(calls).traverse(body)
      calls.foreach { (wrapper, summary) =>
        found += Bypass(
          qualifiedName(method),
          info.name,
          summary._2,
          Finding.short(qualifiedName(wrapper.target)),
          wrapper.label,
          summary._1
        )
      }
    }

  private class Scanner(calls: mutable.LinkedHashMap[Wrapper, (Int, Int)]) extends tpd.TreeTraverser {
    def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case inlined: tpd.Inlined   => inlined.bindings.foreach(traverse(_))
      case application: tpd.Apply =>
        val (target, arguments) = flatten(application)
        imported.getOrElse(target, Vector.empty).find(matches(_, arguments)).foreach { wrapper =>
          val (count, line) = calls.getOrElse(wrapper, (0, lines.line(application.sourcePos)))
          calls(wrapper) = (count + 1, line)
        }
        arguments.foreach(traverse(_))
      case other => traverseChildren(other)
    }
  }

  private def matches(wrapper: Wrapper, arguments: List[tpd.Tree])(using Context): Boolean =
    agrees(wrapper.specs, arguments)

  private def agrees(specs: List[Spec], arguments: List[tpd.Tree])(using Context): Boolean =
    specs.sizeIs == arguments.size && specs.lazyZip(arguments).forall {
      case (Spec.Constant(value), argument)          => constant(argument).contains(value)
      case (Spec.Parameter(_, _, true), _)           => true
      case (Spec.Parameter(_, tpe, false), argument) => argument.tpe.widen <:< tpe
      case (Spec.Empty, argument)                    => Calls.peel(argument) match {
          case tpd.SeqLiteral(Nil, _) => true
          case _                      => false
        }
      case (Spec.Nested(target, inner), argument) => Calls.peel(argument) match {
          case call: tpd.Apply =>
            val (symbol, nested) = flatten(call)
            symbol == target && agrees(inner, nested)
          case _ => false
        }
    }

  @annotation.tailrec
  private def head(tree: tpd.Tree): tpd.Tree = tree match {
    case tpd.Apply(function, _)     => head(function)
    case tpd.TypeApply(function, _) => head(function)
    case other                      => other
  }

  private def static(qualifier: tpd.Tree)(using Context): Boolean =
    qualifier.symbol.is(Flags.Module) || qualifier.symbol.is(Flags.Package)

  private def flatten(application: tpd.Apply)(using Context): (Symbol, List[tpd.Tree]) = {
    val receiver = head(application) match {
      case tpd.Select(qualifier, _) if !static(qualifier) && !qualifier.isInstanceOf[tpd.New] => List(qualifier)
      case _                                                                                  => Nil
    }
    (application.symbol, receiver ++ Calls.arguments(application))
  }

  private def constant(argument: tpd.Tree)(using Context): Option[String] = Calls.peel(argument) match {
    case tpd.Literal(value) => Some(value.stringValue)
    case reference: (tpd.Select | tpd.Ident)
        if reference.symbol.exists &&
          (reference.symbol.is(Flags.Module) || reference.symbol.isStatic && !reference.symbol.is(Flags.Method)) =>
      Some(qualifiedName(reference.symbol))
    case _ => None
  }

  private def library(expr: Symbol)(using Context): Vector[Wrapper] =
    libraries.getOrElseUpdate(
      expr,
      if (!expr.exists || !(expr.is(Flags.Module) || expr.is(Flags.Package))) Vector.empty
      else members(expr).flatMap(wrapper(_)).toVector
    )

  private def members(expr: Symbol)(using Context): List[Symbol] = {
    val declared = expr.moduleClass.info.decls.toList
    if (expr.is(Flags.Package))
      declared.flatMap(member =>
        if (member.is(Flags.Module) && member.moduleClass.isTopLevelDefinitionsObject)
          member.moduleClass.info.decls.toList
        else Nil
      )
    else declared
  }

  private def wrapper(member: Symbol)(using Context): Option[Wrapper] =
    Option.when(
      member.is(Flags.Method) && member.isOneOf(Flags.ExtensionMethod | Flags.Inline) &&
        !member.isOneOf(Flags.Private | Flags.Synthetic) && output(member).isEmpty &&
        !member.paramSymss.exists(_.exists(_.isType))
    )(unpickled(member)).collect {
      case definition: tpd.DefDef if !definition.rhs.isEmpty =>
        val parameters = definition.termParamss.flatten.map(_.symbol)
        Calls.peelInlined(definition.rhs) match {
          case application: tpd.Apply =>
            val (target, arguments) = flatten(application)
            val specs               = arguments.map(argument => spec(argument, parameters))
            val fixing              = specs.flatten.exists(_.fixes)
            Option.when(
              target.exists && target != member && (fixing || target.owner != member.owner) &&
                specs.forall(_.isDefined) && parameters.indices.forall(index => specs.flatten.exists(_.passes(index)))
            )(Wrapper(member, target, specs.flatten, member.name.toString, Finding.short(qualifiedName(member))))
          case _ => None
        }
    }.flatten

  private def spec(argument: tpd.Tree, parameters: List[Symbol])(using Context): Option[Spec] = {
    val peeled = Calls.peel(argument)
    val index  = parameters.indexOf(peeled.symbol)
    if (index >= 0) Some(Spec.Parameter(index, parameters(index).info.widen, parameters(index).info.isRepeatedParam))
    else
      peeled match {
        case tpd.SeqLiteral(Nil, _) => Some(Spec.Empty)
        case call: tpd.Apply        =>
          val (target, arguments) = flatten(call)
          val nested              = arguments.map(spec(_, parameters))
          Option.when(target.exists && nested.forall(_.isDefined))(Spec.Nested(target, nested.flatten))
        case _ => constant(peeled).map(Spec.Constant(_))
      }
  }

  // A library body is unpickled on first read, and a reference in it to a class the classpath lacks can fail that.
  private def unpickled(symbol: Symbol)(using Context): tpd.Tree =
    try
      symbol.defTree match {
        case tree: tpd.ValOrDefDef => tree.rhs: Unit; tree
        case tree                  => tree
      }
    catch { case NonFatal(_) => tpd.EmptyTree }
}
