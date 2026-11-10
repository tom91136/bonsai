package net.kurobako.bonsai.lifting

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.BooleanTag
import dotty.tools.dotc.core.Constants.CharTag
import dotty.tools.dotc.core.Constants.IntTag
import dotty.tools.dotc.core.Constants.LongTag
import dotty.tools.dotc.core.Constants.StringTag
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.collections.*
import scala.annotation.tailrec

private[lifting] object Argument {
  private val Component = "_([0-9]+)".r
  private val Reference = """[A-Za-z_$][\w$]*(\.[A-Za-z_$][\w$]*)*""".r
  private val Fixed     =
    Set(IntTag, LongTag, StringTag, CharTag, BooleanTag)

  def apply(tree: tpd.Tree, purity: Purity, owner: Symbol)(using Context): Argument = {
    val bare       = Calls.peelInlined(tree)
    val definition = Calls.lambda(tree)
    val parameter  = definition.flatMap(Parameters.sole(_))
    val body       = definition.map(found => Calls.peelInlined(found.rhs))
    val pure       = definition match {
      case Some(found)                                  => purity.lambda(owner, found)
      case None if bare.symbol.exists && function(bare) =>
        Calls.identity(bare.symbol) || bare.symbol.is(Flags.Method) && purity.method(bare.symbol)
      case None => purity.value(bare)
    }
    val patterned = definition.flatMap(destructured(_))
    val identity  = (body, parameter) match {
      case _ if patterned.isDefined         => patterned.exists(kept(_))
      case (Some(expression), Some(symbol)) => identical(expression, symbol) || flattening(expression, symbol)
      case _                                => Calls.identity(bare.symbol)
    }
    val projection = (body, parameter) match {
      case _ if patterned.isDefined => patterned.flatMap(selected(_))
      case (Some(tpd.Select(qualifier, name)), Some(symbol)) if Calls.peelInlined(qualifier).symbol == symbol =>
        name.toString match {
          case Component(index) if qualifier.tpe.widen.derivesFrom(defn.ProductClass) => Some(index.toInt)
          case _                                                                      => None
        }
      case _ => None
    }
    val literal = bare match {
      case tpd.Literal(constant) => Some(constant.value).filter(_ != ())
      case _                     => None
    }
    val comparison = (body, parameter) match {
      case (Some(tpd.Apply(tpd.Select(left, name), List(right))), Some(symbol))
          if (name.toString == "==" || name.toString == "!=") && Calls.peelInlined(left).symbol == symbol =>
        val value = Calls.peelInlined(right)
        val fixed = value match {
          case tpd.Literal(constant) => Fixed(constant.tag)
          case _                     => stable(value) && free(value, symbol)
        }
        Option.when(fixed)(Comparison(name.toString == "==", Texts.of(value), assignable(value, symbol)))
      case _ => None
    }
    val membership = (body, parameter) match {
      case (Some(tpd.Apply(tpd.Select(collection, name), List(argument))), Some(symbol))
          if name.toString == "contains" && Calls.peelInlined(argument).symbol == symbol && immutableSet(collection) =>
        Some(Texts.of(collection))
      case _ => None
    }
    val matching = definition.flatMap(matched(_))
    val inspects = matching.map(shape(_)).orElse(for {
      expression <- body
      symbol     <- parameter
      found      <- read(expression, symbol).orElse(tested(expression, symbol))
    } yield found)
    val destructures = matching.map(shape(_)).orElse(for {
      expression <- body
      symbol     <- parameter
      found      <- cast(expression, symbol)
    } yield found)
    val selection = (body, parameter) match {
      case (Some(expression), Some(symbol)) => selects(expression, symbol)
      case _                                => false
    }
    val result = body.filterNot(converted(_)).map(_.tpe)
      .orElse(Option.when(body.isEmpty && function(bare))(bare.tpe.widen.dealias.argInfos.lastOption).flatten)
    new Argument(
      Texts.of(tree),
      pure,
      identity,
      projection,
      literal,
      comparison,
      membership,
      inspects,
      destructures,
      selection,
      bare.tpe.widenDealias.isRef(defn.NothingClass),
      template(body, parameter),
      Option.unless(converted(bare))(typeName(bare.tpe)),
      result.map(typeName(_))
    )
  }

  /** The pattern of a function written as a block of cases, which the compiler matches its one parameter against. */
  private def matched(definition: tpd.DefDef)(using Context): Option[tpd.Tree] =
    Parameters.sole(definition).filter(_.name.toString.startsWith("x$")).flatMap { parameter =>
      Calls.peelInlined(definition.rhs) match {
        case tpd.Match(selector, List(tpd.CaseDef(pattern, _, _))) if Calls.peelInlined(selector).symbol == parameter =>
          Some(pattern)
        case _ => None
      }
    }

  /** A pattern with every name in it replaced by `_`. */
  private def shape(pattern: tpd.Tree)(using Context): String = pattern match {
    case tpd.Bind(_, inner)                      => shape(inner)
    case tpd.Ident(name) if name.toString == "_" => "_"
    case tpd.Typed(inner, tpt)                   => s"${shape(inner)}: ${tpt.tpe.show}"
    case tpd.Alternative(alternatives)           => alternatives.map(shape(_)).mkString(" | ")
    case tpd.UnApply(_, _, patterns)             =>
      val symbol = pattern.tpe.widen.classSymbol
      val parts  = patterns.map(shape(_)).mkString(", ")
      if (defn.isTupleClass(symbol)) s"($parts)" else s"${symbol.name}($parts)"
    case other => Texts.of(other)
  }

  /** The tuple a function reads only components of, as a pattern of its arity. */
  private def read(body: tpd.Tree, parameter: Symbol)(using Context): Option[String] = {
    val tuple = parameter.info.widenDealias
    val reads = Components(parameter)(0, body)
    Option.when(defn.isTupleClass(tuple.classSymbol) && reads > 0 && reads == collect(body, parameter).size)(
      List.fill(tuple.argInfos.size)("_").mkString("(", ", ", ")")
    )
  }

  /** The type a function tests its parameter against, in the first of the conditions it conjoins. */
  private def tested(body: tpd.Tree, parameter: Symbol)(using Context): Option[String] = {
    @tailrec def first(tree: tpd.Tree): tpd.Tree = Calls.peelInlined(tree) match {
      case tpd.Apply(tpd.Select(left, name), List(_)) if name.toString == "&&" => first(left)
      case other                                                               => other
    }
    first(body) match {
      case tpd.TypeApply(tpd.Select(target, name), List(tpt))
          if name.toString == "isInstanceOf" && Calls.peelInlined(target).symbol == parameter =>
        Some(s"_: ${tpt.tpe.show}")
      case _ => None
    }
  }

  /** The type a function casts its parameter to, when that is all it does with it. */
  private def cast(body: tpd.Tree, parameter: Symbol)(using Context): Option[String] = {
    val uses  = collect(body, parameter).size
    val casts = Casts(parameter)(Nil, body)
    Option.when(uses > 0 && casts.sizeIs == uses && casts.distinct.sizeIs == 1)(s"_: ${casts.head}")
  }

  @tailrec
  private def selects(body: tpd.Tree, parameter: Symbol)(using Context): Boolean = Calls.peelInlined(body) match {
    case tpd.Apply(select: tpd.Select, Nil) => selects(select, parameter)
    case tpd.Select(qualifier, _)           =>
      Calls.peelInlined(qualifier) match {
        case reference: tpd.Ident => reference.symbol == parameter
        case inner                => selects(inner, parameter)
      }
    case _ => false
  }

  private class Components(symbol: Symbol) extends tpd.TreeAccumulator[Int] {
    def apply(found: Int, tree: tpd.Tree)(using Context): Int = tree match {
      case tpd.Select(qualifier, name)
          if Component.matches(name.toString) && Calls.peelInlined(qualifier).symbol == symbol =>
        found + 1
      case _ => foldOver(found, tree)
    }
  }

  private class Casts(symbol: Symbol) extends tpd.TreeAccumulator[List[String]] {
    def apply(found: List[String], tree: tpd.Tree)(using Context): List[String] = tree match {
      case tpd.TypeApply(tpd.Select(target, name), List(tpt))
          if name.toString == "asInstanceOf" && Calls.peelInlined(target).symbol == symbol =>
        tpt.tpe.show :: found
      case _ => foldOver(found, tree)
    }
  }

  /** Whether the compiler wrote a conversion around `tree` to the type it expected, so that its type is not the type of
    * the expression as written.
    */
  @tailrec
  private def converted(tree: tpd.Tree): Boolean = tree match {
    case tpd.Block(_, expression) => converted(expression)
    case tpd.Apply(function, _)   => function.span.isSynthetic
    case _                        => false
  }

  private def typeName(tpe: Type)(using Context): String = tpe.widen.dealias match {
    case AppliedType(constructor, arguments) =>
      s"${typeName(constructor)}[${arguments.map(typeName(_)).mkString(", ")}]"
    case other if other.typeSymbol.exists => other.typeSymbol.fullName.toString
    case other                            => other.show
  }

  /** Whether `value` is accepted where the parameter's type is: it conforms, or it is an integer literal that a wider
    * floating or long type takes.
    */
  private def assignable(value: tpd.Tree, parameter: Symbol)(using Context): Boolean = {
    val element = parameter.info.widen
    value.tpe.widen <:< element ||
    (value match {
      case tpd.Literal(constant) =>
        constant.tag == IntTag &&
        (element.isRef(defn.LongClass) || element.isRef(defn.FloatClass) ||
          element.isRef(defn.DoubleClass))
      case _ => false
    })
  }

  /** The body of a function with the parameter used, as the text between the uses, when the uses can be replaced. */
  private def template(body: Option[tpd.Tree], parameter: Option[Symbol])(using Context): Option[Vector[String]] =
    for {
      expression <- body
      symbol     <- parameter
      uses = collect(expression, symbol)
      if uses.nonEmpty && Texts.real(expression) && uses.forall(use => Texts.real(use) && inside(use, expression)) &&
        !assigns(expression) && !uses.exists(interpolated(_))
      ordered = uses.sortBy(_.span.start)
      if ordered.zip(ordered.tail).forall((before, after) => before.span.end <= after.span.start)
    } yield {
      val text   = Texts.of(expression)
      val start  = expression.span.start
      val bounds = ordered.map(use => (use.span.start - start, use.span.end - start))
      val cuts   = (0 +: bounds.flatMap((from, until) => Vector(from, until)) :+ text.length).grouped(2).toVector
      cuts.map(piece => text.substring(piece(0), piece(1)))
    }

  private def interpolated(use: tpd.Tree)(using Context): Boolean = {
    val position = use.sourcePos
    position.source.content().lift(position.start - 1).contains('$')
  }

  private def function(tree: tpd.Tree)(using Context): Boolean = defn.isFunctionType(tree.tpe.widen)

  private def identical(body: tpd.Tree, parameter: Symbol)(using Context): Boolean = body match {
    case tpd.Ident(_)                        => body.symbol == parameter
    case tpd.Apply(function, List(argument)) => Calls.identity(function.symbol) &&
      Calls.peelInlined(argument).symbol == parameter
    case _ => false
  }

  /** The tuple pattern and the result of a function the compiler wrote for a generator with a pattern, which matches
    * its one parameter against the pattern.
    */
  private def destructured(definition: tpd.DefDef)(using Context): Option[(List[tpd.Tree], tpd.Tree)] =
    Parameters.sole(definition).filter(_.name.toString.startsWith("x$")).flatMap { _ =>
      Calls.peelInlined(definition.rhs) match {
        case tpd.Match(_, List(tpd.CaseDef(pattern, tpd.EmptyTree, result))) =>
          unwrapped(pattern) match {
            case tpd.UnApply(_, _, patterns) if defn.isTupleClass(pattern.tpe.widen.classSymbol) =>
              Some((patterns, Calls.peelInlined(result)))
            case _ => None
          }
        case _ => None
      }
    }

  @tailrec
  private def unwrapped(pattern: tpd.Tree): tpd.Tree = pattern match {
    case tpd.Bind(_, inner) => unwrapped(inner)
    case other              => other
  }

  private def binder(pattern: tpd.Tree)(using Context): Option[Symbol] = pattern match {
    case bind: tpd.Bind if PatternShapes.bound(bind) => Some(bind.symbol)
    case _                                           => None
  }

  /** The number of the tuple component a pattern function returns, when every component is bound or ignored. */
  private def selected(pattern: (List[tpd.Tree], tpd.Tree))(using Context): Option[Int] = {
    val (patterns, result) = pattern
    Option.when(patterns.forall(PatternShapes.simple(_)))(result.symbol).filter(_.exists).flatMap { symbol =>
      Some(patterns.indexWhere(binder(_).contains(symbol)) + 1).filter(_ > 0)
    }
  }

  /** Whether a pattern function rebuilds the tuple it matches from the components it binds. */
  private def kept(pattern: (List[tpd.Tree], tpd.Tree))(using Context): Boolean = {
    val (patterns, result) = pattern
    patterns.forall(PatternShapes.bound(_)) &&
    (result match {
      case tpd.Apply(function, arguments) if Calls.tuple(function.symbol) =>
        arguments.sizeIs == patterns.size &&
        arguments.zip(patterns).forall((argument, pattern) =>
          binder(pattern).exists(symbol => Calls.peelInlined(argument).symbol == symbol)
        )
      case _ => false
    })
  }

  /** Whether the body of a function of a nested generator is the identity map of its parameter, which the compiler
    * writes for `for { row <- rows; element <- row } yield element`.
    */
  private[lifting] def flattening(body: tpd.Tree, parameter: Symbol)(using Context): Boolean = body match {
    case tpd.Apply(function, List(mapper)) =>
      (function match {
        case tpd.TypeApply(inner, _) => inner
        case other                   => other
      }) match {
        case select @ tpd.Select(qualifier, name) =>
          name.toString == "map" && select.span.isSynthetic && Calls.peelInlined(qualifier).symbol == parameter &&
          Calls.lambda(mapper).exists(definition =>
            Parameters.sole(definition).exists(symbol => identical(Calls.peelInlined(definition.rhs), symbol))
          )
        case _ => false
      }
    case _ => false
  }

  /** Whether `text` stays one operand when put in the place of a name. */
  private def atomic(text: String): Boolean = {
    val flat = Source.outline(text)
    flat.nonEmpty && flat.forall(c => c.isLetterOrDigit || c == '_' || c == '$' || c == '.' || c == '`')
  }

  private class Uses(symbol: Symbol) extends tpd.TreeAccumulator[List[tpd.Tree]] {
    def apply(found: List[tpd.Tree], tree: tpd.Tree)(using Context): List[tpd.Tree] = tree match {
      case _: tpd.Ident if tree.symbol == symbol => tree :: found
      case _                                     => foldOver(found, tree)
    }
  }

  private def collect(expression: tpd.Tree, symbol: Symbol)(using Context): List[tpd.Tree] =
    Uses(symbol)(Nil, expression)

  def free(tree: tpd.Tree, symbol: Symbol)(using Context): Boolean = collect(tree, symbol).isEmpty

  private object Assignments extends tpd.TreeAccumulator[Boolean] {
    def apply(found: Boolean, tree: tpd.Tree)(using Context): Boolean = found ||
      (tree match {
        case _: tpd.Assign => true
        case _             => foldOver(found, tree)
      })
  }

  private def assigns(expression: tpd.Tree)(using Context): Boolean = Assignments(false, expression)

  private def inside(use: tpd.Tree, expression: tpd.Tree): Boolean =
    use.span.start >= expression.span.start && use.span.end <= expression.span.end

  /** A reference that evaluates the same on every call and has no effect: a value, an object or `this`. */
  private def stable(tree: tpd.Tree)(using Context): Boolean = tree match {
    case _: tpd.This              => true
    case _: tpd.Ident             => Purity.Syntactic.value(tree.symbol)
    case tpd.Select(qualifier, _) => Purity.Syntactic.value(tree.symbol) && stable(Calls.peelInlined(qualifier))
    case _                        => false
  }

  private def immutableSet(tree: tpd.Tree)(using Context): Boolean =
    tree.tpe.widen.dealias.classSymbol == Classes.required("scala.collection.immutable.Set")
}

/** An argument of a step: the facts the laws ask of a tree and its text as written. A function written in place, with
  * the parameter used at least once, keeps its body as the `template` whose pieces lie between the uses.
  */
private[lifting] class Argument(
    val text: String,
    val pure: Boolean,
    override val identity: Boolean,
    override val projection: Option[Int],
    override val literal: Option[Any],
    override val comparison: Option[Comparison],
    override val membership: Option[String],
    override val inspects: Option[String],
    override val destructures: Option[String],
    override val selection: Boolean,
    override val diverges: Boolean,
    template: Option[Vector[String]],
    override val valueType: Option[String],
    override val resultType: Option[String]
) extends Arg {
  def applied(value: String): String =
    if (identity) value
    else
      template.map(_.mkString(if (Argument.atomic(value)) value else s"($value)")).getOrElse(
        if (Argument.Reference.matches(text)) s"$text($value)" else s"($text)($value)"
      )
}
