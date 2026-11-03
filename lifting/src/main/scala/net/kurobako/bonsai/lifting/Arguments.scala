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
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.collections.Arg
import net.kurobako.bonsai.collections.Comparison
import net.kurobako.bonsai.collections.Source

/** The source text of trees. */
private[lifting] object Texts {
  def of(tree: tpd.Tree)(using Context): String = {
    val position = tree.sourcePos
    if (position.exists) {
      val content = scala.util.Try(position.source.content()).getOrElse(Array.empty[Char])
      if (position.end <= content.length) String(content, position.start, position.end - position.start)
      else tree.show
    } else tree.show
  }

  def real(tree: tpd.Tree): Boolean = tree.span.exists && tree.span.end > tree.span.start
}

/** An argument of a step: the facts the laws ask of a tree and its text as written. A function written in place, with
  * the parameter used at least once, keeps its body as the `template` whose pieces lie between the uses.
  */
private[lifting] final class Argument(
    val text: String,
    val pure: Boolean,
    override val identity: Boolean,
    override val projection: Option[Int],
    override val literal: Option[Any],
    override val comparison: Option[Comparison],
    override val membership: Option[String],
    template: Option[Vector[String]]
) extends Arg {
  def applied(value: String): String =
    if (identity) value
    else
      template.map(_.mkString(if (Argument.atomic(value)) value else s"($value)")).getOrElse(
        if (Argument.Reference.matches(text)) s"$text($value)" else s"($text)($value)"
      )
}

private[lifting] object Argument {
  private val Component = "_([0-9]+)".r
  private val Reference = """[A-Za-z_$][\w$]*(\.[A-Za-z_$][\w$]*)*""".r
  private val Fixed     =
    Set(IntTag, LongTag, StringTag, CharTag, BooleanTag)

  def apply(tree: tpd.Tree, purity: Purity, owner: Symbol)(using Context): Argument = {
    val bare       = Calls.peelInlined(tree)
    val definition = Calls.lambda(tree)
    val parameter  = definition.flatMap(Parameters.sole)
    val body       = definition.map(found => Calls.peelInlined(found.rhs))
    val pure       = definition match {
      case Some(found)                                  => purity.lambda(owner, found)
      case None if bare.symbol.exists && function(bare) =>
        Calls.identity(bare.symbol) || bare.symbol.is(Flags.Method) && purity.method(bare.symbol)
      case None => purity.value(bare)
    }
    val patterned = definition.flatMap(destructured)
    val identity  = (body, parameter) match {
      case _ if patterned.isDefined         => patterned.exists(kept)
      case (Some(expression), Some(symbol)) => identical(expression, symbol) || flattening(expression, symbol)
      case _                                => Calls.identity(bare.symbol)
    }
    val projection = (body, parameter) match {
      case _ if patterned.isDefined => patterned.flatMap(selected)
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
    new Argument(Texts.of(tree), pure, identity, projection, literal, comparison, membership, template(body, parameter))
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
        !assigns(expression) && !uses.exists(interpolated)
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
    Option.when(patterns.forall(PatternShapes.simple))(result.symbol).filter(_.exists).flatMap { symbol =>
      Some(patterns.indexWhere(binder(_).contains(symbol)) + 1).filter(_ > 0)
    }
  }

  /** Whether a pattern function rebuilds the tuple it matches from the components it binds. */
  private def kept(pattern: (List[tpd.Tree], tpd.Tree))(using Context): Boolean = {
    val (patterns, result) = pattern
    patterns.forall(PatternShapes.bound) &&
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

  private def collect(expression: tpd.Tree, symbol: Symbol)(using Context): List[tpd.Tree] = {
    val accumulator = new tpd.TreeAccumulator[List[tpd.Tree]] {
      def apply(found: List[tpd.Tree], tree: tpd.Tree)(using Context): List[tpd.Tree] = tree match {
        case _: tpd.Ident if tree.symbol == symbol => tree :: found
        case _                                     => foldOver(found, tree)
      }
    }
    accumulator(Nil, expression)
  }

  def free(tree: tpd.Tree, symbol: Symbol)(using Context): Boolean = collect(tree, symbol).isEmpty

  private def assigns(expression: tpd.Tree)(using Context): Boolean = {
    val accumulator = new tpd.TreeAccumulator[Boolean] {
      def apply(found: Boolean, tree: tpd.Tree)(using Context): Boolean = found ||
        (tree match {
          case _: tpd.Assign => true
          case _             => foldOver(found, tree)
        })
    }
    accumulator(false, expression)
  }

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

/** The patterns of generators and of the functions the compiler writes for them. */
private[lifting] object PatternShapes {
  private def ignored(pattern: tpd.Tree): Boolean = pattern match {
    case tpd.Ident(name) => name.toString == "_"
    case _               => false
  }

  def bound(pattern: tpd.Tree): Boolean = pattern match {
    case bind: tpd.Bind => ignored(bind.body)
    case _              => false
  }

  def simple(pattern: tpd.Tree): Boolean = ignored(pattern) || bound(pattern)

  /** Whether `pattern` matches every value of the type `scrutinee`: a wildcard, a binding, and a tuple or case class of
    * such, but not a type test or an extractor of a type with other cases.
    */
  def irrefutable(pattern: tpd.Tree, scrutinee: Type)(using Context): Boolean = pattern match {
    case tpd.Ident(name)                    => name.toString == "_"
    case tpd.Bind(_, inner)                 => irrefutable(inner, scrutinee)
    case tpd.Typed(inner, tpt)              => scrutinee <:< tpt.tpe && irrefutable(inner, scrutinee)
    case tpd.UnApply(function, _, patterns) =>
      val symbol = pattern.tpe.widen.classSymbol
      val tuple  = defn.isTupleClass(symbol)
      symbol.exists && function.symbol.name.toString == "unapply" && function.symbol.owner.companionClass == symbol &&
      (tuple || symbol.is(Flags.CaseClass) && function.symbol.is(Flags.Synthetic)) &&
      scrutinee.widen <:< pattern.tpe.widen && {
        val components = scrutinee.widen.dealias.argInfos
        if (tuple && components.sizeIs == patterns.size) patterns.zip(components).forall(irrefutable(_, _))
        else patterns.forall(simple)
      }
    case _ => false
  }
}

/** A call of a binary operator: `tree` is `left operator right`. */
final class Binary(val tree: tpd.Tree, val left: tpd.Tree, val operator: String, val right: tpd.Tree)

object Binary {
  def unapply(tree: tpd.Tree): Option[Binary] = tree match {
    case tpd.Apply(tpd.Select(left, name), List(right)) => Some(Binary(tree, left, name.toString, right))
    // From Scala 3.10 `until` and `to` on `Int` are extension methods of its companion, applied to the left in a clause
    // of their own.
    case tpd.Apply(tpd.Apply(tpd.Select(_, name), List(left)), List(right)) =>
      Some(Binary(tree, left, name.toString, right))
    case _ => None
  }
}

object Parameters {
  def sole(definition: tpd.DefDef)(using Context): Option[Symbol] = definition.termParamss.flatten match {
    case List(only) => Some(only.symbol)
    case _          => None
  }
}

/** An argument that is rewritten source text rather than a tree; it is never taken to be pure. */
private[lifting] final class TextArgument(val text: String) extends Arg {
  def pure: Boolean = false

  def applied(value: String): String = s"($text)($value)"
}
