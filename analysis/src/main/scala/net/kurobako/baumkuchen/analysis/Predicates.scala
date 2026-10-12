package net.kurobako.baumkuchen.analysis

import Algebra.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.ConstantType

/** Reads `require` invariants and argument values, interpreting only standard library and language operations. */
object Predicates {
  private val Flipped = Map("<" -> ">", "<=" -> ">=", ">" -> "<", ">=" -> "<=", "==" -> "==", "!=" -> "!=")

  /** The conditions of the `require` calls among `statements`. */
  def conditions(statements: List[tpd.Tree])(using Context): List[tpd.Tree] = statements.collect {
    case tpd.Apply(function, condition :: _)
        if function.symbol.name.toString == "require" && function.symbol.owner == defn.ScalaPredefModule.moduleClass =>
      condition
  }

  /** The conjunction of `conditions`, over the parameters or fields of `owner`. */
  def required(owner: Symbol, conditions: List[tpd.Tree], fields: Vector[Algebra.Field])(using Context): Predicate = {
    val reader = Reader(owner, fields)
    conditions.map(reader.read(_)).reduceOption(Predicate.And(_, _)).getOrElse(Predicate.Known(true))
  }

  /** The tree without ascriptions, argument names and empty inline or block wrappers. */
  @annotation.tailrec
  def unwrap(tree: tpd.Tree): tpd.Tree = tree match {
    case tpd.NamedArg(_, argument)      => unwrap(argument)
    case tpd.Typed(expression, _)       => unwrap(expression)
    case tpd.Inlined(_, Nil, expansion) => unwrap(expansion)
    case tpd.Block(Nil, expression)     => unwrap(expression)
    case other                          => other
  }

  /** The value an argument tree is known to have, if it is a literal, `None`, `Some`, or an enum case. */
  def argument(tree: tpd.Tree)(using Context): Option[Argument] = {
    val bare = unwrap(tree)
    val kind = bare.tpe.widen.typeSymbol
    bare match {
      case tpd.Literal(Constant(flag: Boolean)) => Some(if (flag) Argument.True else Argument.False)
      case tpd.Literal(Constant(text: String))  => Some(Argument.Text(text))
      case _ if bare.symbol == defn.NoneModule || kind == defn.NoneModule.moduleClass => Some(Argument.Absent)
      case _ if kind == defn.SomeClass                                                => Some(Argument.Present)
      case _ => number(bare).map(Argument.Number(_)).orElse(singleton(bare).map(Argument.Tag(_)))
    }
  }

  /** The value of a numeric literal or constant reference. */
  def number(tree: tpd.Tree)(using Context): Option[BigDecimal] = {
    val constant = tree match {
      case tpd.Literal(value) => Some(value)
      case _                  => tree.tpe.widenTermRefExpr match {
          case ConstantType(value) => Some(value)
          case _                   => None
        }
    }
    constant.filter(value => value.isNumeric && value.tag != Constants.CharTag && value.doubleValue.isFinite)
      .map(value => BigDecimal(value.stringValue))
  }

  /** The name of an enum case or case object a tree refers to. */
  def singleton(tree: tpd.Tree)(using Context): Option[String] = {
    val symbol = tree.symbol
    Option.when(
      symbol.exists && (symbol.isAllOf(Flags.EnumValue) || symbol.is(Flags.Module) && symbol.moduleClass.is(Flags.Case))
    )(symbol.name.toString)
  }

  private class Reader(owner: Symbol, fields: Vector[Algebra.Field])(using Context) {
    private val indices = fields.map(_.name).zipWithIndex.toMap

    private object Field {
      def unapply(tree: tpd.Tree): Option[Int] = tree match {
        case tpd.Ident(_) | tpd.Select(tpd.This(_), _)
            if tree.symbol.isOneOf(Flags.Param | Flags.ParamAccessor) && tree.symbol.owner == owner =>
          indices.get(tree.symbol.name.toString)
        case _ => None
      }
    }

    private object OptionalField {
      def unapply(tree: tpd.Tree): Option[Int] = Field.unapply(tree).filter(fields(_).kind == Algebra.Kind.Optional)
    }

    private object Size {
      @annotation.tailrec
      def unapply(tree: tpd.Tree): Option[Int] = tree match {
        case tpd.Apply(inner, Nil)                                                              => unapply(inner)
        case tpd.Select(receiver, name) if name.toString == "size" || name.toString == "length" =>
          Field.unapply(receiver)
        case _ => None
      }
    }

    def read(tree: tpd.Tree): Predicate = unwrap(tree) match {
      case tpd.Literal(Constant(value: Boolean))                                   => Predicate.Known(value)
      case tpd.Select(inner, name) if name.toString == "unary_!"                   => Predicate.Not(read(inner))
      case tpd.Apply(tpd.Select(left, name), List(right)) if name.toString == "&&" =>
        Predicate.And(read(left), read(right))
      case tpd.Apply(tpd.Select(left, name), List(right)) if name.toString == "||" =>
        Predicate.Or(read(left), read(right))
      case tpd.Apply(tpd.Select(left, name), List(right)) if Flipped.contains(name.toString) =>
        relation(left, name.toString, right)
      case tpd.Select(OptionalField(index), name) =>
        name.toString match {
          case "isDefined" | "nonEmpty" => Predicate.Present(index)
          case "isEmpty"                => Predicate.Not(Predicate.Present(index))
          case _                        => Predicate.Unknown
        }
      case Field(index) if fields(index).kind == Algebra.Kind.Flag => Predicate.Flag(index)
      case _                                                       => Predicate.Unknown
    }

    private def relation(left: tpd.Tree, operator: String, right: tpd.Tree): Predicate = {
      val equality = operator == "==" || operator == "!="
      if (equality && left.tpe.widen.isRef(defn.BooleanClass)) {
        val (first, second) = (read(left), read(right))
        val same            = Predicate.Or(
          Predicate.And(first, second),
          Predicate.And(Predicate.Not(first), Predicate.Not(second))
        )
        if (operator == "==") same else Predicate.Not(same)
      } else
        Some(compare(left, operator, right, equality)).filter(_ != Predicate.Unknown)
          .getOrElse(compare(right, Flipped(operator), left, equality)) match {
          // Non-numeric relations are read as equalities and negated here; numeric ones carry their operator.
          case known @ (Predicate.Unknown | Predicate.Compare(_, _, _)) => known
          case known if operator == "!="                                => Predicate.Not(known)
          case known                                                    => known
        }
    }

    private def compare(subject: tpd.Tree, operator: String, value: tpd.Tree, equality: Boolean): Predicate =
      (subject, value) match {
        case (Size(left), Size(right)) if equality => Predicate.SameSize(left, right)
        case (Field(index), _)                     =>
          fields(index).kind match {
            case Algebra.Kind.Optional if equality && argument(value).contains(Argument.Absent) =>
              Predicate.Not(Predicate.Present(index))
            case Algebra.Kind.Tag(_) if equality => singleton(value).fold(Predicate.Unknown)(Predicate.Is(index, _))
            case Algebra.Kind.Flag if equality   =>
              argument(value) match {
                case Some(Argument.True)  => Predicate.Flag(index)
                case Some(Argument.False) => Predicate.Not(Predicate.Flag(index))
                case _                    => Predicate.Unknown
              }
            case Algebra.Kind.Number => number(value).fold(Predicate.Unknown)(Predicate.Compare(index, operator, _))
            case _                   => Predicate.Unknown
          }
        case _ => Predicate.Unknown
      }
  }
}
