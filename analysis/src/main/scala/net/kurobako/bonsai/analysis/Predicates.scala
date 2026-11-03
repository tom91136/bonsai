package net.kurobako.bonsai.analysis

import Algebra.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.ConstantType
import net.kurobako.bonsai.lifting.*

/** Reads `require` invariants and argument values, interpreting only standard library and language operations. */
object Predicates {
  private val Flipped = Map("<" -> ">", "<=" -> ">=", ">" -> "<", ">=" -> "<=", "==" -> "==", "!=" -> "!=")

  /** The conjunction of `conditions`, over the parameters or fields of `owner`. */
  def required(owner: Symbol, conditions: List[tpd.Tree], fields: Vector[Algebra.Field])(using Context): Predicate = {
    val reader = Reader(owner, fields)
    conditions.map(reader.read(_)).reduceOption(_.and(_)).getOrElse(Predicate.Known(true))
  }

  /** The arguments of a call with each varargs sequence spread into its elements. */
  def spread(arguments: List[tpd.Tree])(using Context): List[tpd.Tree] =
    arguments.flatMap(Calls.peel(_) match {
      case tpd.SeqLiteral(elements, _) => elements
      case other                       => List(other)
    })

  /** The value an argument tree is known to have, if it is a literal, `None`, `Some`, or an enum case. */
  def argument(tree: tpd.Tree)(using Context): Option[Argument] = {
    val bare = Calls.peel(tree)
    val kind = bare.tpe.widen.typeSymbol
    bare match {
      case tpd.Literal(Constant(flag: Boolean)) => Some(if (flag) Argument.True else Argument.False)
      case tpd.Literal(Constant(text: String))  => Some(Argument.Word(Wording.Text, text))
      case _ if bare.symbol == defn.NoneModule || kind == defn.NoneModule.moduleClass => Some(Argument.Absent)
      case _ if kind == defn.SomeClass                                                => Some(Argument.Present)
      case _ => number(bare).map(Argument.Number(_)).orElse(singleton(bare).map(Argument.Word(Wording.Tag, _)))
    }
  }

  /** The value of a numeric literal or constant reference. */
  private def number(tree: tpd.Tree)(using Context): Option[BigDecimal] = {
    val constant = tree match {
      case tpd.Literal(value) => Some(value)
      case _                  => tree.tpe.widenTermRefExpr match {
          case ConstantType(value) => Some(value)
          case _                   => None
        }
    }
    constant.collect {
      case value if value.isNumeric && value.tag != Constants.CharTag && value.doubleValue.isFinite =>
        BigDecimal(value.stringValue)
    }
  }

  /** The name of an enum case or case object a tree refers to. */
  private def singleton(tree: tpd.Tree)(using Context): Option[String] = {
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

    def read(tree: tpd.Tree): Predicate = Calls.peel(tree) match {
      case tpd.Literal(Constant(value: Boolean))                                   => Predicate.Known(value)
      case tpd.Select(inner, name) if name.toString == "unary_!"                   => Predicate.Not(read(inner))
      case tpd.Apply(tpd.Select(left, name), List(right)) if name.toString == "&&" =>
        read(left).and(read(right))
      case tpd.Apply(tpd.Select(left, name), List(right)) if name.toString == "||" =>
        read(left).or(read(right))
      case Binary(binary) if Flipped.contains(binary.operator) => relation(binary)
      case tpd.Select(OptionalField(index), name)              =>
        name.toString match {
          case "isDefined" | "nonEmpty" => Predicate.Holds(index, Probe.Present)
          case "isEmpty"                => Predicate.Not(Predicate.Holds(index, Probe.Present))
          case _                        => Predicate.Unknown
        }
      case Field(index) if fields(index).kind == Algebra.Kind.Flag => Predicate.Holds(index, Probe.Flag)
      case _                                                       => Predicate.Unknown
    }

    private def relation(binary: Binary): Predicate = {
      import binary.*
      val equality = operator == "==" || operator == "!="
      if (equality && left.tpe.widen.isRef(defn.BooleanClass)) {
        val (first, second) = (read(left), read(right))
        val same            = first.and(second).or(Predicate.Not(first).and(Predicate.Not(second)))
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
              Predicate.Not(Predicate.Holds(index, Probe.Present))
            case Algebra.Kind.Tag(_) if equality => singleton(value).fold(Predicate.Unknown)(Predicate.Is(index, _))
            case Algebra.Kind.Flag if equality   =>
              argument(value) match {
                case Some(Argument.True)  => Predicate.Holds(index, Probe.Flag)
                case Some(Argument.False) => Predicate.Not(Predicate.Holds(index, Probe.Flag))
                case _                    => Predicate.Unknown
              }
            case Algebra.Kind.Number => number(value).fold(Predicate.Unknown)(Predicate.Compare(index, operator, _))
            case _                   => Predicate.Unknown
          }
        case _ => Predicate.Unknown
      }
  }
}
