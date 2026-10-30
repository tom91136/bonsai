package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Integer literal constants that equal an expression of the siblings declared before them: a constant moved by a power
  * of two it is aligned to, or the sum or difference of two constants. Written as the expression, the constant follows
  * the ones it comes from. Every operand must share a word with the constant's name, a power of two is not a base, and
  * generated sources are skipped, since round numbers and independently numbered tables coincide.
  */
class DerivedConstants extends SyntacticRule("DerivedConstants") {
  private val Smallest = 0x1000L
  private val Operand  = 0x100L

  // generated tables number their entries independently, so their sums coincide
  override def fix(using document: SyntacticDocument): Patch =
    if (generated(document.input)) Patch.empty
    else document.tree.collect {
      case template: Template => scope(template.body.stats)
      case source: Source     => scope(source.stats)
      case pkg: Pkg           => scope(pkg.body.stats)
    }.asPatch

  private def generated(input: Input): Boolean = (input match {
    case file: Input.File        => file.path.toString
    case file: Input.VirtualFile => file.path
    case _                       => ""
  }).split("[/\\\\]").contains("generated")

  private def scope(stats: List[Stat]): Patch = {
    val constants = stats.collect { case definition: Defn.Val => definition }.flatMap { definition =>
      definition.pats match {
        case List(Pat.Var(name)) => Some(name.value -> definition)
        case _                   => None
      }
    }
    constants.foldLeft((Map.empty[String, Long], Patch.empty)) { case ((known, patches), (name, definition)) =>
      val found = Option.when(literal(definition.rhs))(evaluate(definition.rhs, known)).flatten.filter(_ >= Smallest)
        .flatMap(derivation(name, _, known, definition.rhs.isInstanceOf[Lit.Long])).map(expression =>
          lint(
            "derived-constant",
            s"$name is a literal equal to $expression; write the expression, so it follows the constants it comes from",
            definition.pos
          )
        )
      (evaluate(definition.rhs, known).fold(known)(known.updated(name, _)), patches + found.getOrElse(Patch.empty))
    }._2
  }

  private def literal(tree: Term): Boolean = tree match {
    case _: Lit.Int | _: Lit.Long => true
    case _                        => false
  }

  private def power(value: Long): Boolean = value > 0L && (value & (value - 1L)) == 0L

  private def hex(value: Long, long: Boolean): String =
    s"0x${java.lang.Long.toHexString(value)}${if (long) "L" else ""}"

  // The operands are named, since constants alike in value by chance are common among round numbers and magic numbers;
  // one that shares a word with an operand is meant to follow it, and a base is moved by a step it is aligned to.
  private def derivation(name: String, value: Long, siblings: Map[String, Long], long: Boolean): Option[String] = {
    val own   = words(name)
    val named = siblings.toVector.sortBy(_._1).collect {
      case (sibling, a) if a >= Operand && a != value && words(sibling).exists(own) => sibling -> a
    }
    val sums = for {
      ((left, a), index) <- named.zipWithIndex
      (right, b)         <- named.drop(index + 1) if !power(a) || !power(b)
      found              <- Seq(a + b -> s"$left + $right", a - b -> s"$left - $right", b - a -> s"$right - $left")
      if found._1 == value
    } yield found._2
    val moved = for {
      (base, a) <- named if !power(a)
      shift     <- 8 to 62
      step = 1L << shift
      if a % step == 0L && (a + step == value || a - step == value)
    } yield s"$base ${if (a < value) "+" else "-"} ${hex(step, long)}"
    (moved ++ sums).headOption
  }

  private val Words   = "[A-Z]+(?![a-z])|[A-Z]?[a-z]+".r
  private val Generic =
    Set("base", "limit", "size", "end", "start", "mask", "offset", "shift", "bits", "count", "min", "max")

  private def words(name: String): Set[String] =
    Words.findAllIn(name).map(_.toLowerCase(java.util.Locale.ROOT)).filter(word =>
      word.length > 2 && !Generic(word)
    ).toSet

  private def evaluate(tree: Term, known: Map[String, Long]): Option[Long] = tree match {
    case Lit.Int(value)                         => Some(value.toLong)
    case Lit.Long(value)                        => Some(value)
    case Term.Name(name)                        => known.get(name)
    case Term.ApplyUnary(Term.Name("-"), inner) => evaluate(inner, known).map(-_)
    case Term.ApplyInfix.After_4_6_0(left, Term.Name(operator), _, args) if args.values.sizeIs == 1 =>
      for {
        a <- evaluate(left, known)
        b <- evaluate(args.values.head, known)
        r <- operator match {
          case "+"  => Some(a + b)
          case "-"  => Some(a - b)
          case "*"  => Some(a * b)
          case "<<" => Some(a << b)
          case "|"  => Some(a | b)
          case _    => None
        }
      } yield r
    case _ => None
  }
}
