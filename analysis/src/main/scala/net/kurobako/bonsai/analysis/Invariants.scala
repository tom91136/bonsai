package net.kurobako.bonsai.analysis

import Algebra.*

/** Class invariants over the fields of a product, compared with the inhabitants of its type and its constructions. */
object Invariants {

  private val MaxAssignments = 4096
  private val MaxShownSites  = 3
  private val OptionalValues = Vector(Argument.Absent, Argument.Present)
  private val FlagValues     = Vector(Argument.False, Argument.True)

  /** Kleene evaluation: `None` when the arguments do not decide the predicate. */
  private def evaluate(predicate: Predicate, value: IndexedSeq[Argument]): Option[Boolean] = predicate match {
    case Predicate.Known(known)    => Some(known)
    case Predicate.Present(field)  => decide(value(field), Argument.Present, Argument.Absent)
    case Predicate.Flag(field)     => decide(value(field), Argument.True, Argument.False)
    case Predicate.Is(field, name) => value(field) match {
        case Argument.Tag(actual) => Some(actual == name)
        case _                    => None
      }
    case Predicate.Compare(field, operator, bound) => value(field) match {
        case Argument.Number(actual) => Some(compare(actual, operator, bound))
        case _                       => None
      }
    case Predicate.SameSize(_, _) | Predicate.Unknown => None
    case Predicate.Not(inner)                         => evaluate(inner, value).map(!_)
    case Predicate.And(left, right)                   =>
      (evaluate(left, value), evaluate(right, value)) match {
        case (Some(false), _) | (_, Some(false)) => Some(false)
        case (Some(true), Some(true))            => Some(true)
        case _                                   => None
      }
    // De Morgan's laws hold in Kleene logic too.
    case Predicate.Or(left, right) =>
      evaluate(Predicate.Not(Predicate.And(Predicate.Not(left), Predicate.Not(right))), value)
  }

  private def decide(argument: Argument, yes: Argument, no: Argument): Option[Boolean] =
    if (argument == yes) Some(true) else if (argument == no) Some(false) else None

  private def compare(actual: BigDecimal, operator: String, bound: BigDecimal): Boolean = operator match {
    case "<"  => actual < bound
    case "<=" => actual <= bound
    case ">"  => actual > bound
    case ">=" => actual >= bound
    case "==" => actual == bound
    case _    => actual != bound
  }

  private def leaves(predicate: Predicate): Vector[Predicate] = predicate match {
    case Predicate.Not(inner)       => leaves(inner)
    case Predicate.And(left, right) => leaves(left) ++ leaves(right)
    case Predicate.Or(left, right)  => leaves(left) ++ leaves(right)
    case leaf                       => Vector(leaf)
  }

  private def values(kind: Kind): Vector[Argument] = kind match {
    case Kind.Optional   => OptionalValues
    case Kind.Flag       => FlagValues
    case Kind.Tag(cases) => cases.map(Argument.Tag(_))
    case _               => Vector.empty
  }

  /** A class invariant or method precondition, with the words its findings use. */
  private case class Subject(product: Algebra.Product, name: String, condition: String, uses: String)

  def findings(model: Algebra.Model, nodes: Map[String, Node]): Vector[Finding] = {
    val subjects =
      model.products.map(product => Subject(product, product.name.split('.').last, "invariant", "constructions")) ++
        model.preconditions.map(product =>
          Subject(product, Finding.short(product.name), "precondition", "calls")
        )
    subjects.flatMap { subject =>
      val key   = subject.product.node
      val sites = model.sites.getOrElse(subject.product.name, Vector.empty)
      (states(subject) ++ parallel(subject)).flatMap((detail, rule) => Finding.at(nodes, key, rule, detail)) ++
        violations(subject, sites).flatMap((detail, failing) =>
          Finding.at(nodes, key, Finding.Rule.InvariantViolation, detail, onlyTests = Algebra.onlyTests(failing))
        ) ++
        bypasses(subject, sites).flatMap((detail, outside) =>
          Finding.at(nodes, key, Finding.Rule.BypassedFactory, detail, onlyTests = Algebra.onlyTests(outside))
        )
    }
  }

  /** States the type can represent but the invariant rejects, with the sum the remaining states describe. */
  private def states(subject: Subject): Vector[(String, Finding.Rule)] = {
    val Subject(product, name, condition, _) = subject
    val domain                               = leaves(product.invariant).collect {
      case Predicate.Present(field) => field
      case Predicate.Flag(field)    => field
      case Predicate.Is(field, _)   => field
    }.distinct.sorted.map(index => (index, product.fields(index), values(product.fields(index).kind)))
      .filter(_._3.nonEmpty)
    if (domain.isEmpty || domain.map(_._3.size).product > MaxAssignments) Vector.empty
    else {
      val assignments = domain.foldLeft(Vector(Map.empty[Int, Argument])) { case (partial, (index, _, choices)) =>
        for {
          assignment <- partial
          value      <- choices
        } yield assignment.updated(index, value)
      }
      val valid = assignments.filter(assignment =>
        evaluate(
          product.invariant,
          Vector.tabulate(product.fields.size)(index => assignment.getOrElse(index, Argument.Unknown))
        ) != Some(false)
      )
      val excluded = assignments.size - valid.size
      if (excluded == 0 || valid.isEmpty) Vector.empty
      else {
        val constant  = domain.filter((index, _, _) => valid.map(_(index)).distinct.size == 1)
        val redundant = domain.filterNot(constant.toSet).filter { (index, field, _) =>
          field.kind != Kind.Optional && valid.groupBy(_.removed(index)).values.forall(_.size == 1)
        }
        val forced = constant.map((index, field, _) =>
          s"$name: the $condition forces ${field.name} to ${valid.head(index)}; it carries no information" ->
            Finding.Rule.InvariantConstant
        )
        val determined = redundant.map { (index, field, _) =>
          val others = domain.collect { case (other, named, _) if other != index => named.name }.mkString(", ")
          s"$name: under the $condition ${field.name} is determined by ($others)" -> Finding.Rule.InvariantRedundant
        }
        val sum = Option.when(constant.isEmpty && redundant.isEmpty && valid.size > 1) {
          val representable = Polynomial.product(domain.map((_, field, choices) =>
            if (field.kind == Kind.Optional) field.shape else Polynomial.constant(choices.size)
          ))
          val validShape = Polynomial.sum(valid.map(assignment =>
            Polynomial.product(domain.collect {
              case (index, field, _) if assignment(index) == Argument.Present => field.shape - Polynomial.One
            })
          ))
          val cases = valid.map { assignment =>
            val shown = domain.flatMap((index, field, _) =>
              assignment(index) match {
                case Argument.Present | Argument.True => Some(field.name)
                case Argument.Tag(tag)                => Some(s"${field.name}=$tag")
                case _                                => None
              }
            )
            if (shown.isEmpty) "none" else shown.mkString("+")
          }
          s"$name: the $condition rejects $excluded of ${assignments.size} states of " +
            s"(${domain.map(_._2.name).mkString(", ")}); $representable represented, $validShape valid; " +
            s"as a sum: ${cases.mkString(" | ")}" -> Finding.Rule.InvariantSum
        }
        forced ++ determined ++ sum
      }
    }
  }

  /** Constructions or calls whose known arguments definitely violate the invariant, with those sites. */
  private def violations(subject: Subject, sites: Vector[Algebra.Site]) = {
    val failing = sites.filter(site => evaluate(subject.product.invariant, site.arguments) == Some(false))
    Option.when(failing.nonEmpty)(
      s"${subject.name}: ${failing.size} ${subject.uses} violate the ${subject.condition}: " +
        Finding.listed(failing.map(_.location), MaxShownSites) -> failing
    ).toVector
  }

  private def parallel(subject: Subject) = leaves(subject.product.invariant).collect {
    case Predicate.SameSize(left, right) =>
      val (first, second) = (subject.product.fields(left).name, subject.product.fields(right).name)
      s"${subject.name}: $first and $second are kept the same size; one collection of pairs would make the " +
        "invariant structural" -> Finding.Rule.ParallelCollections
  }

  /** Constructions outside the product and its companion while a validating factory exists, with those sites. */
  private def bypasses(subject: Subject, sites: Vector[Algebra.Site]) = subject.product.access match {
    case Access.Factory(factories) =>
      val outside = sites.filterNot(_.own)
      Option.when(outside.nonEmpty)(
        s"${subject.name}: ${outside.size} of ${sites.size} constructions bypass ${factories.mkString(", ")}: " +
          Finding.listed(outside.map(_.location), MaxShownSites) -> outside
      ).toVector
    case _ => Vector.empty
  }
}
