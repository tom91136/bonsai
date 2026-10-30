package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Tables, lookups and lists that restate the names or constructor arguments of an enum defined in the same file. Their
  * keys and results are facts the enum already holds, so they follow the enum when a case changes. Only forms that
  * cover every singleton case are reported: a partial table is not a copy of the enum, and deriving it would add or
  * drop entries.
  */
class RestatedLookup extends SyntacticRule("RestatedLookup") {
  override def fix(using document: SyntacticDocument): Patch = {
    val tree  = document.tree
    val index = Index(
      tree.collect { case definition: Defn.Enum => definition }.groupBy(_.name.value).collect {
        case (name, List(definition)) => name -> enumeration(definition)
      },
      tree.collect { case importing: Import => importing.importers }.flatten
    )
    tree.collect {
      case map @ Term.Apply.After_4_6_0(Term.Name("Map") | Term.Select(Term.Name("Map"), Term.Name("apply")), args) =>
        table(index, map, args.values)
      case Term.Select(list @ Term.Apply.After_4_6_0(Term.Name("Vector" | "Seq" | "List"), args), Term.Name("toMap")) =>
        table(index, list, args.values)
      case lookup: Term.Match =>
        lookupMatch(index, lookup)
      case definition: Defn.Def =>
        val params = definition.paramClauseGroups.flatMap(_.paramClauses).flatMap(_.values)
        reverse(index, definition, params, definition.body)
      case definition: Defn.Val =>
        reverse(index, definition, Nil, definition.rhs)
      case list @ Term.Apply.After_4_6_0(Term.Name(kind @ ("Set" | "Vector" | "Seq" | "List")), args) =>
        enumerated(index, list, kind, args.values)
    }.flatten.asPatch
  }

  private final case class Variant(name: String, singleton: Boolean, args: List[Term])

  private final case class Enumeration(
      name: String,
      params: List[String],
      variants: List[Variant],
      overridesToString: Boolean
  ) {
    def singletons: List[String] = variants.collect { case variant if variant.singleton => variant.name }
  }

  private final case class Index(enums: Map[String, Enumeration], importers: List[Importer])

  private final case class Entry(key: Any, owner: Enumeration, variant: Variant)

  private def enumeration(definition: Defn.Enum): Enumeration = {
    val name     = definition.name.value
    val variants = definition.templ.body.stats.flatMap {
      case variant: Defn.EnumCase =>
        val args = variant.inits.find(_.tpe.syntax == name).flatMap(_.argClauses.headOption).map(_.values)
        List(Variant(variant.name.value, variant.ctor.paramClauses.isEmpty, args.getOrElse(Nil)))
      case variants: Defn.RepeatedEnumCase => variants.cases.map(variant => Variant(variant.value, true, Nil))
      case _                               => Nil
    }
    Enumeration(
      name,
      definition.ctor.paramClauses.toList.flatMap(_.values).map(_.name.value),
      variants,
      definition.templ.body.stats.exists {
        case method: Defn.Def => method.name.value == "toString"
        case _                => false
      }
    )
  }

  private def table(index: Index, tree: Tree, values: List[Term]): Option[Patch] = {
    val pairs = values.flatMap(pair)
    Option.when(pairs.sizeIs == values.size)(pairs).flatMap(resolved(index, _)).flatMap(restated(_, names = true))
      .map { case (owner, accessor) => lint("restated-map", mapMessage(owner, accessor), tree.pos) }
  }

  private def lookupMatch(index: Index, lookup: Term.Match): Option[Patch] = {
    val cases = lookup.casesBlock.cases match {
      case init :+ Case(_: Pat.Wildcard, None, _) => init
      case all                                    => all
    }
    val arms = cases.flatMap {
      case Case(pattern, None, body) =>
        val (wrapped, result) = wrap(body)
        for {
          key              <- literal(pattern)
          (owner, variant) <- reference(index, result)
        } yield wrapped -> Entry(key, owner, variant)
      case _ => None
    }
    Option.when(arms.sizeIs == cases.size && arms.map(_._1).distinct.sizeIs == 1)(arms.map(_._2))
      .flatMap(restated(_, names = true)).map { case (owner, accessor) =>
        lint("restated-match", lookupMessage(owner, accessor), lookup.pos)
      }
  }

  private def reverse(index: Index, tree: Tree, params: List[Term.Param], body: Term): Option[Patch] = body match {
    case lookup: Term.Match =>
      val scrutinee = lookup.expr match {
        case _: Term.This     => enclosingEnum(index, tree)
        case Term.Name(param) =>
          params.find(_.name.value == param).flatMap(_.decltpe).collect { case Type.Name(name) => name }
            .flatMap(index.enums.get)
        case _ => None
      }
      scrutinee.filter(_.variants.forall(_.singleton)).flatMap { owner =>
        val arms = lookup.casesBlock.cases.flatMap {
          case Case(pattern, None, result) =>
            for {
              key               <- literal(result)
              (target, variant) <- reference(index, pattern)
              if target.name == owner.name
            } yield Entry(key, target, variant)
          case _ => None
        }
        Option.when(arms.sizeIs == lookup.casesBlock.cases.size)(arms).flatMap(restated(_, names = false)).map {
          case (target, accessor) => lint("restated-reverse", reverseMessage(target, accessor), lookup.pos)
        }
      }
    case _ => None
  }

  private def enumerated(index: Index, tree: Tree, kind: String, values: List[Term]): Option[Patch] = {
    val members = values.flatMap(reference(index, _))
    val names   = members.map(_._2.name)
    members.headOption.map(_._1).filter { owner =>
      members.sizeIs == values.size && values.sizeIs >= 3 && members.forall(_._1.name == owner.name) &&
      owner.variants.forall(_.singleton) && names.sorted == owner.singletons.sorted &&
      (kind == "Set" || names == owner.singletons)
    }.map(owner => lint("restated-enumeration", enumerationMessage(owner, kind), tree.pos))
  }

  private def enclosingEnum(index: Index, definition: Tree): Option[Enumeration] =
    definitions.ancestors(definition).collectFirst { case template: Template => template }.flatMap(_.parent).collect {
      case owner: Defn.Enum => owner.name.value
    }.flatMap(index.enums.get)

  private def pair(tree: Term): Option[(Term, Term)] = tree match {
    case Term.ApplyInfix.After_4_6_0(key, Term.Name("->"), _, args) if args.values.sizeIs == 1 =>
      Some(key -> args.values.head)
    case Term.Tuple(List(key, value)) => Some(key -> value)
    case _                            => None
  }

  private def resolved(index: Index, pairs: List[(Term, Term)]): Option[List[Entry]] = {
    val found = pairs.flatMap { case (key, value) =>
      for {
        k                <- literal(key)
        (owner, variant) <- reference(index, value)
      } yield Entry(k, owner, variant)
    }
    Option.when(found.sizeIs == pairs.size)(found)
  }

  private def wrap(tree: Term): (Boolean, Term) = tree match {
    case Term.Apply.After_4_6_0(Term.Name("Some"), args) if args.values.sizeIs == 1 => true  -> args.values.head
    case _                                                                          => false -> tree
  }

  private def literal(tree: Tree): Option[Any] = tree match {
    case lit: Lit                                         => Some(lit.value)
    case Term.ApplyUnary(Term.Name("-"), Lit.Int(value))  => Some(-value)
    case Term.ApplyUnary(Term.Name("-"), Lit.Long(value)) => Some(-value)
    case _                                                => None
  }

  // A bare case name only resolves where the enum's companion or an import brings it into scope: elsewhere it may be a
  // local binding, and a pattern binding would match anything.
  private def reference(index: Index, tree: Tree): Option[(Enumeration, Variant)] = tree match {
    case Term.Select(Term.Name(name), Term.Name(variant)) =>
      index.enums.get(name).flatMap(owner => owner.variants.find(_.name == variant).map(owner -> _))
    case Term.Name(variant) =>
      index.enums.values.filter(owner =>
        owner.variants.exists(_.name == variant) && visible(index, tree, owner.name, variant)
      ).toList match {
        case List(owner) => owner.variants.find(_.name == variant).map(owner -> _)
        case _           => None
      }
    case _ => None
  }

  private def visible(index: Index, tree: Tree, owner: String, variant: String): Boolean =
    definitions.ancestors(tree).exists {
      case companion: Defn.Object => companion.name.value == owner
      case _                      => false
    } || index.importers.exists(importer =>
      named(importer.ref, owner) && importer.importees.exists {
        case _: Importee.Wildcard    => true
        case Importee.Name(imported) => imported.value == variant
        case _                       => false
      }
    )

  private def named(ref: Term.Ref, name: String): Boolean = ref match {
    case Term.Name(value)                 => value == name
    case Term.Select(_, Term.Name(value)) => value == name
    case _                                => false
  }

  private def restated(entries: List[Entry], names: Boolean): Option[(Enumeration, Option[Int])] =
    entries.headOption.map(_.owner).filter { owner =>
      entries.sizeIs >= 3 && entries.forall(_.owner.name == owner.name) &&
      entries.map(_.variant.name).sorted == owner.singletons.sorted
    }.flatMap { owner =>
      val accessors = owner.params.indices.toList.map(Some(_)) ++ Option.when(names && !owner.overridesToString)(None)
      accessors.find(accessor => entries.forall(restates(_, accessor))).map(owner -> _)
    }

  private def restates(entry: Entry, accessor: Option[Int]): Boolean = accessor match {
    case None        => entry.key == entry.variant.name
    case Some(index) => entry.variant.args.lift(index).flatMap(literal).contains(entry.key)
  }

  private def member(owner: Enumeration, accessor: Option[Int]): String = accessor.fold("toString")(owner.params(_))

  private def restatement(noun: String, owner: Enumeration, accessor: Option[Int]): String = accessor match {
    case None        => s"$noun restates the case names of ${owner.name}"
    case Some(index) => s"$noun restates the ${owner.params(index)} of ${owner.name}"
  }

  private def advice(owner: Enumeration, expression: String): String =
    if (owner.variants.forall(_.singleton)) s"derive it with ${owner.name}.values$expression"
    else s"derive it from a list of the singleton cases, as ${owner.name} has no values"

  private def mapMessage(owner: Enumeration, accessor: Option[Int]): String = {
    val derivation = advice(owner, s".iterator.map(v => v.${member(owner, accessor)} -> v).toMap")
    s"${restatement("table", owner, accessor)}; $derivation, so it follows the enum"
  }

  private def lookupMessage(owner: Enumeration, accessor: Option[Int]): String = {
    val derivation = advice(owner, s".find(v => v.${member(owner, accessor)} == key)")
    s"${restatement("lookup", owner, accessor)}; $derivation, so it follows the enum"
  }

  private def reverseMessage(owner: Enumeration, accessor: Option[Int]): String = {
    val field = member(owner, accessor)
    s"${restatement("match", owner, accessor)}; return the field $field, so the results follow the enum"
  }

  private def enumerationMessage(owner: Enumeration, kind: String): String =
    s"list restates every case of ${owner.name}; use ${owner.name}.values.to$kind, so it follows the enum"
}
