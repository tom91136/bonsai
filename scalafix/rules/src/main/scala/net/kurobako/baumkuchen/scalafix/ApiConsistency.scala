package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Members of one type that disagree with their siblings: a value of the kind a registry of the type collects that the
  * registry leaves out, and a parameter whose type or default differs in one method from several others sharing its
  * name.
  */
class ApiConsistency extends SemanticRule("ApiConsistency") {
  // A parameter of a method, with its declared type as written.
  private type Use = (Defn.Def, Term.Param, String)

  private val Collections     = Set("Vector", "Seq", "List", "Set", "IndexedSeq")
  private val MinimumRegistry = 2
  private val MinimumAgreeing = 2

  override def fix(using document: SemanticDocument): Patch = {
    val references =
      document.tree.collect { case name: Term.Name => name.symbol }.groupMapReduce(identity)(_ => 1)(_ + _)
    document.tree.collect {
      case owner: Defn.Object => registries(owner.templ, references) + parameters(owner.templ, owner.name.value)
      case owner: Defn.Class  => registries(owner.templ, references) + parameters(owner.templ, owner.name.value)
      case owner: Defn.Trait  => registries(owner.templ, references) + parameters(owner.templ, owner.name.value)
    }.asPatch
  }

  /** Values listed by a collection literal of their siblings, all of one type constructor, form a registry of it. A
    * public sibling of that type nothing else in the file refers to is likely missing from it.
    */
  private def registries(template: Template, references: Map[Symbol, Int])(using SemanticDocument): Patch = {
    val values = definitions.members(template).collect {
      case value @ Defn.Val(mods, List(Pat.Var(name)), _, _) => (value, mods, name)
    }
    val siblings = values.map(_._3.symbol).toSet
    val kinds    = values.flatMap((_, _, name) => definitions.valueType(name.symbol).map(name.symbol -> _)).toMap
    val listed   = values.flatMap((value, _, _) =>
      value.rhs match {
        case application: Term.Apply if collection(application.fun) =>
          val members = application.argClause.values.collect { case name: Term.Name => name.symbol }
          Option.when(
            members.size >= MinimumRegistry && members.size == application.argClause.values.size &&
              members.forall(member => siblings(member) && kinds.contains(member)) &&
              members.map(kinds).distinct.size == 1
          )(kinds(members.head) -> members)
        case _ => None
      }
    ).groupMapReduce(_._1)(_._2.toSet)(_ ++ _)
    values.collect {
      case (_, mods, name)
          if definitions.public(mods) && kinds.get(name.symbol).exists(listed.contains) &&
            !listed(kinds(name.symbol))(name.symbol) && references.getOrElse(name.symbol, 0) <= 1 =>
        lint(
          "registry-drift",
          s"${name.value} is of the kind the collections of its siblings register, but none lists it; add it to one, " +
            "or derive the registry from the definitions",
          name.pos
        )
    }.asPatch
  }

  @annotation.tailrec
  private def collection(function: Term)(using SemanticDocument): Boolean = function match {
    case Term.Name(name)                        => Collections(name) && function.symbol.value.startsWith("scala/")
    case application: Term.ApplyType            => collection(application.fun)
    case Term.Select(inner, Term.Name("apply")) => collection(inner)
    case _                                      => false
  }

  private def common[K](variants: Map[K, List[Use]]): Option[(K, List[Use])] =
    variants.maxByOption(_._2.size).filter(_._2.size >= MinimumAgreeing)

  /** Parameters of the public, non-overloaded, non-overriding methods of a type that, under one name, take a type or a
    * default that only one method uses while several others agree on another.
    */
  private def parameters(template: Template, owner: String): Patch = {
    val methods = definitions.members(template).collect {
      case method: Defn.Def if definitions.public(method.mods) && !definitions.has[Mod.Override](method.mods) => method
    }
    val unique     = methods.groupBy(_.name.value).collect { case (_, List(method)) => method }.toList
    val parameters = unique.flatMap(method =>
      definitions.parameters(method).flatMap(parameter =>
        parameter.decltpe.map(tpe => (method, parameter, tpe.syntax.replaceAll("\\s+", " ")))
      )
    ).groupBy(_._2.name.value)
    parameters.values.flatMap { uses =>
      val types = uses.groupBy(_._3)
      common(types).toList.flatMap { (tpe, agreeing) =>
        val outliers = types.collect {
          case (other, List((_, parameter, _))) if other == s"Option[$tpe]" || tpe == s"Option[$other]" =>
            lint(
              "inconsistent-parameter",
              s"${parameter.name.value}: $other here, but $tpe in ${agreeing.size} other methods of $owner; encode " +
                "absence one way",
              parameter.pos
            )
        }
        val defaults  = agreeing.groupBy(_._2.default.map(_.syntax))
        val differing = common(defaults).flatMap(_._1).toList.flatMap(default =>
          defaults.collect {
            case (Some(other), List((_, parameter, _))) if other != default =>
              lint(
                "inconsistent-parameter",
                s"${parameter.name.value} defaults to $other here, but $default in other methods of $owner; use one " +
                  "default",
                parameter.pos
              )
          }
        )
        outliers ++ differing
      }
    }.asPatch
  }
}
