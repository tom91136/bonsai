package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Parameters of the methods of one type that disagree with their siblings: a type or default that differs in one
  * method from several others sharing the parameter's name.
  */
class ApiConsistency extends SyntacticRule("ApiConsistency") {
  // A parameter of a method, with its declared type as written.
  private type Use = (Defn.Def, Term.Param, String)

  private val MinimumAgreeing = 2

  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case owner: Defn.Object => parameters(owner.templ, owner.name.value)
    case owner: Defn.Class  => parameters(owner.templ, owner.name.value)
    case owner: Defn.Trait  => parameters(owner.templ, owner.name.value)
  }.asPatch

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
