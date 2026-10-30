package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Givens and `using` parameters that are named but only ever resolved implicitly. Only names no other file can see are
  * checked: private and local givens, and `using` parameters of concrete methods.
  */
class AnonymousGiven extends SemanticRule("AnonymousGiven") {
  override def fix(using document: SemanticDocument): Patch = {
    val references = document.tree.collect { case name: Name if !defining(name) => name -> name.symbol }
    val referenced = references.map(_._2).toSet
    // SemanticDB leaves some references unresolved, such as names inside macro quotes, so those match by name.
    val unresolved = references.collect { case (name, symbol) if symbol.isNone => name.value }.toSet
    def unreferenced(name: Name, symbols: Symbol*): Boolean = !unresolved(name.value) && !symbols.exists(referenced)
    def unused(parameter: Term.Param): Boolean              =
      unreferenced(parameter.name, parameter.name.symbol :: field(parameter).toList*)
    document.tree.collect {
      case alias: Defn.GivenAlias
          if hidden(alias.mods, alias.name, alias) && unreferenced(alias.name, alias.name.symbol) && !clashes(alias) =>
        lint(
          "anonymous-given",
          s"${alias.name.value} is never referenced by name; make the given anonymous",
          alias.name.pos
        )
      case instance: Defn.Given
          if hidden(instance.mods, instance.name, instance) && unreferenced(instance.name, instance.name.symbol) &&
            !clashes(instance) =>
        lint(
          "anonymous-given",
          s"${instance.name.value} is never referenced by name; make the given anonymous",
          instance.name.pos
        )
      case parameter: Term.Param if using(parameter) && unused(parameter) =>
        val tpe = parameter.decltpe.fold("")(_.syntax)
        // A clause is either all named or all anonymous.
        val clause = parameter.parent.collect { case clause: Term.ParamClause => clause.values }.getOrElse(Nil)
        val advice = if (clause.forall(unused(_))) s"write `using $tpe`"
        else s"give it a clause of its own, `(using $tpe)`"
        lint("anonymous-using", s"${parameter.name.value} is never referenced by name; $advice", parameter.name.pos)
    }.asPatch
  }

  private def defining(name: Name): Boolean = name.parent.exists {
    case member: Member         => member.name eq name
    case alias: Defn.GivenAlias => alias.name eq name
    case instance: Defn.Given   => instance.name eq name
    case _                      => false
  }

  private def named(name: Name): Boolean = !name.isInstanceOf[Name.Anonymous] && name.value.nonEmpty

  // Members of a template or package are visible to other files unless private.
  private def hidden(mods: List[Mod], name: Name, definition: Defn): Boolean =
    named(name) && !definitions.has[Mod.Override](mods) &&
      (definitions.has[Mod.Private](mods) || !definition.parent.exists {
        case _: Template.Body | _: Pkg.Body | _: Source => true
        case _                                          => false
      })

  // The compiler names an anonymous given after its type, so a sibling of the same type would collide.
  private def clashes(definition: Defn): Boolean = {
    def tpe(tree: Tree): Option[String] = tree match {
      case alias: Defn.GivenAlias => Some(alias.decltpe.structure)
      case instance: Defn.Given   => instance.templ.inits.headOption.map(_.tpe.structure)
      case _                      => None
    }
    val own = tpe(definition)
    definition.parent.exists(_.children.exists(sibling =>
      (sibling ne definition) && own.isDefined && tpe(sibling) == own
    ))
  }

  private def using(parameter: Term.Param): Boolean = named(parameter.name) &&
    (parameter.parent match {
      case Some(clause: Term.ParamClause) => clause.mod.exists(_.isInstanceOf[Mod.Using]) && clause.parent.flatMap {
          case group: Member.ParamClauseGroup => group.parent
          case other                          => Some(other)
        }.exists {
          case _: Decl.Def | _: Decl.Given => false
          case _                           => true
        } && !parameter.mods.exists(mod => mod.isInstanceOf[Mod.ValParam] || mod.isInstanceOf[Mod.VarParam])
      case _ => false
    })

  // A class body refers to a constructor parameter through the field it becomes.
  private def field(parameter: Term.Param)(using SemanticDocument): Option[Symbol] = {
    val owner = parameter.name.symbol.owner
    Option.when(owner.value.endsWith("`<init>`()."))(Symbol(owner.owner.value + parameter.name.value + "."))
  }
}
