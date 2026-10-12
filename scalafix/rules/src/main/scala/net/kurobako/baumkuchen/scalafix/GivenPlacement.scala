package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Public givens defined where the implicit scope cannot find them, givens of types too common to mean one thing, and
  * implicit conversions outside the companions they convert between. Private givens never leave their owner.
  */
class GivenPlacement extends SemanticRule("GivenPlacement") {
  private val ambient = Set(
    "scala/Int#",
    "scala/Long#",
    "scala/Double#",
    "scala/Boolean#",
    "scala/Predef.String#",
    "java/lang/String#",
    "scala/Unit#"
  )

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case alias: Defn.GivenAlias if definitions.public(alias.mods)  => check(alias, alias.decltpe)
    case instance: Defn.Given if definitions.public(instance.mods) =>
      instance.templ.inits.headOption.map(init => check(instance, init.tpe)).asPatch
  }.asPatch

  // Anonymous givens have no symbol in SemanticDB, so the owner comes from the enclosing definition.
  @annotation.tailrec
  private def owner(tree: Tree)(using SemanticDocument): Option[Symbol] = tree.parent match {
    case Some(enclosing @ (_: Template | _: Template.Body | _: Pkg.Body)) => owner(enclosing)
    case Some(enclosing: Defn.Object)                                     => Some(enclosing.name.symbol)
    case Some(_: Pkg | _: Source)                                         => Some(Symbol.None)
    case _                                                                => None
  }

  private def check(tree: Tree, tpe: Type)(using SemanticDocument): Patch = owner(tree).map { owner =>
    val constructor = definitions.symbol(tpe)
    val arguments   = tpe match {
      case application: Type.Apply => application.argClause.values.map(definitions.symbol(_))
      case _                       => Nil
    }
    val companions = (constructor :: arguments).map(type_ => Symbol(type_.value.stripSuffix("#") + "."))
    if (ambient(constructor.value) || definitions.function(tpe))
      lint(
        "ambient-given",
        "a given of a common type is picked up by unrelated code; give it a specific type",
        tree.pos
      )
    else if (constructor.value == "scala/Conversion#" && !companions.contains(owner))
      lint("conversion-placement", "define implicit conversions in a companion of the types they convert", tree.pos)
    else if (arguments.nonEmpty && !companions.contains(owner))
      lint(
        "orphan-given",
        "an instance outside the companions of its types must be imported and may differ between call sites",
        tree.pos
      )
    else Patch.empty
  }.asPatch
}
