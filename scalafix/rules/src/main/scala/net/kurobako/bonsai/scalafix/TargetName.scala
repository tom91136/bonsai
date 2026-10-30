package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Symbolic definitions callers outside their scope can see carry `@targetName`, which gives them a stable name in
  * bytecode and in other languages. Hidden ones leave it out, unless a same-named sibling needs it to avoid a clash.
  * Overrides take their name from the member they override. Type aliases and opaque types are erased, so they have no
  * name in bytecode to set.
  */
class TargetName extends SyntacticRule("TargetName") {
  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case method: Defn.Def                          => check(method, method.name, method.mods)
    case method: Decl.Def                          => check(method, method.name, method.mods)
    case alias: Defn.Type if annotated(alias.mods) =>
      lint("redundant-target-name", s"${alias.name.value} is erased, so @targetName adds nothing", alias.name.pos)
  }.asPatch

  private def check(definition: Stat, name: Name, mods: List[Mod]): Patch =
    if (!symbolic(name.value) || definitions.has[Mod.Override](mods)) Patch.empty
    else if (definitions.reachable(definition, mods)) Option.unless(annotated(mods))(
      lint("missing-target-name", s"${name.value} is visible, so it needs @targetName", name.pos)
    ).asPatch
    else Option.when(annotated(mods) && !overloaded(definition, name.value))(
      lint("redundant-target-name", s"${name.value} is hidden and has no clash, so @targetName adds nothing", name.pos)
    ).asPatch

  private def symbolic(name: String): Boolean = name.headOption.exists(head => !head.isLetterOrDigit && head != '_')

  private def annotated(mods: List[Mod]): Boolean = definitions.annotated(mods, "targetName")

  // Extension methods of every group in a scope are members of the same owner, so they clash with each other.
  @annotation.tailrec
  private def scope(tree: Tree): List[Stat] = tree.parent match {
    case Some(body: Template.Body)                                    => body.stats
    case Some(body: Pkg.Body)                                         => body.stats
    case Some(source: Source)                                         => source.stats
    case Some(block: Term.Block) if !definitions.extensionBody(block) => block.stats
    case Some(parent)                                                 => scope(parent)
    case None                                                         => Nil
  }

  private def overloaded(definition: Stat, name: String): Boolean = scope(definition).flatMap {
    case group: Defn.ExtensionGroup => group.body match {
        case block: Term.Block => block.stats
        case stat              => List(stat)
      }
    case stat => List(stat)
  }.exists {
    case other: Defn.Def => (other ne definition) && other.name.value == name
    case other: Decl.Def => (other ne definition) && other.name.value == name
    case _               => false
  }
}
