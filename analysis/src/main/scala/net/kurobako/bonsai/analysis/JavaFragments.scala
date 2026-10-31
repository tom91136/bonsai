package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.java.{Clones => JavaClones}
import net.kurobako.bonsai.java.{Model => JavaModel}
import scala.collection.Seq

/** Groups the fragments of the Java code of a build into the copies of the code that recur, the way the fragments of
  * the Scala code are grouped.
  *
  * The grouping is done here, and not by the javac plugin, as the copies of the code of one module are of the code of
  * the whole build: the same block of code written in two modules of it is as much a copy as one written twice in one.
  */
object JavaFragments {

  /** The groups of copies the fragments of the Java code make, which the rules read alike with those of the Scala code.
    */
  def groups(fragments: Seq[(JavaModel.Fragment, Boolean)]): Vector[Clones.Group] =
    Clones.groups(fragments.toVector.map((fragment, test) => candidate(fragment, test)))

  private def candidate(fragment: JavaModel.Fragment, test: Boolean): Clones.Candidate = {
    def has(role: Int): Boolean = (fragment.roles & role) != 0
    val whole                   = has(JavaModel.Roles.Whole)
    Clones.Candidate(
      Clones.Fragment(fragment.node, fragment.path, fragment.start, fragment.end, test, member = fragment.member),
      (fragment.from, fragment.to),
      fragment.cost,
      exact = fragment.exact,
      loose = fragment.loose,
      literals = Clones.Deferred(fragment.literals.map(Clones.Literal(_)())),
      minimumLines = if (whole) Clones.WindowLines else 1,
      solid = whole,
      broad = fragment.broad,
      variables = Clones.Deferred(fragment.variables.map(variable)),
      small = has(JavaModel.Roles.Small),
      tiny = has(JavaModel.Roles.Tiny),
      entire = has(JavaModel.Roles.Entire),
      direct = has(JavaModel.Roles.Direct),
      helper = has(JavaModel.Roles.Helper),
      reach = Option.when(fragment.reach.nonEmpty)(fragment.reach),
      whole = whole
    )
  }

  private def variable(value: JavaClones.Variable): Clones.Literal =
    if value.kind == 'R' then
      Clones.Literal(
        value.text,
        reference = true,
        parameter = Option.when(value.parameter >= 0)(Clones.Binding(value.parameter, value.suffix))
      )(value.shown)
    else Clones.Literal(value.text, string = value.kind == 'S')()
}
