package net.kurobako.baumkuchen.analysis

import net.kurobako.baumkuchen.java.JavaLint
import net.kurobako.baumkuchen.java.{Model => JavaModel}

/** The lints the javac plugin finds in the Java code of a build, which the analysis reports as findings of their own.
  */
object JavaLints {

  /** A lint of the Java code, with the module and the scope of the build it was found in. */
  case class Found(lint: JavaModel.Lint, module: String, scope: Output.Scope, generated: Boolean)

  /** The rules of the lints, by the id of the lint, which the rule of a lint carries; a lint no rule carries is one the
    * plugin found and the analysis does not know yet.
    */
  private val rules: Map[String, Finding.Rule] = {
    val lints = JavaLint.values.map(_.id).toSet
    Finding.Rule.values.filter(rule => lints(rule.id)).map(rule => rule.id -> rule).toMap
  }

  /** The lints whose inspection the IDE reports as a weak warning, so that their findings inform without failing. */
  private val informational: Set[String] = Set(JavaLint.ExtractMethod.id, JavaLint.LoggingSimilar.id)

  /** The findings of the lints, on the nodes of the types that hold the code they are of. */
  def findings(founds: Vector[Found], nodes: Map[String, Node]): Vector[Finding] =
    founds.flatMap(found => finding(found, nodes))

  /** The finding of a lint, which the rule of the lint names, or nothing when no rule carries the id of the lint or the
    * type that holds it is not in the build.
    */
  private def finding(found: Found, nodes: Map[String, Node]): Option[Finding] = {
    val lint = found.lint
    for {
      rule <- rules.get(lint.id)
      node <- nodeOf(found, nodes)
    } yield Finding(
      rule,
      node,
      detail(lint),
      if node.test || informational(lint.id) then Finding.Level.Info else Finding.Level.Warning
    )
  }

  /** The node the finding of a lint is reported on: the node of the type that holds the code, with the place the lint
    * is in, or a node of the file itself when the lint is of the file.
    */
  private def nodeOf(found: Found, nodes: Map[String, Node]): Option[Node] = {
    val lint = found.lint
    if lint.owner.isEmpty then
      Some(Node(
        lint.path,
        found.module,
        found.scope,
        lint.path,
        lint.line,
        generated = found.generated,
        exported = false,
        outer = None
      ))
    else nodes.get(lint.owner).map(node => node.copy(source = lint.path, line = lint.line))
  }

  /** The detail of the finding, which names the method the lint is in, when it is in one. */
  private def detail(lint: JavaModel.Lint): String =
    if lint.method.isEmpty then lint.detail else s"${Finding.short(lint.method)}: ${lint.detail}"
}
