package net.kurobako.bonsai.scalafix

import metaconfig.Configured
import scala.meta.*
import scalafix.v1.*

/** Import the configured namespaces once, and keep their package paths out of expressions and types.
  *
  * `NamespaceImports.packages` lists the root packages, such as `org.example`, whose members are imported by wildcard.
  */
class NamespaceImports(packages: List[List[String]]) extends SemanticRule("NamespaceImports") {
  def this() = this(Nil)

  override def withConfiguration(config: Configuration): Configured[Rule] = Configured.Ok(
    NamespaceImports(settings.strings(
      settings.section(config.conf, "NamespaceImports"),
      "packages"
    ).map(_.split('.').toList))
  )

  override def fix(using document: SemanticDocument): Patch = if (packages.isEmpty) Patch.empty
  else document.tree.collect {
    case importer: Importer
        if importer.parent.exists { case _: Import => true; case _ => false } &&
          namespaced(importer.ref) && importer.importees.exists {
            case Importee.Name(name) => !definitions.info(name.symbol).exists(_.isGiven)
            case _                   => false
          } =>
      lint(
        "namespace-import",
        "use a namespace wildcard import; retain rename, exclusion, and given selectors where needed",
        importer.pos
      )
    case reference: Term.Select if qualified(reference) && namespaced(reference) && codeReference(reference) =>
      qualifiedDiagnostic(reference)
    case reference: Type.Select if qualified(reference) && namespaced(reference) && codeReference(reference) =>
      qualifiedDiagnostic(reference)
  }.asPatch

  private def namespaced(tree: Tree)(using SemanticDocument): Boolean =
    packages.exists(root => tree.symbol.value.startsWith(root.mkString("", "/", "/")))

  private def qualified(tree: Tree): Boolean = {
    val segments = path(tree).dropWhile(_ == "_root_")
    packages.exists(segments.startsWith(_))
  }

  private def path(tree: Tree): List[String] = tree match {
    case Term.Name(name)              => List(name)
    case Term.Select(qualifier, name) => path(qualifier) :+ name.value
    case Type.Select(qualifier, name) => path(qualifier) :+ name.value
    case _                            => Nil
  }

  private def codeReference(tree: Tree): Boolean = tree.parent match {
    case Some(_: Import | _: Export | _: Importer | _: Pkg) => false
    case Some(parent: Term.Select) if qualified(parent)     => false
    case Some(parent: Type.Select) if qualified(parent)     => false
    case _                                                  => true
  }

  private def qualifiedDiagnostic(tree: Tree): Patch = lint(
    "qualified-reference",
    "import the namespace and use a short reference; use an alias for a conflicting name",
    tree.pos
  )
}
