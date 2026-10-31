package net.kurobako.bonsai.scalafix

import metaconfig.Configured
import scala.meta.*
import scalafix.v1.*

/** Writes a declaration reached through its full package path by its simple name, and imports it.
  *
  * Packages of `QualifiedCollectionNamespaces` and `NamespaceImports` keep the forms those rules require. A name that
  * another declaration in the file also uses keeps its full path and is not reported.
  */
class FullyQualifiedReference(namespaces: List[QualifiedCollectionNamespaces.Namespace])
    extends SemanticRule("FullyQualifiedReference") {
  def this() = this(QualifiedCollectionNamespaces.Defaults)

  override def withConfiguration(config: Configuration): Configured[Rule] = {
    val packages = settings.strings(settings.section(config.conf, "NamespaceImports"), "packages")
    Configured.Ok(FullyQualifiedReference(
      QualifiedCollectionNamespaces.fromConfiguration(config) ++
        packages.map(pkg => QualifiedCollectionNamespaces.Namespace(pkg, pkg, subpackages = true))
    ))
  }

  override def fix(using document: SemanticDocument): Patch = {
    val written  = document.tree.collect { case tree @ (_: Term.Select | _: Type.Select) => candidate(tree) }.flatten
    val entities =
      (document.tree.collect {
        case name: Name if references.notSelectionName(name) && !name.symbol.isNone =>
          name.value -> entity(name.symbol.value)
      } ++ written.map(c => c.name -> c.entity)).groupMap(_._1)(_._2)
    val rewritable = written.filterNot(c => entities(c.name).exists(_ != c.entity))
    val needed     = rewritable.map(c => (c.pkg, c.name)).distinct.filterNot {
      case (pkg, name) => references.imported(pkg, name)
    }
    (rewritable.map(c =>
      lint.replace(c.tree, c.name, "fully-qualified-reference", s"write ${c.name} by its simple name")
    ) ++ references.placements(needed)).asPatch
  }

  private final case class Candidate(tree: Tree, pkg: String, name: String, entity: String)

  private def candidate(tree: Tree)(using SemanticDocument): Option[Candidate] = selection(tree).flatMap {
    case (qualifier, name) =>
      packagePath(qualifier).filter(_.size >= 2).flatMap { path =>
        val prefix = path.mkString("", "/", "/")
        val symbol = tree.symbol.value
        Option.when(
          symbol.startsWith(prefix) && !symbol.drop(prefix.length).contains('/') && path != List("java", "lang") &&
            !namespaces.exists(_.owns(prefix)) && ancestry.outsideImports(tree) && !tree.parent.exists {
              case clause: Pkg => clause.ref eq tree
              case _           => false
            }
        )(Candidate(tree, path.mkString("."), name.value, entity(symbol)))
      }
  }

  private def selection(tree: Tree): Option[(Term, Name)] = tree match {
    case Term.Select(qualifier, name) => Some(qualifier -> name)
    case Type.Select(qualifier, name) => Some(qualifier -> name)
    case _                            => None
  }

  private def packagePath(tree: Term)(using SemanticDocument): Option[List[String]] = tree match {
    case name: Term.Name if name.value != "_root_" && name.symbol.value.endsWith("/") => Some(List(name.value))
    case Term.Select(qualifier, name) if name.symbol.value.endsWith("/")              =>
      packagePath(qualifier).map(_ :+ name.value)
    case _ => None
  }

  private def entity(value: String): String = value.stripSuffix("#").stripSuffix(".")
}
