package net.kurobako.baumkuchen.scalafix

import metaconfig.Configured
import scala.meta.*
import scalafix.v1.*

object QualifiedCollectionNamespaces {

  /** Members of `pkg`, and of its subpackages when `subpackages` holds, are written `prefix.Member`. */
  case class Namespace(pkg: String, prefix: String, subpackages: Boolean) {
    val symbol: String = pkg.replace('.', '/') + "/"

    // Members of the types in a package are owned by those types, whose symbols do not end in a slash.
    def owns(owner: String): Boolean =
      if (subpackages) owner.endsWith("/") && owner.startsWith(symbol) else owner == symbol
  }

  val Defaults: List[Namespace] = List(
    Namespace("scala.collection.mutable", "mutable", subpackages = true),
    Namespace("java.util", "java.util", subpackages = false)
  )
}

/** Types whose names collide with the ones code reaches for first, written so that they read as what they are: a
  * mutable collection as `mutable.LongMap`, and a type of `java.util` as `java.util.BitSet`, in types and terms alike.
  *
  * `QualifiedCollectionNamespaces.namespaces` lists objects with a `package`, the `prefix` its members are written
  * with, and whether `subpackages` are covered too; by default `scala.collection.mutable`, written `mutable.` and
  * imported only as `import scala.collection.mutable`, and the types declared directly in `java.util`, written in full
  * and never imported.
  */
class QualifiedCollectionNamespaces(namespaces: List[QualifiedCollectionNamespaces.Namespace])
    extends SemanticRule("QualifiedCollectionNamespaces") {
  def this() = this(QualifiedCollectionNamespaces.Defaults)

  override def withConfiguration(config: Configuration): Configured[Rule] = {
    val configured = settings.objects(settings.section(config.conf, "QualifiedCollectionNamespaces"), "namespaces")
      .flatMap(fields =>
        (settings.string(fields, "package"), settings.string(fields, "prefix")) match {
          case (Some(pkg), Some(prefix)) =>
            Some(QualifiedCollectionNamespaces.Namespace(
              pkg,
              prefix,
              settings.string(fields, "subpackages") != Some("false")
            ))
          case _ => None
        }
      )
    Configured.Ok(QualifiedCollectionNamespaces(if (configured.isEmpty) QualifiedCollectionNamespaces.Defaults
    else configured))
  }

  override def fix(using document: SemanticDocument): Patch = {
    val imported = document.tree.collect {
      case importer: Importer if importer.parent.exists { case _: Import => true; case _ => false } =>
        namespace(importer.ref.symbol.value).toList.flatMap(space =>
          importer.importees.collect {
            case importee @ (_: Importee.Name | _: Importee.Rename | _: Importee.Wildcard) =>
              lint(
                "member-import",
                s"import ${
                    if (space.prefix == space.pkg) "nothing of" else space.pkg.stripSuffix(s".${space.prefix}")
                  } " +
                  s"${space.pkg}; write its members as ${space.prefix}.${member(importee)}",
                importee.pos
              ) + Patch.removeImportee(importee)
          }
        )
    }.flatten
    val prefixed = document.tree.collect {
      case name: Name if reference(name) =>
        val symbol = name.symbol
        Option.when(!symbol.isNone)(symbol.owner.value).flatMap(namespace(_)).filterNot(written(name, _)).map {
          space =>
            val replaced = qualified(name)
            lint.replace(
              replaced,
              s"${space.prefix}.${symbol.displayName}",
              "unqualified",
              s"write ${space.prefix}.${symbol.displayName}, so the name reads as what it is"
            )
        }
    }.flatten
    val needed = prefixed.nonEmpty || imported.nonEmpty
    val added  = namespaces.filter(space => space.prefix != space.pkg && needed && !importsPackage(space)).map(space =>
      Patch.addGlobalImport(importer(space))
    )
    (imported ++ prefixed ++ added).asPatch
  }

  private def namespace(owner: String): Option[QualifiedCollectionNamespaces.Namespace] =
    namespaces.find(_.owns(owner))

  private def member(importee: Importee): String = importee match {
    case Importee.Name(name)      => name.value
    case Importee.Rename(name, _) => name.value
    case _                        => "<member>"
  }

  // A name standing for a reference in code, not a definition or part of an import; the names of a package clause
  // stand for packages, which no namespace owns.
  private def reference(name: Name): Boolean = name match {
    case _: Term.Name | _: Type.Name =>
      !name.isDefinition && !definitions.ancestors(name).exists {
        case _: Import | _: Export => true
        case _                     => false
      }
    case _ => false
  }

  // The tree to replace: the name, or the qualified reference it ends.
  private def qualified(name: Name): Tree = name.parent match {
    case Some(select @ Term.Select(_, selected)) if selected eq name => select
    case Some(select @ Type.Select(_, selected)) if selected eq name => select
    case _                                                           => name
  }

  private def written(name: Name, space: QualifiedCollectionNamespaces.Namespace): Boolean = name.parent match {
    case Some(Term.Select(qualifier, selected)) if selected eq name => accepted(qualifier.syntax, space)
    case Some(Type.Select(qualifier, selected)) if selected eq name => accepted(qualifier.syntax, space)
    case _                                                          => false
  }

  private def accepted(qualifier: String, space: QualifiedCollectionNamespaces.Namespace): Boolean = {
    val path = qualifier.stripPrefix("_root_.")
    path == space.prefix || path == space.pkg || space.subpackages && path.startsWith(s"${space.prefix}.")
  }

  private def importsPackage(space: QualifiedCollectionNamespaces.Namespace)(using
      document: SemanticDocument
  ): Boolean =
    document.tree.collect {
      case Importer(reference, importees) if reference.syntax == space.pkg.stripSuffix(s".${space.prefix}") =>
        importees.exists {
          case Importee.Name(name) => name.value == space.prefix
          case _                   => false
        }
    }.contains(true)

  private def importer(space: QualifiedCollectionNamespaces.Namespace): Importer = {
    val path      = space.pkg.split('.').toList
    val reference = path.init.tail.foldLeft[Term.Ref](Term.Name(path.head))((qualifier, segment) =>
      Term.Select(qualifier, Term.Name(segment))
    )
    Importer(reference, List(Importee.Name(Name.Indeterminate(path.last))))
  }
}
