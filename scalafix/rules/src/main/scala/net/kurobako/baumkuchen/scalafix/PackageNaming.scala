package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Version numbers in package names, which move every file on a version change, and type names that repeat the name of
  * their package.
  */
class PackageNaming extends SyntacticRule("PackageNaming") {
  private val Tests     = Vector("Suite", "Spec", "Test")
  private val Versioned = "v\\d+(_\\d+)*".r

  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case pkg: Pkg =>
      val segments = name(pkg.ref)
      val versions = segments.collect {
        case segment @ Versioned(_*) =>
          lint(
            "versioned-package",
            s"package segment $segment pins a version; keep one version per module instead",
            pkg.ref.pos
          )
      }
      val prefix = segments.filterNot(Versioned.matches(_)).lastOption.toList.flatMap { last =>
        pkg.body.stats.collect {
          case declared: Defn.Class if repeats(declared.name.value, last)  => declared.name
          case declared: Defn.Trait if repeats(declared.name.value, last)  => declared.name
          case declared: Defn.Object if repeats(declared.name.value, last) => declared.name
          case declared: Defn.Enum if repeats(declared.name.value, last)   => declared.name
        }.map(declared =>
          lint("redundant-prefix", s"${declared.value} repeats its package name $last", declared.pos)
        )
      }
      (versions ++ prefix).asPatch
  }.asPatch

  private def name(reference: Term.Ref): List[String] = reference match {
    case Term.Name(value)                           => List(value)
    case Term.Select(qualifier: Term.Ref, selected) => name(qualifier) :+ selected.value
    case _                                          => Nil
  }

  // `sass.SASSParser` and `parser.ParserState` repeat their package; the package itself or a longer word does not.
  private def repeats(declared: String, segment: String): Boolean =
    declared.length > segment.length && declared.toLowerCase.startsWith(segment.toLowerCase) &&
      declared.charAt(segment.length).isUpper && !Tests.exists(declared.endsWith(_))
}
