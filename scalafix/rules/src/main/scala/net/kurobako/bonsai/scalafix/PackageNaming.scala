package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.util.matching.Regex
import scalafix.v1.*

object PackageNaming {
  private val Tests    = Vector("Suite", "Spec", "Test")
  val Versioned: Regex = "v\\d+(_\\d+)*".r

  def segments(reference: Term.Ref): List[String] = reference match {
    case Term.Name(value)                           => List(value)
    case Term.Select(qualifier: Term.Ref, selected) => segments(qualifier) :+ selected.value
    case _                                          => Nil
  }

  def lastSegment(pkg: Pkg): Option[String] = segments(pkg.ref).filterNot(Versioned.matches(_)).lastOption

  def redundant(stat: Stat, pkg: Pkg): Option[Name] =
    lastSegment(pkg).flatMap(last => declaredName(stat).filter(declared => repeats(declared.value, last)))

  private def declaredName(stat: Stat): Option[Name] = stat match {
    case declared: Defn.Class  => Some(declared.name)
    case declared: Defn.Trait  => Some(declared.name)
    case declared: Defn.Object => Some(declared.name)
    case declared: Defn.Enum   => Some(declared.name)
    case _                     => None
  }

  // `sass.SASSParser` and `parser.ParserState` repeat their package; the package itself or a longer word does not.
  private def repeats(declared: String, segment: String): Boolean =
    declared.length > segment.length && declared.toLowerCase.startsWith(segment.toLowerCase) &&
      declared.charAt(segment.length).isUpper && !Tests.exists(declared.endsWith(_))
}

/** Version numbers in package names, which move every file on a version change, and type names that repeat the name of
  * their package.
  */
class PackageNaming extends SyntacticRule("PackageNaming") {
  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case pkg: Pkg =>
      val versions = PackageNaming.segments(pkg.ref).collect {
        case segment @ PackageNaming.Versioned(_*) =>
          lint(
            "versioned-package",
            s"package segment $segment pins a version; keep one version per module instead",
            pkg.ref.pos
          )
      }
      val prefix = PackageNaming.lastSegment(pkg).toList.flatMap { last =>
        pkg.body.stats.flatMap(stat =>
          PackageNaming.redundant(stat, pkg).map(declared =>
            lint("redundant-prefix", s"${declared.value} repeats its package name $last", declared.pos)
          )
        )
      }
      (versions ++ prefix).asPatch
  }.asPatch
}
