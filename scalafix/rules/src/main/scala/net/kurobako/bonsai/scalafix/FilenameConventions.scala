package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Keeps named top-level declarations in the file named after them.
  *
  * This is intentionally a linter rather than a rewrite: grouped declarations are a structural decision and need to be
  * reviewed before they are split. A single diagnostic is emitted per file so the first pass is actionable even for
  * generated files that contain a large number of declarations.
  */
class FilenameConventions extends SyntacticRule("FilenameConventions") {
  private case class Declaration(name: String, position: Position)

  override def fix(using document: SyntacticDocument): Patch =
    sourceName(document.input).fold(Patch.empty) { fileName =>
      // scala-cli selects a source's scope and platforms by suffixes, which are not part of its name.
      val expected     = fileName.stripSuffix(".scala").replaceFirst("""(\.(test|jvm|js|native))+$""", "")
      val declarations = topLevelDeclarations(document.tree)
      val mismatches   = declarations.filterNot(_.name == expected)
      if (mismatches.isEmpty) Patch.empty
      else {
        val names = mismatches.map(_.name).distinct.sorted.mkString(", ")
        lint(
          "top-level-filename",
          s"named top-level declarations in $fileName must be named $expected; found: $names",
          mismatches.head.position
        )
      }
    }

  private def sourceName(input: Input): Option[String] =
    definitions.path(input).split("[/\\\\]").lastOption.filter(_.nonEmpty)

  private def topLevelDeclarations(tree: Tree): List[Declaration] = tree match {
    case Source(stats) => stats.flatMap(stat => packageStats(stat).flatMap(declarations(_)))
    case _             => Nil
  }

  private def packageStats(stat: Stat): List[Stat] = stat match {
    case pkg: Pkg => pkg.body.stats.flatMap(packageStats(_))
    case other    => List(other)
  }

  private def declarations(stat: Stat): List[Declaration] = stat match {
    case value: (Defn.Val | Decl.Val | Defn.Var | Decl.Var) => patternDeclarations(value.pats)
    case value: Defn.Def                                    => named(value.name)
    case value: Decl.Def                                    => named(value.name)
    case value: Defn.Type                                   => named(value.name)
    case value: Decl.Type                                   => named(value.name)
    case value: Defn.Class                                  => named(value.name)
    case value: Defn.Trait                                  => named(value.name)
    case value: Defn.Object                                 => named(value.name)
    case value: Defn.Enum                                   => named(value.name)
    case value: Defn.Given                                  => named(value.name)
    case value: Defn.GivenAlias                             => named(value.name)
    case _                                                  => Nil
  }

  private def named(name: Name): List[Declaration] =
    if (name.value.isEmpty) Nil else List(Declaration(name.value, name.pos))

  private def patternDeclarations(patterns: List[Pat]): List[Declaration] = patterns.collect { case Pat.Var(name) =>
    Declaration(name.value, name.pos)
  }
}
