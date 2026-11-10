package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Keeps named top-level declarations in the file named after them.
  *
  * This is intentionally a linter rather than a rewrite: grouped declarations are a structural decision and need to be
  * reviewed before they are split. Each stray name gets one diagnostic, so a suppression covers only what it names. A
  * stray in a reference cycle with the namesake is exempt: moving it out would make the two files depend on each other.
  */
class FilenameConventions extends SyntacticRule("FilenameConventions") {
  private case class Declaration(name: String, position: Position, stat: Stat)

  override def fix(using document: SyntacticDocument): Patch =
    sourceName(document.input).fold(Patch.empty) { fileName =>
      // scala-cli selects a source's scope and platforms by suffixes, which are not part of its name.
      val expected      = fileName.stripSuffix(".scala").replaceFirst("""(\.(test|jvm|js|native))+$""", "")
      val declarations  = topLevelDeclarations(document.tree)
      val declared      = declarations.map(_.name).toSet
      val edges         = declarations.groupMapReduce(_.name)(d => referenced(d.stat, declared))(_ ++ _)
      val fromNamesake  = reachable(expected, edges)
      val sealedParents = declarations.collect { case Declaration(name, _, stat) if isSealed(stat) => name }.toSet
      // PackageNaming reports these, and its rename also resolves the file mismatch.
      val prefixed = document.tree.collect {
        case pkg: Pkg => pkg.body.stats.flatMap(stat => PackageNaming.redundant(stat, pkg)).map(_.pos.start)
      }.flatten.toSet
      declarations.filter(_.name != expected).filterNot(stray => prefixed(stray.position.start)).filterNot { stray =>
        fromNamesake.contains(stray.name) && reachable(stray.name, edges).contains(expected)
      }.distinctBy(_.name).map { stray =>
        parents(stray.stat).find(sealedParents) match {
          case Some(parent) =>
            lint(
              "top-level-filename",
              s"top-level declaration ${stray.name} in $fileName extends sealed $parent: nest it in the companion of " +
                s"$parent or remove sealed from $parent",
              stray.position
            )
          case None if stray.name.equalsIgnoreCase(expected) =>
            lint(
              "top-level-filename",
              s"top-level declaration ${stray.name} in $fileName must be nested in object $expected",
              stray.position
            )
          case None =>
            lint(
              "top-level-filename",
              s"top-level declaration ${stray.name} in $fileName must be named $expected or nested in object $expected",
              stray.position
            )
        }
      }.asPatch
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

  private def declarations(stat: Stat): List[Declaration] =
    declaredNames(stat).filter(_.value.nonEmpty).map(name => Declaration(name.value, name.pos, stat))

  private def declaredNames(stat: Stat): List[Name] = stat match {
    case value: (Defn.Val | Decl.Val | Defn.Var | Decl.Var) => value.pats.collect { case Pat.Var(name) => name }
    case value: Defn.Def                                    => List(value.name)
    case value: Decl.Def                                    => List(value.name)
    case value: Defn.Type                                   => List(value.name)
    case value: Decl.Type                                   => List(value.name)
    case value: Defn.Class                                  => List(value.name)
    case value: Defn.Trait                                  => List(value.name)
    case value: Defn.Object                                 => List(value.name)
    case value: Defn.Enum                                   => List(value.name)
    case value: Defn.Given                                  => List(value.name)
    case value: Defn.GivenAlias                             => List(value.name)
    case _                                                  => Nil
  }

  private def isSealed(stat: Stat): Boolean = stat match {
    case value: (Defn.Class | Defn.Trait) => value.mods.exists(_.is[Mod.Sealed])
    case _                                => false
  }

  private def parents(stat: Stat): List[String] = stat match {
    case value: (Defn.Class | Defn.Trait | Defn.Object) =>
      value.templ.inits.map(_.tpe).collect {
        case Type.Name(name)                            => name
        case Type.Apply.After_4_6_0(Type.Name(name), _) => name
      }
    case _ => Nil
  }

  private def referenced(stat: Stat, declared: Set[String]): Set[String] =
    stat.collect { case name: Name if declared(name.value) => name.value }.toSet

  private def reachable(start: String, edges: Map[String, Set[String]]): Set[String] = {
    @annotation.tailrec
    def visit(frontier: List[String], seen: Set[String]): Set[String] = frontier match {
      case Nil          => seen
      case name :: rest =>
        val next = edges.getOrElse(name, Set.empty).diff(seen)
        visit(next.toList ::: rest, seen ++ next)
    }
    visit(List(start), Set(start))
  }
}
