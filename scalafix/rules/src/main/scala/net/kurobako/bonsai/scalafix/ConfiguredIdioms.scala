package net.kurobako.bonsai.scalafix

import metaconfig.Conf
import metaconfig.Configured
import scala.meta.*
import scalafix.v1.*

object ConfiguredIdioms {
  case class Idiom(pattern: Term, helper: String, message: String) {
    val definition: SymbolMatcher = SymbolMatcher.normalized(helper)
  }

  private def metavariable(name: String): Boolean = name.length > 1 && name.startsWith("$")

  private def idiom(fields: Map[String, Conf]): Either[String, Idiom] =
    (settings.string(fields, "pattern"), settings.string(fields, "helper")) match {
      case (Some(source), Some(helper)) =>
        dialects.Scala3(source).parse[Term].toEither match {
          case Left(error) =>
            Left(s"ConfiguredIdioms: pattern `$source` is not a Scala expression: ${error.message}")
          case Right(Term.Name(name)) if metavariable(name) =>
            Left(s"ConfiguredIdioms: pattern `$source` is a lone metavariable and matches every expression")
          case Right(pattern) =>
            Right(Idiom(pattern, helper, settings.string(fields, "message").getOrElse(s"`$source` is `$helper`")))
        }
      case _ => Left(s"ConfiguredIdioms: every idiom needs a pattern and a helper, got ${fields.keys.mkString(", ")}")
    }
}

/** Configured expression shapes that a named helper already implements.
  *
  * `ConfiguredIdioms.idioms` lists objects with a `pattern`, the fully qualified name of its `helper`, whose own body
  * may spell the pattern out, and an optional `message`. A pattern is a Scala expression in which an identifier
  * starting with `$` is a metavariable: it matches any expression, and every occurrence of a repeated metavariable must
  * match the same expression. Parentheses and the width of integer literals are ignored; everything else must match
  * structurally.
  */
class ConfiguredIdioms(idioms: List[ConfiguredIdioms.Idiom]) extends SemanticRule("ConfiguredIdioms") {
  def this() = this(Nil)

  override def withConfiguration(config: Configuration): Configured[Rule] = {
    val parsed =
      settings.objects(settings.section(config.conf, "ConfiguredIdioms"), "idioms").map(ConfiguredIdioms.idiom(_))
    parsed.collect { case Left(error) => error } match {
      case Nil    => Configured.Ok(ConfiguredIdioms(parsed.collect { case Right(idiom) => idiom }))
      case errors => Configured.error(errors.mkString("\n"))
    }
  }

  override def fix(using document: SemanticDocument): Patch =
    if (idioms.isEmpty) Patch.empty
    else document.tree.collect { case term: Term =>
      idioms.collect {
        case idiom
            if bind(idiom.pattern, term, Map.empty, true).isDefined && !helperBody(term, idiom) && !inlined(term) =>
          lint("configured-idiom", idiom.message, term.pos)
      }.asPatch
    }.asPatch

  private def helperBody(term: Term, idiom: ConfiguredIdioms.Idiom)(using SemanticDocument): Boolean =
    definitions.ancestors(term).exists {
      case definition: Defn.Def => idiom.definition.matches(definition.name)
      case _                    => false
    }

  // An inline method is expanded where it is called, so a helper called from it would be expanded at every such site.
  private def inlined(term: Term): Boolean = definitions.ancestors(term).exists {
    case definition: Defn.Def => definition.mods.exists { case _: Mod.Inline => true; case _ => false }
    case _                    => false
  }

  private def integer(literal: Lit): Option[Long] = literal match {
    case Lit.Int(value)  => Some(value.toLong)
    case Lit.Long(value) => Some(value)
    case _               => None
  }

  // Trees carry no parentheses, so comparing them ignores redundant ones.
  private def bind(pattern: Tree, value: Tree, bindings: Map[String, Term], metavariables: Boolean)
      : Option[Map[String, Term]] = (pattern, value) match {
    case (Term.Name(name), term: Term) if metavariables && ConfiguredIdioms.metavariable(name) =>
      bindings.get(name) match {
        case Some(bound) => bind(bound, term, bindings, false).map(_ => bindings)
        case None        => Some(bindings.updated(name, term))
      }
    case (expected: Lit, actual: Lit) if integer(expected).isDefined =>
      Option.when(integer(expected) == integer(actual))(bindings)
    case _ if pattern.productPrefix != value.productPrefix => None
    case (expected: Lit, actual: Lit)                      => Option.when(expected.value == actual.value)(bindings)
    case (expected: Name, actual: Name)                    => Option.when(expected.value == actual.value)(bindings)
    case _ if pattern.children.size != value.children.size => None
    case _                                                 =>
      pattern.children.zip(value.children).foldLeft(Option(bindings)) { case (bound, (expected, actual)) =>
        bound.flatMap(bind(expected, actual, _, metavariables))
      }
  }
}
