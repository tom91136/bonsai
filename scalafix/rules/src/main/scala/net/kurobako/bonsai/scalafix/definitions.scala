package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.util.control.NonFatal
import scalafix.v1.*

private object definitions {
  def public(mods: List[Mod]): Boolean = !mods.exists {
    case _: Mod.Private | _: Mod.Protected => true
    case _                                 => false
  }

  /** Whether `mods` carry an annotation named `name`, written bare or qualified. */
  def annotated(mods: List[Mod], name: String): Boolean = mods.exists {
    case Mod.Annot(init) =>
      init.tpe match {
        case Type.Name(annotation)                 => annotation == name
        case Type.Select(_, Type.Name(annotation)) => annotation == name
        case _                                     => false
      }
    case _ => false
  }

  /** The path of the file an input names, or nothing for an input that is not a file. */
  def path(input: Input): String = input match {
    case file: Input.File        => file.path.toString
    case file: Input.VirtualFile => file.path
    case _                       => ""
  }

  def has[M <: Mod](mods: List[Mod])(using tag: scala.reflect.ClassTag[M]): Boolean =
    mods.exists(tag.runtimeClass.isInstance(_))

  def parameters(definition: Defn.Def): List[Term.Param] =
    definition.paramClauseGroups.flatMap(_.paramClauses.flatMap(clause =>
      if (clause.mod.nonEmpty) Nil else clause.values
    ))

  /** The symbol of a declared type, looking through application and annotations. */
  @annotation.tailrec
  def symbol(tpe: Type)(using SemanticDocument): Symbol = tpe match {
    case application: Type.Apply => symbol(application.tpe)
    case Type.Annotate(base, _)  => symbol(base)
    case other                   => other.symbol
  }

  /** The information of a symbol, or none when the symbol table cannot read its class file, as for JDK classes on a JDK
    * newer than the ASM the symbol table reads them with.
    */
  def info(symbol: Symbol)(using SemanticDocument): Option[SymbolInformation] =
    try symbol.info
    catch {
      case NonFatal(_) => None
    }

  /** Whether the term names a method that takes arguments, which the compiler eta expands where a function is wanted.
    */
  def methodReference(term: Term)(using SemanticDocument): Boolean = term match {
    case reference: (Term.Name | Term.Select) =>
      info(reference.symbol).map(_.signature).exists {
        case MethodSignature(_, parameterLists, _) =>
          parameterLists.filterNot(_.exists(parameter => parameter.isImplicit || parameter.isGiven)).exists(_.nonEmpty)
        case _ => false
      }
    case _ => false
  }

  @annotation.tailrec
  private def resolve(tpe: SemanticType, visited: Set[Symbol])(using SemanticDocument): Option[Symbol] = tpe match {
    case ExistentialType(inner, _)                       => resolve(inner, visited)
    case AnnotatedType(_, inner)                         => resolve(inner, visited)
    case ByNameType(inner)                               => resolve(inner, visited)
    case TypeRef(_, reference, _) if !visited(reference) =>
      info(reference).map(_.signature) match {
        case Some(TypeSignature(_, lower, upper)) if lower == upper => resolve(upper, visited + reference)
        case _                                                      => Some(reference)
      }
    case _ => None
  }

  /** The type constructor of a value or parameterless method, through aliases. */
  private def valueType(symbol: Symbol)(using SemanticDocument): Option[Symbol] =
    info(symbol).map(_.signature).flatMap {
      case ValueSignature(tpe)          => resolve(tpe, Set.empty)
      case MethodSignature(_, Nil, tpe) => resolve(tpe, Set.empty)
      case _                            => None
    }

  /** The type constructor of a named value, through aliases. */
  def typeSymbol(term: Term)(using SemanticDocument): Option[String] = (term match {
    case reference: (Term.Name | Term.Select) => Some(reference.symbol)
    case _                                    => None
  }).flatMap(valueType(_)).map(_.value)

  def arrayType(term: Term)(using SemanticDocument): Boolean = typeSymbol(term).contains("scala/Array#")

  /** Statements of an object, class or trait body. */
  def members(template: Template): List[Stat] = template.body.stats
}
