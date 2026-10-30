package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

private object references {
  // Scala 3 can attach the same symbol to both a selection and its final name
  def isSelectionName(reference: Tree): Boolean = reference.parent match {
    case Some(selection: Term.Select) => selection.name == reference
    case Some(selection: Type.Select) => selection.name == reference
    case _                            => false
  }

  def imported(pkg: String, name: String)(using document: SemanticDocument): Boolean =
    document.tree.collect {
      case Importer(reference, importees) if reference.syntax == pkg =>
        importees.exists {
          case Importee.Name(imported) => imported.value == name
          case _: Importee.Wildcard    => true
          case _                       => false
        }
    }.contains(true)

  // Java and Scala imports come first, the other packages after a blank line, each group in ASCII order.
  def placements(needed: List[(String, String)])(using document: SemanticDocument): List[Patch] = {
    val statements = document.tree.collect { case pkg: Pkg => pkg.body.stats }.headOption.getOrElse(
      document.tree.collect { case source: Source => source.stats }.flatten
    )
    val imports                        = statements.collect { case importing: Import => importing }
    def key(importing: Import): String =
      importing.importers.head.ref.syntax + "." + importing.importers.head.importees.head.syntax
    def group(path: String): Int = if (Set("java", "javax", "scala")(path.takeWhile(_ != '.'))) 0 else 1
    val placed                   = needed.sortBy { case (pkg, name) => s"$pkg.$name" }.flatMap { case (pkg, name) =>
      val text = s"$pkg.$name"
      val same = imports.filter(importing => group(key(importing)) == group(pkg))
      same.find(importing => key(importing) > text) match {
        case Some(next)                                  => Some((next, true, false, name, pkg))
        case None if same.nonEmpty                       => Some((same.last, false, false, name, pkg))
        case None if imports.nonEmpty && group(pkg) == 0 => Some((imports.head, true, true, name, pkg))
        case None if imports.nonEmpty                    => Some((imports.last, false, true, name, pkg))
        case None                                        => statements.headOption.map((_, true, true, name, pkg))
      }
    }
    placed.groupMap { case (anchor, before, blank, _, _) => (anchor, before, blank) } { case (_, _, _, name, pkg) =>
      s"$pkg.$name"
    }.toList.map {
      case ((anchor, true, blank), paths) =>
        Patch.addLeft(anchor, paths.map(path => s"import $path\n").mkString + (if (blank) "\n" else ""))
      case ((anchor, _, blank), paths) =>
        Patch.addRight(anchor, (if (blank) "\n" else "") + paths.map(path => s"\nimport $path").mkString)
    }
  }
}
