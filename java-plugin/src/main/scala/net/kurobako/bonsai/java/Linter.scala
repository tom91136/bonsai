package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreeScanner
import com.sun.source.util.Trees
import javax.lang.model.element.TypeElement
import javax.lang.model.util.Elements
import javax.lang.model.util.Types
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The linter of the Java code: it walks the trees the compiler gives it and asks the lints about the code they read.
  *
  * The compiler attributes one type at a time, so the linter is given the type it has just attributed, and the lints of
  * the code of that type are the ones it asks about. The lints of the file as a whole are asked once, when the compiler
  * reads the file the first time, as the code of the file is whole then.
  */
class Linter(trees: Trees, elements: Elements, types: Types, enabled: Set[String] = Set.empty) {

  private val read     = mutable.Set.empty[CompilationUnitTree]
  private val registry = new Registry(Linter.release(enabled))

  /** The findings that need every type of the compilation, which the end of it reports. */
  def finished(): Vector[Model.Lint] =
    registry.agreed(Registry.Returns, value => s"the method always returns $value") ++
      registry.agreed(Registry.Empty, _ => "the method is empty")

  /** The findings of the lints in the code of the type the compiler has just attributed. */
  def attributed(compilation: CompilationUnitTree, typeElement: TypeElement): Vector[Model.Lint] =
    Lints.declaration(trees, compilation, typeElement) match {
      case None              => Vector.empty
      case Some(declaration) =>
        Linter.walk(trees, elements, types, enabled, compilation, new TreePath(compilation), declaration, registry)
    }

  /** The findings of the lints of the file as a whole, which the compiler reads once. */
  def file(compilation: CompilationUnitTree): Vector[Model.Lint] =
    if !read.add(compilation) then Vector.empty
    else {
      val root          = new TreePath(compilation)
      val noinspections = Linter.noinspections(compilation)
      val imports       = compilation.getImports.asScala.toSet[Tree]
      val declared      = declarations(compilation)
      Lints.fileLints.filter(lint => lint.enabledByDefault || enabled(lint.id)).flatMap { lint =>
        val context = new Context(trees, elements, types, compilation, root, "", None, List.empty, registry)
        lint.check(context, compilation).filterNot { finding =>
          val lines = noinspections.getOrElse(Lints.line(trees, compilation, finding.at), Set.empty[String])
          lines("*") || lines(lint.id) || imports(finding.at) && (declared("*") || declared(lint.id))
        }.map(finding => Linter.found(lint, context, finding))
      }
    }

  /** The checks every top-level type of the file suppresses, which an import takes as it cannot carry an annotation. */
  private def declarations(compilation: CompilationUnitTree): Set[String] = {
    val root = new TreePath(compilation)
    compilation.getTypeDecls.asScala.toVector.collect { case tree: ClassTree =>
      Collector.suppressed(trees.getElement(new TreePath(root, tree)))
    }.reduceOption(_ intersect _).getOrElse(Set.empty)
  }
}

/** The walk of the code of a type, which asks the lints about the trees they read.
  *
  * The walk keeps the paths of the code itself, so that the lints read the elements and the types of the code the
  * compiler attributed, whichever compiler the build runs.
  */
object Linter {

  /** The release the build names among the lints it enables, as `release=11`. */
  def release(enabled: Set[String]): Option[Int] =
    enabled.collectFirst {
      case setting if setting.startsWith("release=") => setting.drop("release=".length).toIntOption
    }
      .flatten

  private val NoInspectionPattern = """(?://|\*)\s*noinspection\s+([^\r\n*]+)""".r

  /** The suppressions declared by `//noinspection <names>` comments in the compilation unit. */
  def noinspections(compilation: CompilationUnitTree): Map[Int, Set[String]] = {
    val charContent = Option(compilation.getSourceFile).map { sf =>
      try sf.getCharContent(true).toString
      catch { case _: Exception => "" }
    }.getOrElse("")
    if (charContent.isEmpty) Map.empty
    else {
      val lineMap = compilation.getLineMap
      val result  = mutable.Map.empty[Int, mutable.Set[String]]
      val lines   = charContent.linesIterator.toVector
      for (m <- NoInspectionPattern.findAllMatchIn(charContent)) {
        val namesStr = m.group(1).trim
        val names    = namesStr.split("[,\\s]+").filter(_.nonEmpty)
        val resolved = names.flatMap(JavaLint.resolve).toSet
        if (resolved.nonEmpty) {
          val pos  = m.start
          val line = lineMap.getLineNumber(pos).toInt
          // Applies to the comment's line and the next non-empty, non-comment line
          result.getOrElseUpdate(line, mutable.Set.empty) ++= resolved
          result.getOrElseUpdate(line + 1, mutable.Set.empty) ++= resolved
          var next = line
          while (
            next < lines.size &&
            (lines(next).trim.isEmpty || lines(next).trim.startsWith("//") || lines(next).trim.startsWith("/*") ||
              lines(next).trim.startsWith("*"))
          )
            next += 1
          if (next < lines.size) {
            result.getOrElseUpdate(next + 1, mutable.Set.empty) ++= resolved
          }
        }
      }
      result.view.mapValues(_.toSet).toMap
    }
  }

  /** The findings of the lints in the code of a type. */
  def walk(
      trees: Trees,
      elements: Elements,
      types: Types,
      enabled: Set[String],
      compilation: CompilationUnitTree,
      root: TreePath,
      declaration: ClassTree,
      registry: Registry = new Registry()
  ): Vector[Model.Lint] = {
    val paths         = mutable.Stack.empty[TreePath]
    val typeNames     = mutable.Stack.empty[String]
    val methods       = mutable.Stack.empty[String]
    val ancestors     = mutable.ListBuffer.empty[Tree]
    val found         = mutable.ArrayBuffer.empty[Model.Lint]
    val suppressions  = mutable.Stack.empty[Set[String]]
    val noinspections = Linter.noinspections(compilation)

    val scanner = new TreeScanner[Unit, Unit] {
      override def scan(tree: Tree, unused: Unit): Unit =
        if tree != null then {
          val path       = new TreePath(paths.headOption.getOrElse(root), tree)
          val kind       = tree.getKind
          val isType     = Lints.owners.contains(kind)
          val isMethod   = kind == Tree.Kind.METHOD
          val isVariable = kind == Tree.Kind.VARIABLE
          val typeName   = if isType then Collector.qualified(trees.getElement(path)) else ""
          val methodName = if isMethod then Collector.qualified(trees.getElement(path)) else ""
          if isType then typeNames.push(typeName)
          if isMethod then methods.push(methodName)

          val element           = if isType || isMethod || isVariable then trees.getElement(path) else null
          val elementSuppressed = if element != null then Collector.suppressed(element) else Set.empty[String]
          val treeLine          = Lints.line(trees, compilation, tree)
          val lineSuppressed    = noinspections.getOrElse(treeLine, Set.empty[String])
          val currentScope      = suppressions.headOption.getOrElse(Set.empty[String])
          val activeSuppressed  = currentScope ++ elementSuppressed ++ lineSuppressed
          suppressions.push(activeSuppressed)

          val context = new Context(
            trees,
            elements,
            types,
            compilation,
            path,
            typeNames.headOption.getOrElse(""),
            methods.headOption,
            // The trees that hold the code, the closest first, which is what the lints that ask for the code that holds
            // them read.
            ancestors.toList.reverse,
            registry,
            activeSuppressed
          )
          Lints.byKind.getOrElse(kind, Vector.empty)
            .filter(lint =>
              (lint.enabledByDefault || enabled(lint.id)) && !activeSuppressed("*") && !activeSuppressed(lint.id)
            )
            .foreach { lint =>
              lint.check(context, tree).foreach { finding =>
                val findingLine      = Lints.line(trees, compilation, finding.at)
                val atLineSuppressed = noinspections.getOrElse(findingLine, Set.empty[String])
                if (!atLineSuppressed("*") && !atLineSuppressed(lint.id)) {
                  found += Linter.found(lint, context, finding)
                }
              }
            }
          ancestors += tree
          paths.push(path)
          try super.scan(tree, unused)
          finally {
            paths.pop(): Unit
            ancestors.remove(ancestors.size - 1): Unit
            suppressions.pop(): Unit
            if isMethod then methods.pop(): Unit
            if isType then typeNames.pop(): Unit
          }
        } else super.scan(tree, unused)
    }

    scanner.scan(declaration, ())
    found.toVector
  }

  /** The finding of a lint, which the model holds. */
  def found(lint: Lint, context: Context, finding: Found): Model.Lint =
    Model.Lint(
      lint.id,
      context.owner,
      context.method.getOrElse(""),
      Lints.path(context.unit),
      Lints.line(context.trees, context.unit, finding.at),
      finding.detail
    )
}
