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

  private val read          = mutable.Set.empty[CompilationUnitTree]
  private val registry      = new Registry(Linter.release(enabled))
  private val noinspections = mutable.Map.empty[CompilationUnitTree, Map[Int, Set[String]]]
  private val active = Linter.byKind.view.mapValues(_.filter(lint => lint.enabledByDefault || enabled(lint.id))).toMap

  private def suppressed(compilation: CompilationUnitTree): Map[Int, Set[String]] =
    noinspections.getOrElseUpdate(compilation, Linter.noinspections(compilation))

  /** The findings that need every type of the compilation, which the end of it reports. */
  def finished(): Vector[Model.Lint] =
    registry.agreed(Registry.Returns, value => s"the method always returns $value") ++
      registry.agreed(Registry.Empty, _ => "the method is empty")

  /** The findings of the lints in the code of the type the compiler has just attributed. */
  def attributed(compilation: CompilationUnitTree, typeElement: TypeElement): Vector[Model.Lint] =
    Lints.declaration(trees, compilation, typeElement) match {
      case None              => Vector.empty
      case Some(declaration) =>
        Linter.walk(
          trees,
          elements,
          types,
          active,
          compilation,
          new TreePath(compilation),
          declaration,
          suppressed(compilation),
          registry
        )
    }

  /** The findings of the lints of the file as a whole, which the compiler reads once. */
  def file(compilation: CompilationUnitTree): Vector[Model.Lint] =
    if !read.add(compilation) then Vector.empty
    else {
      val root          = new TreePath(compilation)
      val noinspections = suppressed(compilation)
      val imports       = compilation.getImports.asScala.toSet[Tree]
      val declared      = declarations(compilation)
      val context       = new Context(trees, elements, types, compilation, root, "", None, List.empty, registry)
      Linter.fileLints.view.filter(lint => lint.enabledByDefault || enabled(lint.id)).flatMap { lint =>
        lint.check(context, compilation).view.filterNot { finding =>
          val lines = noinspections.getOrElse(Linter.line(trees, compilation, finding), Set.empty[String])
          val id    = finding.reportedAs(lint)
          lines(JavaLint.Wildcard) || lines(id) || imports(finding.at) && (declared(JavaLint.Wildcard) || declared(id))
        }.map(finding => Linter.found(lint, context, finding))
      }.toVector
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

  /** The line a finding is on, which is the one of its offset, or the one of its code. */
  private def line(trees: Trees, unit: CompilationUnitTree, finding: Found): Int =
    if finding.offset >= 0 then unit.getLineMap.getLineNumber(finding.offset).toInt
    else line(trees, unit, finding.at)

  /** The line the code of a tree is on. */
  private def line(trees: Trees, unit: CompilationUnitTree, tree: Tree): Int = {
    // a method is placed on the line of its result type, which is where its name is, as its annotations and modifiers
    // may be on lines above it
    val positions = trees.getSourcePositions
    val offset    = tree match {
      case method: MethodTree if method.getReturnType != null =>
        positions.getStartPosition(unit, method.getReturnType)
      case _ => positions.getStartPosition(unit, tree)
    }
    if offset < 0 then 0 else unit.getLineMap.getLineNumber(offset).toInt
  }

  /** The release the build names among the lints it enables, as `release=11`. */
  private def release(enabled: Set[String]): Option[Int] =
    enabled.collectFirst {
      case setting if setting.startsWith("release=") => setting.drop("release=".length).toIntOption
    }
      .flatten

  private val NoInspectionPattern = """(?://|\*)\s*noinspection\s+([^\r\n*]+)""".r

  /** The suppressions declared by `//noinspection <names>` comments in the compilation unit. */
  private def noinspections(compilation: CompilationUnitTree): Map[Int, Set[String]] = {
    val charContent = Option(compilation.getSourceFile).map { sf =>
      try sf.getCharContent(true).toString
      catch { case _: Exception => "" }
    }.getOrElse("")
    if (!charContent.contains("noinspection")) Map.empty
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
  private def walk(
      trees: Trees,
      elements: Elements,
      types: Types,
      lints: Map[Tree.Kind, Vector[Lint]],
      compilation: CompilationUnitTree,
      root: TreePath,
      declaration: ClassTree,
      noinspections: Map[Int, Set[String]],
      registry: Registry
  ): Vector[Model.Lint] = {
    val paths        = mutable.Stack.empty[TreePath]
    val typeNames    = mutable.Stack.empty[String]
    val methods      = mutable.Stack.empty[String]
    val found        = mutable.ArrayBuffer.empty[Model.Lint]
    val suppressions = mutable.Stack.empty[Set[String]]
    var ancestors    = List.empty[Tree]

    val scanner = new TreeScanner[Unit, Unit] {
      override def scan(tree: Tree, unused: Unit): Unit =
        if tree != null then {
          val path       = new TreePath(paths.headOption.getOrElse(root), tree)
          val kind       = tree.getKind
          val isType     = owners.contains(kind)
          val isMethod   = kind == Tree.Kind.METHOD
          val isVariable = kind == Tree.Kind.VARIABLE
          val typeName   = if isType then Collector.qualified(trees.getElement(path))(using trees) else ""
          val methodName = if isMethod then Collector.qualified(trees.getElement(path))(using trees) else ""
          if isType then typeNames.push(typeName)
          if isMethod then methods.push(methodName)

          val element           = if isType || isMethod || isVariable then trees.getElement(path) else null
          val elementSuppressed = if element != null then Collector.suppressed(element) else Set.empty[String]
          val lineSuppressed    =
            if noinspections.isEmpty then Set.empty[String]
            else noinspections.getOrElse(Linter.line(trees, compilation, tree), Set.empty[String])
          val currentScope     = suppressions.headOption.getOrElse(Set.empty[String])
          val activeSuppressed =
            if elementSuppressed.isEmpty && lineSuppressed.isEmpty then currentScope
            else currentScope ++ elementSuppressed ++ lineSuppressed
          suppressions.push(activeSuppressed)

          val candidates = lints.getOrElse(kind, Vector.empty)
          val applicable =
            candidates.filterNot(lint => activeSuppressed(JavaLint.Wildcard) || activeSuppressed(lint.id))
          if applicable.nonEmpty then {
            val context = new Context(
              trees,
              elements,
              types,
              compilation,
              path,
              typeNames.headOption.getOrElse(""),
              methods.headOption,
              // The trees that hold the code, the closest first, which is what the lints that ask for the code that
              // holds them read.
              ancestors,
              registry,
              activeSuppressed
            )
            applicable.foreach { lint =>
              lint.check(context, tree).foreach { finding =>
                val findingLine      = Linter.line(trees, compilation, finding)
                val atLineSuppressed = noinspections.getOrElse(findingLine, Set.empty[String])
                val id               = finding.reportedAs(lint)
                if (!atLineSuppressed(JavaLint.Wildcard) && !atLineSuppressed(id) && !activeSuppressed(id)) {
                  found += Linter.found(lint, context, finding)
                }
              }
            }
          }
          ancestors = tree :: ancestors
          paths.push(path)
          try super.scan(tree, unused)
          finally {
            val _ = paths.pop()
            ancestors = ancestors.tail
            val _ = suppressions.pop()
            if isMethod then { val _ = methods.pop() }
            if isType then { val _ = typeNames.pop() }
          }
        } else super.scan(tree, unused)
    }

    scanner.scan(declaration, ())
    found.toVector
  }

  /** The lints of the code of a type, which the compiler attributes one type at a time. */
  private val classLints: Vector[Lint] =
    LintBugs.lints ++
      LintRedundant.lints ++
      LintControlFlow.lints ++
      LintErrors.lints ++
      LintThreading.lints ++
      LintPerformance.lints ++
      LintLoops.lints ++
      LintSerialization.lints ++
      LintOptionals.lints ++
      LintChains.lints ++
      LintFlow.lints ++
      LintScopes.lints ++
      LintReflection.lints ++
      LintInternals.lints

  /** The lints of each kind of tree, so that the walk asks only the lints that read the tree. */
  private val byKind: Map[Tree.Kind, Vector[Lint]] =
    classLints.flatMap(lint => lint.kinds.map(kind => kind -> lint)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  /** The lints of the file as a whole, which the compiler reads once. */
  private val fileLints: Vector[Lint] = LintRedundant.fileLints ++ LintLogging.lints ++ LintInternals.fileLints

  /** The kinds of tree that hold a type, which the finding of a lint is placed on. */
  private val owners: Set[Tree.Kind] =
    Set(Tree.Kind.CLASS, Tree.Kind.INTERFACE, Tree.Kind.ENUM, Tree.Kind.RECORD, Tree.Kind.ANNOTATION_TYPE)

  /** The finding of a lint, which the model holds. */
  def found(lint: Lint, context: Context, finding: Found): Model.Lint =
    Model.Lint(
      finding.reportedAs(lint),
      context.owner,
      context.method.getOrElse(""),
      Collector.source(context.unit),
      Linter.line(context.trees, context.unit, finding),
      finding.detail
    )
}
