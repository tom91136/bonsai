package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.java.{Model => JavaModel}
import scala.collection.mutable
import scala.util.Try
import scala.util.matching.Regex

/** The modules of a JPMS build: what each one requires, and which module holds a class the code names only in a text.
  *
  * The compiler checks the requirements of the code it reads, but a class named by `Class.forName` is a text, so the
  * module system only checks the requirement when the code runs. The rule reads the descriptors the build writes and
  * reports the lookups whose module the code does not require.
  */
object Modules {

  /** A module descriptor of the build, with the modules it requires and whether each is re-exported transitively. */
  case class Descriptor(name: String, requires: Vector[(String, Boolean)])

  /** A reflective lookup of a class of another module, which the caller does not require. */
  case class Use(caller: String, source: String, line: Int, callerModule: String, targetModule: String)

  private val Name: Regex     = raw"(?s)(?:open\s+)?module\s+([\w.]+)\s*\{".r
  private val Requires: Regex = raw"(?s)requires\s+([^;]+);".r
  private val Whitespace      = java.util.regex.Pattern.compile("\\s+")
  private val BlockComment    = java.util.regex.Pattern.compile("(?s)/\\*.*?\\*/")
  private val LineComment     = java.util.regex.Pattern.compile("(?m)//[^\n]*")

  /** The lookups of classes of modules the caller does not require, which the build can read. */
  def uses(
      manifest: Manifest,
      nodes: Map[String, Node],
      reflections: Vector[JavaModel.Reflection]
  ): Vector[Use] = {
    val descriptors = manifest.sources.flatMap { source =>
      if source.path.getFileName.toString == "module-info.java" then
        descriptor(source.path).map(source.module -> _)
      else None
    }.toMap
    if descriptors.isEmpty then Vector.empty
    else {
      val byName  = descriptors.map((_, descriptor) => descriptor.name -> descriptor)
      val outputs = manifest.outputs.sortBy(_.scope == Output.Scope.Test).map(output => output.module -> output.path)
      val holders = mutable.Map.empty[String, Option[String]]
      def holder(name: String): Option[String] = holders.getOrElseUpdate(
        name, {
          val relative = name.replace('.', '/') + ".class"
          outputs.collectFirst { case (module, path) if Files.exists(path.resolve(relative)) => module }
        }
      )
      reflections.flatMap { reflection =>
        for {
          caller       <- nodes.get(reflection.caller)
          from         <- descriptors.get(caller.module)
          targetModule <- holder(reflection.target)
          if targetModule != caller.module
          to <- descriptors.get(targetModule)
          if from.name != to.name
          if !reads(from.name, to.name, byName)
          (source, line) <- located(reflection.location)
        } yield Use(reflection.caller, source, line, from.name, to.name)
      }.distinctBy(use => (use.caller, use.targetModule))
    }
  }

  /** The descriptor of a module source, read from the text of the file. */
  def descriptor(file: Path): Option[Descriptor] =
    Try(Files.readString(file)).toOption.flatMap { text =>
      val source = LineComment
        .matcher(BlockComment.matcher(text).replaceAll(" "))
        .replaceAll(" ")
      Name.findFirstMatchIn(source).map { matched =>
        val requires = Requires.findAllMatchIn(source).toVector.map { clause =>
          val words = Whitespace.split(clause.group(1).trim).toVector
          words.last -> words.dropRight(1).contains("transitive")
        }
        Descriptor(matched.group(1), requires)
      }
    }

  /** Whether the module reads the other one, directly or through a module it reads that re-exports it. */
  private def reads(from: String, target: String, modules: Map[String, Descriptor]): Boolean = {
    def visit(module: String, root: Boolean, seen: Set[String]): Boolean =
      if seen(module) then false
      else
        modules.get(module).exists(_.requires.exists { (dependency, transitive) =>
          dependency == target || ((root || transitive) && visit(dependency, root = false, seen + module))
        })
    visit(from, root = true, Set.empty)
  }

  /** The file and line a location names, which the model writes as `path:line`. */
  private def located(location: String): Option[(String, Int)] = {
    val split = location.lastIndexOf(':')
    if split < 0 then None
    else location.substring(split + 1).toIntOption.map(line => location.substring(0, split) -> line)
  }
}
