package net.kurobako.bonsai.lifting

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Driver
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.reporting.StoreReporter
import java.nio.file.Files
import net.kurobako.bonsai.testing.Scratch

/** A source compiled up to the typed trees the plugin sees, with the context that holds them. */
final class Compiled(val source: String) {
  private var captured: Option[(tpd.Tree, Context)] = None

  private object Capture extends Phase {
    val phaseName = "capture"

    def run(using Context): Unit = captured = Some((ctx.compilationUnit.tpdTree, ctx))
  }

  private object Fixture extends Driver {
    override protected def newCompiler(using Context): Compiler = new Compiler {
      override def phases: List[List[Phase]] = {
        val all = super.phases
        val at  = all.indexWhere(_.exists(_.phaseName == "posttyper"))
        all.patch(at + 1, List(List(Capture)), 0)
      }
    }
  }

  private val reporter = StoreReporter()
  Scratch.using("bonsai-lifting-") { directory =>
    val file = directory.resolve("Fixture.scala")
    Files.writeString(file, source)
    Fixture.process(
      Array(
        "-classpath",
        sys.props("bonsai.lifting.classpath"),
        "-d",
        directory.toString,
        "-color:never",
        file.toString
      ),
      reporter
    )
  }: Unit

  val errors: List[String] = reporter.allErrors.map(_.message).toList

  given Context = captured.map(_._2).getOrElse(throw AssertionError(s"does not compile: $errors"))

  val tree: tpd.Tree = captured.map(_._1).getOrElse(throw AssertionError(s"does not compile: $errors"))

  /** The right-hand side of the value or method named `name`. */
  def definition(name: String): tpd.Tree = tree.filterSubTrees {
    case value: tpd.ValDef  => value.name.toString == name
    case method: tpd.DefDef => method.name.toString == name
    case _                  => false
  }.collectFirst {
    case value: tpd.ValDef  => value.rhs
    case method: tpd.DefDef => method.rhs
  }.getOrElse(throw AssertionError(s"no definition $name"))

  def definitionTree(name: String): tpd.Tree = tree.filterSubTrees {
    case method: tpd.DefDef => method.name.toString == name
    case _                  => false
  }.head

  def lifter(purity: Purity = Purity.Syntactic): Lifter = Lifter(purity)

  def chains(name: String): Vector[Lifted] = lifter().chains(definition(name), definitionOwner(name))

  def lifted(name: String): Lifted = chains(name).headOption.getOrElse(throw AssertionError(s"$name does not lift"))

  private def definitionOwner(name: String) = tree.filterSubTrees {
    case value: tpd.ValDef => value.name.toString == name
    case _                 => false
  }.head.symbol
}
