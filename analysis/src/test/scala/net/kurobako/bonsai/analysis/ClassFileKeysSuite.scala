package net.kurobako.bonsai.analysis

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Driver
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.analysis.ClassFiles.*
import net.kurobako.bonsai.testing.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using

class ClassFileKeysSuite extends AnalysisSuite {
  private val source = """package fixture

class Top {
  def inst(x: Int, names: Array[String]): Unit = ()
  def gen[T <: Comparable[T]](x: T): T = x
  def prims(flag: Boolean, c: Char, l: Long, d: Double): Short = 0
}

object Obj {
  def stat(l: Long): Double = l.toDouble
  def over(x: Int): Unit = ()
  def over(s: String): Unit = ()
  def dflt(x: Int = 1): Int = x
  def +(x: Int): Int = x
  def unary_! : Boolean = false
}

class Companion {
  def m(x: Int): Int = x
}

object Companion {
  def m(x: Int): Int = x
}

trait Tr {
  def concrete(x: Int): Int = x
  def abstractOne(s: String): Unit
}

class Outer {
  class Inner(v: Int) {
    def go(): Int = v
  }
}

object Holder {
  class Nested {
    def go(): Unit = ()
  }
}
"""

  private def capture(directory: Path, source: String): (Vector[tpd.DefDef], Context) = {
    val captured = mutable.ArrayBuffer.empty[(tpd.Tree, Context)]
    object Capture extends Phase {
      val phaseName = "capture"

      def run(using Context): Unit = captured += ((ctx.compilationUnit.tpdTree, ctx)): Unit
    }
    class Capturing extends Compiler {
      override def phases: List[List[Phase]] = {
        val all = super.phases
        val at  = all.indexWhere(_.exists(_.phaseName == "posttyper"))
        all.patch(at + 1, List(List(Capture)), 0)
      }
    }
    class Fixture extends Driver {
      override protected def newCompiler(using Context): Compiler = Capturing()
    }
    val file   = directory.resolve("Fixture.scala")
    val output = Files.createDirectories(directory.resolve("classes"))
    Files.writeString(file, source): Unit
    val reporter = StoreReporter()
    Fixture().process(
      Array(
        "-classpath",
        classpath.mkString(File.pathSeparator),
        "-d",
        output.toString,
        "-color:never",
        file.toString
      ),
      reporter
    ): Unit
    assert(!reporter.hasErrors, reporter.allErrors.map(_.message).mkString("\n"))
    val (tree, context) = captured.head
    given Context       = context
    val methods         = tree.filterSubTrees {
      case method: tpd.DefDef => method.symbol.owner.isClass
      case _                  => false
    }.collect { case method: tpd.DefDef => method }
    (methods.toVector, context)
  }

  private def classFile(path: Path): ClassFile =
    ClassFiles.read(Files.readAllBytes(path)).fold(error => fail(error), identity)

  test("keys of the methods are the keys of the class file methods") {
    Scratch.using("bonsai-class-keys-") { directory =>
      val (methods, context) = capture(directory, source)
      given Context          = context
      val expected           = methods.flatMap(method => Keys.of(method.symbol))
      val actual             = Using.resource(Files.walk(directory.resolve("classes")))(_.toList.asScala.toVector)
        .filter(_.toString.endsWith(".class"))
        .flatMap { path =>
          val file = classFile(path)
          file.methods.map(method => ClassFiles.key(Reference(file.name, method.name, method.descriptor)))
        }
        .toSet
      assert(expected.nonEmpty)
      assertEquals(expected.filterNot(actual.contains).sorted, Vector.empty[String])
    }
  }
}
