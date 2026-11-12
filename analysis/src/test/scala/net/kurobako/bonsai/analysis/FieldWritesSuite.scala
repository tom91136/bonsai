package net.kurobako.bonsai.analysis

import java.nio.file.Files
import net.kurobako.bonsai.testing.*

/** Java methods that write a field of an argument through an updater, a handle or an atomic. */
class FieldWritesSuite extends AnalysisSuite {
  private val java = Seq(
    "scala/collection/concurrent/Linked.java" -> """package scala.collection.concurrent;

import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

public abstract class Linked {
  public static final AtomicReferenceFieldUpdater<Linked, Linked> LINK =
      AtomicReferenceFieldUpdater.newUpdater(Linked.class, Linked.class, "next");
  public static final AtomicIntegerFieldUpdater<Linked> SIZE =
      AtomicIntegerFieldUpdater.newUpdater(Linked.class, "size");

  public volatile Linked next = null;
  public volatile int size = 0;

  public boolean casNext(Linked expected, Linked updated) { return LINK.compareAndSet(this, expected, updated); }
  public boolean casSize(int expected, int updated) { return SIZE.compareAndSet(this, expected, updated); }
}
""",
    "scala/collection/concurrent/Handled.java" -> """package scala.collection.concurrent;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;

public class Handled {
  public volatile int count = 0;

  private static final VarHandle COUNT;

  static {
    try {
      COUNT = MethodHandles.lookup().findVarHandle(Handled.class, "count", int.class);
    } catch (ReflectiveOperationException e) {
      throw new ExceptionInInitializerError(e);
    }
  }

  public boolean casCount(int expected, int updated) { return COUNT.compareAndSet(this, expected, updated); }
}
""",
    "scala/collection/concurrent/Cells.java" ->
      """package scala.collection.concurrent;

import java.util.concurrent.atomic.AtomicInteger;

public class Cells {
  public static boolean casOf(AtomicInteger cell, int expected, int updated) { return cell.compareAndSet(expected, updated); }
  public static int viaField(Cells cells) { return cells.cell.incrementAndGet(); }

  public final AtomicInteger cell = new AtomicInteger();
}
"""
  )

  private val callers = """package fixture

import scala.collection.concurrent.Cells
import scala.collection.concurrent.Handled
import scala.collection.concurrent.Linked

object Callers {
  def link(node: Linked, expected: Linked): Unit = {
    node.casNext(expected, null)
    ()
  }
  def size(node: Linked): Unit = {
    node.casSize(0, 1)
    ()
  }
  def count(handled: Handled): Unit = {
    handled.casCount(0, 1)
    ()
  }
  def cell(cells: Cells): Unit = {
    Cells.viaField(cells)
    ()
  }
  def main(args: Array[String]): Unit = ()
}
"""

  private lazy val result: Extraction.Result =
    Scratch.using("bonsai-field-writes-") { directory =>
      val classes = Files.createDirectories(directory.resolve("scala/classes"))
      write(directory.resolve("java"), java)
      javac(java.map((name, _) => directory.resolve("java").resolve(name)), classes, classpath): Unit
      compile(directory.resolve("scala"), Seq("Callers.scala" -> callers), Vector(classes)): Unit
      val manifest = Manifest(
        Vector(Output("m", Output.Scope.Main, classes)),
        scoped(Map("m" -> classpath)),
        Set.empty,
        sources = Vector(Output("m", Output.Scope.Main, directory.resolve("scala/Callers.scala"))),
        jdk = targetJdk
      )
      Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
    }

  private lazy val discarded: Vector[String] =
    Purity.findings(result.purity, result.graph.nodes).filter(_.rule == Finding.Rule.PureDiscard).map(_.detail)

  private def dropsPure(callee: String): Boolean = discarded.exists(_.startsWith(s"$callee has no effects"))

  test("a Java method that compare-and-sets a reference field updater is not pure to drop") {
    assert(!dropsPure("Linked.casNext"), discarded.mkString("\n"))
  }

  test("a Java method that compare-and-sets an integer field updater is not pure to drop") {
    assert(!dropsPure("Linked.casSize"), discarded.mkString("\n"))
  }

  test("a Java method that compare-and-sets through a variable handle is not pure to drop") {
    assert(!dropsPure("Handled.casCount"), discarded.mkString("\n"))
  }

  test("a Java method that increments an atomic held in a field is not pure to drop") {
    assert(!dropsPure("Cells.viaField"), discarded.mkString("\n"))
  }
}
