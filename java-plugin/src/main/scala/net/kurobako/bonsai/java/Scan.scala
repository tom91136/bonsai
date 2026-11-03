package net.kurobako.bonsai.java

import com.sun.source.tree.Tree
import com.sun.source.util.TreeScanner

private object Scan {

  /** Whether a node of the tree, the tree itself included, is one the test accepts, among those `into` admits. */
  def exists(tree: Tree, into: Tree => Boolean = _ => true)(test: PartialFunction[Tree, Boolean]): Boolean = {
    val finder = Finder(into, test)
    finder.scan(tree, ())
    finder.found
  }

  /** What the find yields for each node of the tree it applies to, in visiting order, among those `into` admits. */
  def collect[A](tree: Tree, into: Tree => Boolean)(find: PartialFunction[Tree, A]): Vector[A] = {
    val found = Vector.newBuilder[A]
    foreach(tree, into) { case node => find.runWith(found += _)(node): Unit }
    found.result()
  }

  /** Runs the visit on each node of the tree, the tree itself included, and on those of the nodes `into` admits. */
  def foreach(tree: Tree, into: Tree => Boolean = _ => true)(visit: PartialFunction[Tree, Unit]): Unit =
    Visitor(into, visit).scan(tree, ())

  private class Visitor(into: Tree => Boolean, visit: PartialFunction[Tree, Unit]) extends TreeScanner[Unit, Unit] {
    // The scan reaches every node, so the missing children are tested for null rather than wrapped.
    @SuppressWarnings(Array("scalafix:DisableSyntax.null"))
    override def scan(tree: Tree, unused: Unit): Unit =
      if (tree ne null) {
        visit.applyOrElse(tree, _ => ())
        if (into(tree)) super.scan(tree, unused)
      }
  }

  private class Finder(into: Tree => Boolean, test: PartialFunction[Tree, Boolean]) extends TreeScanner[Unit, Unit] {

    // scalafix:off DisableSyntax.var
    var found = false
    // scalafix:on DisableSyntax.var

    // The scan reaches every node, so the missing children are tested for null rather than wrapped.
    @SuppressWarnings(Array("scalafix:DisableSyntax.null"))
    override def scan(tree: Tree, unused: Unit): Unit =
      if (!found && (tree ne null)) {
        if (test.applyOrElse(tree, _ => false)) found = true
        else if (into(tree)) super.scan(tree, unused)
      }
  }
}
