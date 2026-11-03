package net.kurobako.bonsai.java

import com.sun.source.tree.Tree

private object Identities {

  // scalafix:off NoBoxedPrimitives.boxed-type
  /** A set that holds the trees of a compilation by identity. */
  def trees: java.util.Set[Tree] =
    java.util.Collections.newSetFromMap(java.util.IdentityHashMap[Tree, java.lang.Boolean]())
  // scalafix:on NoBoxedPrimitives.boxed-type
}
