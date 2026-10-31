package net.kurobako.bonsai.scalafix

import scalafix.v1.*

private object mutables {
  private val mutableJava =
    Set("java/util/ArrayList#", "java/util/HashMap#", "java/util/HashSet#", "java/util/LinkedList#")

  def contains(symbol: Symbol): Boolean =
    symbol.value.startsWith("scala/collection/mutable/") || symbol.value == "scala/Array#" ||
      mutableJava(symbol.value) || MutableLibraries(symbol.value)

  // ujson's values hold mutable maps and buffers.
  private val MutableLibraries = Set("ujson/Value#", "ujson/Obj#", "ujson/Arr#")
}
