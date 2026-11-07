package net.kurobako.bonsai.java

/** A reference from one type to another, which the graph of the build holds.
  *
  * @param from
  *   the type that makes the reference
  * @param to
  *   the type the reference names
  * @param method
  *   the key of the method the reference is inside, or empty outside any method
  */
case class Edge(from: String, to: String, method: String = "")
