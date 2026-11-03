package net.kurobako.bonsai.java

/** A reference from one type to another, which the graph of the build holds.
  *
  * @param from
  *   the type that makes the reference
  * @param to
  *   the type the reference names
  */
case class Edge(from: String, to: String)
