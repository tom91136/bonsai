package net.kurobako.bonsai.analysis

/** A callee of the build: the key its sites are recorded under, its number of parameters, and the name, node and
  * suppressions its findings carry.
  */
private[analysis] case class Callee(key: String, arity: Int, name: String, node: String, suppressed: Set[String])
