package net.kurobako.bonsai.effects

/** A method's effects and the indices of the function and by-name parameters it runs. */
case class Summary[+T](effects: Iterable[Effect[T]], invokes: Set[Int])
