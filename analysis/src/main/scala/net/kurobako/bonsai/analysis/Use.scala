package net.kurobako.bonsai.analysis

/** A use of a member by `node`, from inside the method `method` when the code is a method the program runs as such: it
  * is the key `Keys` gives that method. A use in a class body or in inline code has none, and is as live as its node.
  */
case class Use(node: String, method: Option[String] = None)
