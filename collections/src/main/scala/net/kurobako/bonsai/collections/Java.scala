package net.kurobako.bonsai.collections

/** What a law says of its steps in Java: the `message`, and the `render` of the expression that replaces them from the
  * text of their receiver, when the law has an exact rewrite in Java.
  */
case class Java(message: String, render: Option[String => String])
