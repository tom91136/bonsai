package net.kurobako.bonsai.collections

/** How a law reads in Java: what it says, the rewrite where Java has one, and when it holds there. */
private[collections] case class JavaForm(
    message: Hit => String,
    rewrite: Option[(Hit, String) => String] = None,
    applies: Hit => Boolean = _ => true
)
