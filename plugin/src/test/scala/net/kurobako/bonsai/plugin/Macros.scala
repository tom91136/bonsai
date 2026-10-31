package net.kurobako.bonsai.plugin

import scala.quoted.*

/** A macro for the sources the suites compile, which reads its argument at compile time without running it. */
object Macros {
  inline def checked(inline value: String): Unit = ${ check('value) }

  def check(value: Expr[String])(using Quotes): Expr[Unit] = {
    if value.value.contains("") then quotes.reflect.report.error("the text is empty", value)
    '{ () }
  }
}
