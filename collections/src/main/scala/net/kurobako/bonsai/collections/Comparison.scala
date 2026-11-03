package net.kurobako.bonsai.collections

/** A function that tests its parameter for equality with a value (`equal`) or for inequality, where `constant` is the
  * source of a value that is the same on every call, cannot throw and does not mention the parameter, and `assignable`
  * when its type conforms to the parameter's, so a method that takes the element accepts it.
  */
case class Comparison(equal: Boolean, constant: String, assignable: Boolean = true)
