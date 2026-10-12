package fix

object ContextBoundSyntax {
  trait Show[A]
  trait Read[A]
  trait Json

  def chained[O <: Json: {Show, Read}](value: O): O                            = value
  def pair[I, O: {Show, Read}](input: I, value: O): O                          = value
  def grouped[O <: Json: {Show, Read}](value: O): O                          = value
  def single[O: Show](value: O): O                                           = value
  class Holder[A: {Show, Read}](val value: A)
}
