// format: off
package fix

object DanglingDocComment {
  
  /** documents Stacked */
  val Stacked = 1

  
  
  /** documents Triple */
  val Triple = 2

  

  /** documents Spaced */
  val Spaced = 3

  
  // a note
  /** documents Noted */
  val Noted = 4

  val Inline = 5
  /** documents Tail */
  val Tail = 6

  class Holder {
    val member = 7

    
  }

  trait Bare {
    
  }

  /** documents Plain */
  val Plain = 8

  /** documents Annotated */
  @deprecated("x", "1")
  val Annotated = 9

  /** documents Modifiers */
  private final val Modifiers = 10

  /** documents Noted across a line comment */
  // a note
  val Commented = 11

  /** documents Blocked across a block comment */
  /* a note */
  val Blocked = 12

  /** documents Trailing */ // a trailing note
  val Trailing = 13

  /* a plain block comment before a doc comment */
  /** documents Mixed */
  val Mixed = 14

  /**/
  val Empty = 15

  class Notes {
    val member = 16
    /* a plain block comment before the brace */
  }

  class Lines {
    val member = 17
    // a line comment before the brace
  }

  class Hollow {
    val member = 18
    /**/
  }

  def matching(value: Int): Int = value match {
    /** documents the case */
    case 1 => 1
    case _ => 0
  }

  val Text = "/** in a string */"
}

/** documents Companion */
class Companion


