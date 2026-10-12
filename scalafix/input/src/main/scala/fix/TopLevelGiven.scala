/*
rule = FilenameConventions
 */
package fix

given TopLevelGiven: Ordering[Int] with {
  def compare(left: Int, right: Int): Int = left - right
}

given spareGiven: Ordering[Long] with { // assert: FilenameConventions.top-level-filename
  def compare(left: Long, right: Long): Int = java.lang.Long.compare(left, right)
}
