package net.kurobako.bonsai.collections

/** Names of the standard collection methods that the tools reading code by name agree on. */
object Methods {

  /** The methods that walk a linear sequence to reach an element, its last element or its length. */
  val Walks: Set[String] = Set("apply", "length", "size", "last")

  /** The methods that copy a collection to put elements at its end. */
  val Appends: Set[String] = Set(":+", "appended", "++", "concat", "appendedAll", ":++")

  /** The methods that convert a collection to another standard collection. */
  val Conversions: Set[String] = Catalogue.identities.keySet

  /** The methods that look through a collection for an element. */
  val Searches: Set[String] = Set("contains", "indexOf", "lastIndexOf", "containsSlice", "indexOfSlice", "sameElements")

  /** The methods that sort a collection. */
  val Sorts: Set[String] = Set("sorted", "sortBy", "sortWith", "sort")

  /** The methods that copy a collection into another. */
  val Copies: Set[String] = Conversions ++ Set("toArray", "toBuffer", "distinct", "reverse", "flatten")

  /** The methods that keep their arguments in the collection they are called on. */
  val Mutators: Set[String] = Set(
    "addAll",
    "+=",
    "++=",
    "append",
    "appendAll",
    "prepend",
    "prependAll",
    "insert",
    "insertAll",
    "add",
    "put",
    "putAll",
    "update",
    "push",
    "enqueue",
    "offer",
    "addOne",
    "+=:",
    "++=:",
    "updateWith",
    "getOrElseUpdate"
  )

  /** The methods of a traversal whose functions return what the traversal collects, one per turn. */
  val Accumulating: Set[String] = Set(
    "map",
    "mapToObj",
    "mapMulti",
    "flatMap",
    "collect",
    "foldLeft",
    "foldRight",
    "fold",
    "scanLeft",
    "scanRight",
    "reduce",
    "reduceLeft",
    "reduceRight",
    "aggregate",
    "groupMap",
    "groupMapReduce"
  )
}
