package net.kurobako.bonsai.collections

/** What the receiver or the result of a step is, as far as the laws care: `library` for a standard collection rather
  * than a user type with the same method names, `strict` unless it is a view, an iterator or a lazy list, `ordered`
  * when it keeps an order, `distinct` for a set, `keyed` for a map, `single` for an option and `sorted` when a
  * comparator rather than equality decides which elements are the same. The `name` is the standard type the value has
  * when the front end knows it exactly, written below `scala.collection` such as `immutable.List`, `Seq` or `Array`,
  * and empty otherwise.
  */
case class Kind(
    library: Boolean,
    strict: Boolean = true,
    ordered: Boolean = true,
    distinct: Boolean = false,
    keyed: Boolean = false,
    single: Boolean = false,
    name: String = "",
    sorted: Boolean = false
)
