package net.kurobako.bonsai.java

/** The names of the Java methods the lifter reads as steps of the algebra, by the receiver they are called on and the
  * number of their arguments. The steps they lower to are the methods the algebra knows, `Ops` giving their costs.
  */
object ChainMethods {

  val Streams: Map[(String, Int), String] = Map(
    ("filter", 1)      -> "filter",
    ("map", 1)         -> "map",
    ("mapToInt", 1)    -> "map",
    ("mapToLong", 1)   -> "map",
    ("mapToDouble", 1) -> "map",
    ("mapToObj", 1)    -> "map",
    ("flatMap", 1)     -> "flatMap",
    ("sorted", 0)      -> "sorted",
    ("sorted", 1)      -> "sortWith",
    ("distinct", 0)    -> "distinct",
    ("limit", 1)       -> "take",
    ("skip", 1)        -> "drop",
    ("takeWhile", 1)   -> "takeWhile",
    ("dropWhile", 1)   -> "dropWhile",
    ("findFirst", 0)   -> "headOption",
    ("findAny", 0)     -> "headOption",
    ("anyMatch", 1)    -> "exists",
    ("allMatch", 1)    -> "forall",
    ("noneMatch", 1)   -> "noneMatch",
    ("count", 0)       -> "size",
    ("min", 0)         -> "min",
    ("min", 1)         -> "min",
    ("max", 0)         -> "max",
    ("max", 1)         -> "max",
    ("toList", 0)      -> "toList"
  )

  val Collections: Map[(String, Int), String] = Map(
    ("size", 0)     -> "size",
    ("isEmpty", 0)  -> "isEmpty",
    ("contains", 1) -> "contains",
    ("stream", 0)   -> "stream"
  )

  val Maps: Map[(String, Int), String] = Map(("size", 0) -> "size", ("isEmpty", 0) -> "isEmpty")

  val Optionals: Map[(String, Int), String] = Map(
    ("isPresent", 0)   -> "isDefined",
    ("isEmpty", 0)     -> "isEmpty",
    ("get", 0)         -> "get",
    ("getAsInt", 0)    -> "get",
    ("getAsLong", 0)   -> "get",
    ("getAsDouble", 0) -> "get",
    ("map", 1)         -> "map",
    ("filter", 1)      -> "filter",
    ("flatMap", 1)     -> "flatMap",
    ("orElse", 1)      -> "getOrElse",
    ("orElseGet", 1)   -> "getOrElse"
  )

  /** The static factories whose result a chain starts from, by the class that has them. */
  val Factories: Map[String, Set[String]] = Map("java.util.List" -> Set("of"), "java.util.Arrays" -> Set("asList"))

  /** The unmodifiable views of a copy, by the method of `Collections`, with the type of the collection each copies. */
  val Unmodifiable: Map[String, String] = Map(
    "unmodifiableList" -> "java.util.Collection",
    "unmodifiableSet"  -> "java.util.Collection",
    "unmodifiableMap"  -> "java.util.Map"
  )

  private[java] val Names
      : Set[String] = (Streams.keySet ++ Collections.keySet ++ Maps.keySet ++ Optionals.keySet).map(_._1) ++
    Factories.values.flatten ++
    Unmodifiable.keySet ++ Set("collect", "reduce", "orElseThrow")
}
