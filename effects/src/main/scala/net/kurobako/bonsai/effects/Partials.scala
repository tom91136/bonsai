package net.kurobako.bonsai.effects

/** What fails for some inputs in the standard library and the libraries code commonly calls, and what to call instead.
  */
object Partials {

  /** A call that throws where a total alternative returns the failure; `optional` when the alternative, called the same
    * way, returns an Option.
    */
  case class Partial(alternative: String, optional: Boolean)

  val Alternatives: Map[String, Partial] = Map(
    "scala.collection.MapOps.apply"           -> Partial("get", optional = true),
    "scala.collection.SeqOps.apply"           -> Partial("lift", optional = true),
    "scala.collection.IterableOps.head"       -> Partial("headOption", optional = true),
    "scala.collection.IterableOps.last"       -> Partial("lastOption", optional = true),
    "scala.collection.IterableOnceOps.reduce" -> Partial("reduceOption", optional = true),
    "scala.collection.IterableOnceOps.max"    -> Partial("maxOption", optional = true),
    "scala.collection.IterableOnceOps.min"    -> Partial("minOption", optional = true),
    "scala.Option.get"                        -> Partial("getOrElse or fold", optional = false),
    "scala.collection.StringOps.toInt"        -> Partial("toIntOption", optional = true),
    "scala.collection.StringOps.toLong"       -> Partial("toLongOption", optional = true),
    "scala.collection.StringOps.toDouble"     -> Partial("toDoubleOption", optional = true),
    "scala.collection.StringOps.toFloat"      -> Partial("toFloatOption", optional = true),
    "scala.collection.StringOps.toShort"      -> Partial("toShortOption", optional = true),
    "scala.collection.StringOps.toByte"       -> Partial("toByteOption", optional = true),
    "scala.collection.StringOps.toBoolean"    -> Partial("toBooleanOption", optional = true),
    "java.lang.Integer.parseInt"              -> Partial("toIntOption", optional = false),
    "java.lang.Long.parseLong"                -> Partial("toLongOption", optional = false),
    "ujson.Value.obj"                         -> Partial("objOpt", optional = true),
    "ujson.Value.str"                         -> Partial("strOpt", optional = true),
    "ujson.Value.num"                         -> Partial("numOpt", optional = true),
    "ujson.Value.arr"                         -> Partial("arrOpt", optional = true),
    "ujson.Value.bool"                        -> Partial("boolOpt", optional = true),
    "ujson.Value.apply"                       -> Partial("objOpt and arrOpt lookups", optional = false)
  )

  /** Methods of the standard library that fail for some inputs and have no total alternative to point at. */
  private val Guards =
    Set("init", "tail", "reduceLeft", "reduceRight", "maxBy", "minBy", "ensuring", "/", "%", "<init>")

  /** Methods that validate their arguments or state by throwing. */
  private val Checks: Set[String] = Set("require", "assert", "assume")

  /** Names of the methods of the standard library that fail for some inputs, wherever they are declared. */
  val Names: Set[String] =
    Alternatives.keys.filter(key => key.startsWith("scala.") || key.startsWith("java.")).map(_.split('.').last).toSet ++
      Guards ++ Checks
}
