package net.kurobako.bonsai.collections

/** The methods the laws know, as data, keyed by name. Besides the standard methods, the keys include the pseudo methods
  * that `Chain` describes the lowering to, and the Java front end's stream and factory methods. Some keys stand for
  * what a rewrite leaves, with nothing to run.
  */
object Ops {
  private val one        = Cost(passes = 1, allocations = 1)
  private val search     = Cost(passes = 1, early = 1)
  private val constant   = Cost()
  private val allocating = Cost(allocations = 1)
  private val pairing    = Cost(passes = 1, allocations = 2)

  private def transformer(shape: Shape, cost: Cost, lazily: Cost = Cost()): Op = Op(shape, cost, lazily)

  private def forced(cost: Cost, lazily: Cost): Op = Op(Shape.Terminal, cost, lazily)

  private val forcing = Cost(passes = 1)
  private val first   = Cost(passes = 1, early = 1)
  private val sorting = Cost(passes = 1, sorts = 1)

  private val allocatingSearch = forced(search.copy(allocations = 1), search.copy(allocations = 1))

  private def lookup(op: Op): Op = op.copy(hashed = Some(constant))

  private val ops: Map[String, Op] = Map(
    "map"              -> transformer(Shape.Map, one),
    "filter"           -> transformer(Shape.Filter, one),
    "filterNot"        -> transformer(Shape.Filter, one),
    "takeWhile"        -> transformer(Shape.Filter, one),
    "dropWhile"        -> transformer(Shape.Filter, one),
    "flatMap"          -> transformer(Shape.Flat, one),
    "collect"          -> transformer(Shape.Flat, one),
    "flatten"          -> transformer(Shape.Other, one),
    "reverse"          -> transformer(Shape.Other, one),
    "take"             -> transformer(Shape.Other, one),
    "drop"             -> transformer(Shape.Other, one),
    "slice"            -> transformer(Shape.Other, one),
    "intersect"        -> transformer(Shape.Other, one),
    "self"             -> transformer(Shape.Other, constant),
    "empty"            -> transformer(Shape.Other, constant),
    "takeRight"        -> transformer(Shape.Other, one),
    "distinct"         -> transformer(Shape.Other, one),
    "zip"              -> transformer(Shape.Other, one),
    "zipWithIndex"     -> transformer(Shape.Other, one),
    "toSet"            -> transformer(Shape.Other, one),
    "toSeq"            -> transformer(Shape.Other, one),
    "toList"           -> transformer(Shape.Other, one),
    "toVector"         -> transformer(Shape.Other, one),
    "toIndexedSeq"     -> transformer(Shape.Other, one),
    "toMap"            -> transformer(Shape.Other, one),
    "unzip"            -> transformer(Shape.Other, pairing),
    "unzip3"           -> transformer(Shape.Other, Cost(passes = 1, allocations = 3)),
    "sorted"           -> transformer(Shape.Other, one.copy(sorts = 1), sorting),
    "sortBy"           -> transformer(Shape.Other, one.copy(sorts = 1), sorting),
    "sortWith"         -> transformer(Shape.Other, one.copy(sorts = 1), sorting),
    "fused"            -> transformer(Shape.Other, one),
    "keys"             -> transformer(Shape.Other, allocating),
    "values"           -> transformer(Shape.Other, allocating),
    "indices"          -> transformer(Shape.Other, allocating),
    "untilSize"        -> transformer(Shape.Other, allocating),
    "toLastIndex"      -> transformer(Shape.Other, allocating),
    "stream"           -> transformer(Shape.Other, constant),
    "streamOf"         -> transformer(Shape.Other, constant),
    "of"               -> transformer(Shape.Other, allocating),
    "asList"           -> transformer(Shape.Other, allocating),
    "unmodifiableList" -> transformer(Shape.Other, pairing),
    "unmodifiableSet"  -> transformer(Shape.Other, pairing),
    "unmodifiableMap"  -> transformer(Shape.Other, pairing),
    "copyOf"           -> transformer(Shape.Other, one),
    "size"             -> forced(constant, forcing),
    "length"           -> forced(constant, forcing),
    "isEmpty"          -> forced(constant, first),
    "nonEmpty"         -> forced(constant, first),
    "isDefined"        -> forced(constant, first),
    "sizeIsZero"       -> forced(constant, forcing),
    "sizeNonZero"      -> forced(constant, forcing),
    "isNone"           -> forced(constant, first),
    "isSome"           -> forced(constant, first),
    "equalsSome"       -> forced(constant, constant),
    "notEqualsSome"    -> forced(constant, constant),
    "get"              -> forced(allocating, allocating),
    "getOrElse"        -> forced(constant, constant),
    "apply"            -> forced(constant, constant),
    "applyLast"        -> forced(constant, constant),
    "lift"             -> forced(allocating, allocating),
    "liftLast"         -> forced(allocating, allocating),
    "fold"             -> forced(forcing, forcing),
    "head"             -> forced(constant, first),
    "last"             -> forced(constant, forcing),
    "headOption"       -> forced(allocating, one.copy(early = 1)),
    "lastOption"       -> forced(allocating, one),
    "_1"               -> forced(constant, constant),
    "_2"               -> forced(constant, constant),
    "_3"               -> forced(constant, constant),
    "count"            -> forced(forcing, forcing),
    "countZero"        -> forced(forcing, forcing),
    "countNonZero"     -> forced(forcing, forcing),
    "foldTrueAnd"      -> forced(forcing, forcing),
    "join"             -> forced(forcing, forcing),
    "joinReduce"       -> forced(forcing, forcing),
    "joinReduceStrict" -> forced(forcing, forcing),
    "stringJoin"       -> forced(forcing, forcing),
    "noneMatch"        -> forced(search, search),
    "min"              -> forced(forcing, forcing),
    "max"              -> forced(forcing, forcing),
    "minBy"            -> forced(forcing, forcing),
    "maxBy"            -> forced(forcing, forcing),
    "exists"           -> forced(search, search),
    "forall"           -> forced(search, search),
    "contains"         -> lookup(forced(search, search)),
    "find"             -> allocatingSearch,
    "findLast"         -> allocatingSearch,
    "collectFirst"     -> allocatingSearch
  )

  def apply(method: String): Option[Op] = ops.get(method)

  /** The methods that apply a function to each element and give a collection. */
  val elementWise: Set[String] = ops.collect { case (method, op) if op.elementWise => method }.toSet

  /** What `step` costs. A method the table does not know costs a pass and a collection. */
  def cost(step: Step): Cost = apply(step.method) match {
    case _ if step.receiver.single => constant
    case Some(op)                  =>
      val receiver = step.receiver
      if (op.shape == Shape.Terminal)
        op.hashed.filter(_ => receiver.keyed || receiver.distinct)
          .getOrElse(if (receiver.strict) op.cost else op.lazily)
      else if (step.result.strict) op.cost
      else op.lazily
    case None => one
  }

  def cost(steps: Seq[Step]): Cost = steps.foldLeft(Cost())((total, step) => total + cost(step))
}
